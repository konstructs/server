package konstructs

import java.util.UUID
import java.awt.image.BufferedImage
import java.awt.Graphics
import java.io.ByteArrayOutputStream
import javax.imageio.ImageIO
import konstructs.plugin.Plugin._

import scala.collection.JavaConverters._

import com.typesafe.config.{ Config => TypesafeConfig }
import akka.actor.{ Actor, Props, ActorRef, Stash }

import com.google.gson.reflect.TypeToken

import konstructs.api._
import konstructs.api.messages._
import konstructs.plugin.{ListConfig, PluginConstructor, Config}

case class BlockFactoryImpl(blockTypeIdMapping: java.util.Map[BlockTypeId, Integer],
  wMapping: java.util.Map[Integer, BlockTypeId], blockTypes: java.util.Map[BlockTypeId, BlockType])
    extends BlockFactory {

  def createBlock(uuid: UUID, w: Int): Block = {
    val t = wMapping.get(w)
    new Block(uuid, t)
  }

  def createBlock(w: Int): Block = {
    val t = wMapping.get(w)
    new Block(null, t)
  }

  def getW(block: Block) =
    blockTypeIdMapping.get(block.getType)

  def getW(stack: Stack) =
    blockTypeIdMapping.get(stack.getTypeId)

  def getW(typeId: BlockTypeId) =
    blockTypeIdMapping.get(typeId)

  def getBlockType(id: BlockTypeId): BlockType = {
    blockTypes.get(id)
  }

  def getBlockTypeId(w: Int) =
    wMapping.get(w)

  def getBlockTypes() =
    blockTypes

  def getWMapping() =
    wMapping

}

object BlockFactoryImpl {

    private def findFreeW(wMapping: java.util.Map[Integer, BlockTypeId]): Integer = {
    for(w <- 0 until 256) {
      if(!wMapping.containsKey(w)) {
        return w
      }
    }
    throw new IllegalStateException("No free w to allocate for new block type")
  }

  private def addBlockType(blockTypeIdMapping: java.util.Map[BlockTypeId, Integer],
    wMapping: java.util.Map[Integer, BlockTypeId],
    blockTypeMapping: java.util.Map[BlockTypeId, BlockType]): PartialFunction[(BlockTypeId, BlockType), Unit] = {
    case (t, bt) =>
      val foundW = blockTypeIdMapping.get(t)
      val w = if(foundW != null) {
        foundW
      } else {
        findFreeW(wMapping)
      }
      blockTypeIdMapping.put(t, w)
      wMapping.put(w, t)
      blockTypeMapping.put(t, bt)
  }

  def apply(defined: java.util.Map[Integer, BlockTypeId], configured: Seq[(BlockTypeId, BlockType)]): BlockFactoryImpl = {
    val wMapping = new java.util.HashMap[Integer, BlockTypeId]()
    val reverse = new java.util.HashMap[BlockTypeId, Integer]()
    val tMapping = new java.util.HashMap[BlockTypeId, BlockType]()

    for((w, tId) <- defined.asScala) {
      wMapping.put(w, tId)
      reverse.put(tId, w)
    }
    configured.map(addBlockType(reverse, wMapping, tMapping))
    apply(reverse, wMapping, tMapping)
  }
}


class BlockMetaActor( val ns: String,
                      val jsonStorage: ActorRef,
                      configuredBlocks: Seq[(BlockTypeId, BlockType)],
                      textures: Array[Byte]
                    ) extends Actor with Stash with utils.Scheduled with JsonStorage {

  import BlockMetaActor._

  val BlockIdFile = "block-id-mapping"

  val typeOfwMapping = new TypeToken[java.util.Map[Integer, BlockTypeId]](){}.getType

  def storeDb(factory: BlockFactory) {
    storeGson(BlockIdFile, gson.toJsonTree(factory.getWMapping, typeOfwMapping))
  }

  loadGson(BlockIdFile)
  def receive() = {
    case GsonLoaded(_, json) if json != null =>
      val defined: java.util.Map[Integer, BlockTypeId] = gson.fromJson(json, typeOfwMapping)
      val factory = BlockFactoryImpl(defined, configuredBlocks)
      storeDb(factory)
      context.become(ready(factory))
      unstashAll()
    case GsonLoaded(_, _) =>
      val map = new java.util.HashMap[Integer, BlockTypeId]()
      map.put(0, BlockTypeId.VACUUM)
      val factory = BlockFactoryImpl(map, configuredBlocks)
      storeDb(factory)
      context.become(ready(factory))
      unstashAll()
    case _ =>
      stash()
  }

  def ready(factory: BlockFactoryImpl): Receive = {
    case GetBlockFactory =>
      sender ! factory
    case GetTextures =>
      sender ! Textures(textures)
  }

}

object BlockMetaActor {
  val NumTextures = 16
  val TextureSize = 16

  def textureFilename(idString: String): String =
    s"/textures/$idString.png"

  def loadTexture(idString: String): BufferedImage = {
    val textureFile = getClass.getResource(textureFilename(idString))
    if(textureFile == null) throw new IllegalStateException(s"No resource for texture: ${textureFilename(idString)}")
    ImageIO.read(textureFile)
  }

  def insertTexture(i: Int, texture: BufferedImage, g: Graphics) {
    val x = (i % NumTextures) * TextureSize
    val y = (NumTextures - (i / NumTextures) - 1) * TextureSize
    g.drawImage(texture, x, y, null)
  }

  def readClasses(config: TypesafeConfig): Array[BlockClassId] = {
    config.root.entrySet.asScala.map( e =>
      Option(config.getString(e.getKey)).map { id =>
        BlockClassId.fromString(id)
      }
    ).flatten.toArray
  }

  def blockType(idString: String, config: TypesafeConfig, texturePosition: Int): (BlockTypeId, BlockType) = {
    val typeId = BlockTypeId.fromString(idString)
    val isObstacle = if(config.hasPath("obstacle")) {
      config.getBoolean("obstacle")
    } else {
      true
    }
    val shape = if(config.hasPath("shape")) {
      BlockShape.fromString(config.getString("shape"))
    } else {
      BlockShape.BLOCK
    }

    val state = if(config.hasPath("state")) {
      BlockState.fromString(config.getString("state"))
    } else {
      BlockState.SOLID
    }

    val classes = if(config.hasPath("classes")) {
      readClasses(config.getConfig("classes"))
    } else {
      BlockType.NO_CLASSES;
    }

    val blockType = if(config.hasPath("faces")) {
      val faces = config.getIntList("faces")
      if(faces.size != 6) throw new IllegalStateException("There must be exactly 6 faces")
      new BlockType(faces.asScala.map(_ + texturePosition).toArray, shape, isObstacle, false, state, classes)
    } else {
      /* Default is to assume only one texture for all faces */
      new BlockType(Array(texturePosition,texturePosition,texturePosition,texturePosition,texturePosition,texturePosition), shape, isObstacle, false, state, classes)
    }
    typeId -> blockType
  }

  def isTransparent(texture: BufferedImage): Boolean = {
    for(
      x <- 0 until texture.getWidth;
      y <- 0 until texture.getHeight) {
      if(0xFF00FF == (texture.getRGB(x, y) & 0x00FFFFFF)) return true
    }
    return false
  }

  def parseBlocks(config: TypesafeConfig): (Seq[(BlockTypeId, BlockType)], Array[Byte]) = {
    val blocks = config.root.entrySet.asScala.map { e =>
      e.getKey -> config.getConfig(e.getKey)
    }
    var texturePosition = 0
    val textures = new BufferedImage(
      NumTextures * TextureSize,
      NumTextures * TextureSize,
      BufferedImage.TYPE_INT_RGB)
    val texturesGraphics = textures.getGraphics()
    val blockSeq = (for((idString, block) <- blocks) yield {
      val t = blockType(idString, block, texturePosition)
      val maxIndex = t._2.getFaces().max + 1
      val numTextures = maxIndex - t._2.getFaces().min
      val img = loadTexture(idString)
      var transparent = false
      for(i <- 0 until numTextures) {
        val texture = img.getSubimage(i * TextureSize, 0, TextureSize, TextureSize)
        if(isTransparent(texture)) transparent = true
        insertTexture(texturePosition + i, texture, texturesGraphics)
      }
      texturePosition = maxIndex
      t._1 -> t._2.withTransparent(transparent)
    }) toSeq
    val texturesBinary = new ByteArrayOutputStream()

    ImageIO.write(textures, "png", texturesBinary)

    import java.io.File
    ImageIO.write(textures, "png", new File("textures.png"))
    (blockSeq, texturesBinary.toByteArray)
  }

  @PluginConstructor
  def props(
             name: String, universe: ActorRef,
             @Config(key = "json-storage") jsonStorage: ActorRef,
             @Config(key = "blocks") blockConfig: TypesafeConfig
           ): Props = {
    print("Loading block data... ")
    val (blocks, textures) = parseBlocks(blockConfig)
    println("done!")
    Props(
      classOf[BlockMetaActor],
      name,
      jsonStorage,
      blocks,
      textures
    )
  }
}
