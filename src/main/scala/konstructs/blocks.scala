package konstructs

import java.util.UUID
import java.awt.image.BufferedImage
import java.awt.Graphics
import java.io.ByteArrayOutputStream
import javax.imageio.ImageIO
import konstructs.plugin.Plugin._

import scala.collection.mutable
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


case class Chunk(data: Array[Byte])

class BlockMetaActor( val ns: String,
                      val jsonStorage: ActorRef,
                      configuredBlocks: Seq[(BlockTypeId, BlockType)],
                      textures: Array[Byte]
                    ) extends Actor with Stash with utils.Scheduled with JsonStorage {

  import BlockMetaActor._

  val PositionMappingFile = "position-mapping"
  val BlockIdFile = "block-id-mapping"

  val typeOfwMapping = new TypeToken[java.util.Map[Integer, BlockTypeId]](){}.getType
  val typeOfPositionMapping = new TypeToken[java.util.Map[String, UUID]](){}.getType

  var factory: BlockFactoryImpl = null
  var positionMapping: java.util.Map[String, UUID] = null

  def load(pos: Position, w: Int, remove: Boolean = false): Block = {
    val uuid = if(remove) {
      positionMapping.remove(str(pos))
    } else {
      positionMapping.get(str(pos))
    }
    factory.createBlock(uuid, w)
  }

  def store(pos: Position, block: Block): Int = {
    if(block.getId != null) {
      positionMapping.put(str(pos), block.getId)
    }
    factory.getW(block)
  }

  schedule(5000, StoreData)

  loadGson(PositionMappingFile)

  private def str(p: Position) = s"${p.getX}-${p.getY}-${p.getZ}"

  def receive = {
    case GsonLoaded(_, json) if json != null =>
      positionMapping = gson.fromJson(json, classOf[java.util.Map[String, UUID]])
      context.become(loadBlockDb)
      loadGson(BlockIdFile)
    case GsonLoaded(_, _) =>
      positionMapping = new java.util.HashMap()
      context.become(loadBlockDb)
      loadGson(BlockIdFile)
    case _ =>
      stash()
  }

  def storeDb() {
    storeGson(BlockIdFile, gson.toJsonTree(factory.getWMapping, typeOfwMapping))
  }

  def loadBlockDb: Receive = {
    case GsonLoaded(_, json) if json != null =>
      val defined = gson.fromJson(json, classOf[java.util.Map[Integer, BlockTypeId]])
      factory = BlockFactoryImpl(defined, configuredBlocks)
      storeDb()
      context.become(ready)
      unstashAll()
    case GsonLoaded(_, _) =>
      val map = new java.util.HashMap[Integer, BlockTypeId]()
      map.put(0, BlockTypeId.VACUUM)
      factory = BlockFactoryImpl(map, configuredBlocks)
      storeDb()
      context.become(ready)
      unstashAll()
    case _ =>
      stash()
  }

  def ready: Receive = {
    case PutBlockTo(pos, block, db) =>
      db ! DbActor.PutBlock(pos, store(pos, block), sender)
    case DbActor.BlockViewed(pos, w, initiator) =>
      initiator ! new BlockViewed(pos, load(pos, w))
    case DbActor.BlockRemoved(pos, w, initiator) =>
      initiator ! new BlockRemoved(pos, load(pos, w, true))
    case DbActor.UnableToPut(pos, w, initiator) =>
      initiator ! new UnableToPut(pos, load(pos, w, true))
    case StoreData =>
      storeGson(PositionMappingFile, gson.toJsonTree(positionMapping, typeOfPositionMapping))
    case GetBlockFactory =>
      sender ! factory
    case GetTextures =>
      sender ! Textures(textures)
  }

}

object BlockMetaActor {
  val NumTextures = 16
  val TextureSize = 16

  case object StoreData

  case class PutBlockTo(pos: Position, block: Block, db: ActorRef)

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

  def blockType(idString: String, config: TypesafeConfig, texturePosition: Int): (BlockTypeId, BlockType) = {
    val typeId = BlockTypeId.fromString(idString)
    val isObstacle = if(config.hasPath("obstacle")) {
      config.getBoolean("obstacle")
    } else {
      true
    }
    val shape = if(config.hasPath("shape")) {
      val s = config.getString("shape")
      if(s != BlockType.SHAPE_BLOCK && s != BlockType.SHAPE_PLANT) {
        throw new IllegalStateException(s"Block shape must be ${BlockType.SHAPE_BLOCK} or ${BlockType.SHAPE_PLANT}")
      }
      s
    } else {
      BlockType.SHAPE_BLOCK
    }
    val blockType = if(config.hasPath("faces")) {
      val faces = config.getIntList("faces")
      if(faces.size != 6) throw new IllegalStateException("There must be exactly 6 faces")
      new BlockType(faces.asScala.map(_ + texturePosition).toArray, shape, isObstacle, false)
    } else {
      /* Default is to assume only one texture for all faces */
      new BlockType(Array(texturePosition,texturePosition,texturePosition,texturePosition,texturePosition,texturePosition), shape, isObstacle, false)
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
