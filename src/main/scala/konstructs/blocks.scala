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

import spray.json._

import konstructs.api._
import konstructs.plugin.{ListConfig, PluginConstructor, Config}

case class Chunk(data: Array[Byte])

class BlockMetaActor(
                      val ns: String,
                      val jsonStorage: ActorRef,
                      configuredBlocks: Seq[(BlockTypeId, BlockType)],
                      textures: Array[Byte]
                    ) extends Actor with Stash with utils.Scheduled with JsonStorage {

  import KonstructsJsonProtocol._
  import BlockMetaActor._

  val PositionMappingFile = "position-mapping"
  val BlockIdFile = "block-id-mapping"

  implicit val positionFormat = jsonFormat3(Position.apply)

  val blockTypeIdMapping = mutable.HashMap[BlockTypeId, Int]()
  val wMapping = mutable.HashMap[Int, BlockTypeId]()

  var factory: BlockFactory = null
  var positionMapping: mutable.HashMap[String, UUID] = null

  def load(pos: Position, w: Int, remove: Boolean = false): Block = {
    val uuid = if(remove) {
      positionMapping.remove(str(pos))
    } else {
      positionMapping.get(str(pos))
    }
    factory.block(uuid, w)
  }

  def store(pos: Position, block: Block): Int = {
    if(block.id.isDefined) {
      positionMapping += str(pos) -> block.id.get
    }
    factory.w(block)
  }

  schedule(5000, StoreData)

  loadJson(PositionMappingFile)

  private def str(p: Position) = s"${p.x}-${p.y}-${p.z}"

  def receive = {
    case JsonLoaded(_, Some(json)) =>
      val m = json.convertTo[Map[String, UUID]]
      positionMapping = mutable.HashMap[String, UUID](m.toSeq: _*)
      context.become(loadBlockDb)
      loadJson(BlockIdFile)
    case JsonLoaded(_, None) =>
      positionMapping = mutable.HashMap[String, UUID]()
      context.become(loadBlockDb)
      loadJson(BlockIdFile)
    case _ =>
      stash()
  }

  def storeDb() {
    storeJson(BlockIdFile, factory.wMapping.toSeq.map {
      case (k, v) => k.toString -> v
    }.toMap.toJson)
  }

  def loadBlockDb: Receive = {
    case JsonLoaded(_, Some(json)) =>
      val defined = json.convertTo[Map[String, BlockTypeId]]
      factory = BlockFactory(defined, configuredBlocks)
      storeDb()
      context.become(ready)
      unstashAll()
    case JsonLoaded(_, None) =>
      factory = BlockFactory(Map[String, BlockTypeId]("0" -> BlockTypeId("org/konstructs", "vacuum")), configuredBlocks)
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
      initiator ! BlockViewed(pos, load(pos, w))
    case DbActor.BlockRemoved(pos, w, initiator) =>
      initiator ! BlockRemoved(pos, load(pos, w, true))
    case DbActor.UnableToPut(pos, w, initiator) =>
      initiator ! UnableToPut(pos, load(pos, w, true))
    case StoreData =>
      storeJson(PositionMappingFile, positionMapping.toMap.toJson)
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
      if(s != BlockType.ShapeBlock && s != BlockType.ShapePlant) {
        throw new IllegalStateException(s"Block shape must be ${BlockType.ShapeBlock} or ${BlockType.ShapePlant}")
      }
      s
    } else {
      BlockType.ShapeBlock
    }
    val blockType = if(config.hasPath("faces")) {
      val faces = config.getIntList("faces")
      if(faces.size != 6) throw new IllegalStateException("There must be exactly 6 faces")
      BlockType(faces.asScala.map(_ + texturePosition).asJava, shape, isObstacle, false)
    } else {
      /* Default is to assume only one texture for all faces */
      BlockType(List(texturePosition,texturePosition,texturePosition,texturePosition,texturePosition,texturePosition).asJava, shape, isObstacle, false)
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
      val maxIndex = t._2.faces.asScala.max + 1
      val numTextures = maxIndex - t._2.faces.asScala.min
      val img = loadTexture(idString)
      var transparent = false
      for(i <- 0 until numTextures) {
        val texture = img.getSubimage(i * TextureSize, 0, TextureSize, TextureSize)
        if(isTransparent(texture)) transparent = true
        insertTexture(texturePosition + i, texture, texturesGraphics)
      }
      texturePosition = maxIndex
      t._1 -> t._2.copy(isTransparent = transparent)
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
