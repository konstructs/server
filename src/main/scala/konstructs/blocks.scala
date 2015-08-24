package konstructs

import java.util.UUID

import scala.collection.mutable
import scala.collection.JavaConverters._

import com.typesafe.config.{ Config => TypesafeConfig }
import akka.actor.{ Actor, Props, ActorRef, Stash }

import spray.json._

import konstructs.api._
import konstructs.plugin.{ PluginConstructor, Config }

case class Chunk(data: Array[Byte])

class BlockMetaActor(val ns: String, val jsonStorage: ActorRef,
  configuredBlocks: Seq[BlockTypeId])
    extends Actor with Stash with utils.Scheduled with JsonStorage {
  import KonstructsJsonProtocol._
  import BlockMetaActor._

  println(configuredBlocks)

  val PositionMappingFile = "position-mapping"
  val BlockIdFile = "block-id-mapping"

  implicit val positionFormat = jsonFormat3(Position.apply)

  val blockTypeIdMapping = mutable.HashMap[BlockTypeId, Int]()
  val wMapping = mutable.HashMap[Int, BlockTypeId]()
  var positionMapping: mutable.HashMap[String, UUID] = null

  def findFreeW: Int = {
    for(w <- 0 until 256) {
      if(!wMapping.contains(w)) {
        return w
      }
    }
    throw new IllegalStateException("No free w to allocate for new block type")
  }

  def addBlockType(t: BlockTypeId) {
    val w = blockTypeIdMapping.getOrElse(t, findFreeW)
    updateMapping(w, t)
  }

  def updateMapping(w: Int, t: BlockTypeId) {
    blockTypeIdMapping += t -> w
    wMapping += w -> t
  }

  def load(pos: Position, w: Int, remove: Boolean = false): Block = {
    val uuid = if(remove) {
      positionMapping.remove(str(pos))
    } else {
      positionMapping.get(str(pos))
    }
    val t = wMapping(w)
    Block(uuid, t)
  }

  def store(pos: Position, block: Block): Int = {
    if(block.id.isDefined) {
      positionMapping += str(pos) -> block.id.get
    }
    blockTypeIdMapping(block.`type`)
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

  def loadBlockDb: Receive = {
    case JsonLoaded(_, Some(json)) =>
      val m = json.convertTo[Map[String, BlockTypeId]]
      for((wString, tId) <- m) {
        val w = wString.toInt
        wMapping += w -> tId
        blockTypeIdMapping += tId -> w
      }
      context.become(createDb)
    case JsonLoaded(_, None) =>
      context.become(createDb)
    case _ =>
      stash()
  }

  def createDb: Receive = {
    configuredBlocks.map(addBlockType)
    val r = ready
    unstashAll()
    r
  }

  def ready: Receive = {
    case ViewBlockTo(pos, db) =>
      db ! DbActor.ViewBlock(pos, sender)
    case ReplaceBlockTo(pos, block, db) =>
      db ! DbActor.ReplaceBlock(pos, store(pos, block), sender)
    case PutBlockTo(pos, block, db) =>
      db ! DbActor.PutBlock(pos, store(pos, block), sender)
    case RemoveBlockTo(pos, db) =>
      db ! DbActor.RemoveBlock(pos, sender)
    case DbActor.BlockViewed(pos, w, initiator) =>
      initiator ! BlockViewed(pos, load(pos, w))
    case DbActor.BlockRemoved(pos, w, initiator) =>
      initiator ! BlockRemoved(pos, load(pos, w, true))
    case DbActor.UnableToPut(pos, w, initiator) =>
      initiator ! UnableToPut(pos, load(pos, w, true))
    case StoreData =>
      storeJson(PositionMappingFile, positionMapping.toMap.toJson)
      storeJson(BlockIdFile, wMapping.toSeq.map {
        case (k, v) => k.toString -> v
      }.toMap.toJson)
    case ConvertStack(stack: Stack) =>
      sender ! ConvertedStack(stack, blockTypeIdMapping(stack.typeId))
    case ConvertStacks(stacks: Seq[Stack]) =>
      sender ! ConvertedStacks(stacks.map { stack =>
        ConvertedStack(stack, blockTypeIdMapping(stack.typeId))
      })
    case ConvertView(view: View) =>
      sender ! ConvertedView(for((p, stack) <- view.items) yield {
        p -> stack.map { s =>
          ConvertedStack(s, blockTypeIdMapping(s.typeId))
        }
      })


  }

}

object BlockMetaActor {
  case object StoreData

  case class PutBlockTo(pos: Position, block: Block, db: ActorRef)
  case class ReplaceBlockTo(pos: Position, block: Block, db: ActorRef)
  case class RemoveBlockTo(pos: Position, db: ActorRef)
  case class ViewBlockTo(pos: Position, db: ActorRef)

  case class ConvertStack(stack: Stack)
  case class ConvertStacks(stacks: Seq[Stack])
  case class ConvertedStack(stack: Stack, w: Int)
  case class ConvertedStacks(stacks: Seq[ConvertedStack])
  case class ConvertView(view: View)
  case class ConvertedView(items: Map[Int, Option[ConvertedStack]])

  def parseBlocks(config: TypesafeConfig): Seq[BlockTypeId] = {
    val blocks = config.root.entrySet.asScala.map { e =>
      e.getKey -> config.getConfig(e.getKey)
    }
    (for((idString, block) <- blocks) yield {
      BlockTypeId.fromString(idString)
    }) toSeq
  }

  @PluginConstructor
  def props(name: String, universe: ActorRef,
    @Config(key = "json-storage") jsonStorage: ActorRef,
    @Config(key = "blocks") blockConfig: TypesafeConfig): Props =
    Props(classOf[BlockMetaActor], name, jsonStorage, parseBlocks(blockConfig))
}
