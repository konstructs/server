package konstructs

import java.util.UUID

import scala.collection.mutable

import akka.actor.{ Actor, Props, ActorRef, Stash }

import spray.json._

import konstructs.api._
import konstructs.plugin.{ PluginConstructor, Config }

case class Chunk(data: Array[Byte])

class BlockMetaActor(val ns: String, val jsonStorage: ActorRef)
    extends Actor with Stash with utils.Scheduled with JsonStorage {
  import KonstructsJsonProtocol._
  import BlockMetaActor._

  val PositionMappingFile = "position-mapping"

  implicit val positionFormat = jsonFormat3(Position.apply)

  schedule(5000, StoreData)

  loadJson(PositionMappingFile)

  private def str(p: Position) = s"${p.x}-${p.y}-${p.z}"

  def receive = {
    case JsonLoaded(_, Some(json)) =>
      val positionMapping = json.convertTo[Map[String, UUID]]
      context.become(ready(mutable.HashMap[String, UUID](positionMapping.toSeq: _*)))
      unstashAll()
    case JsonLoaded(_, None) =>
      context.become(ready(mutable.HashMap[String, UUID]()))
      unstashAll()
    case _ =>
      stash()
  }


  def ready(positionMapping: mutable.HashMap[String, UUID]): Receive = {
    case GetBlockId(pos, w, sendTo) =>
      val uuid = positionMapping.get(str(pos))
      sendTo ! GetBlockResponse(pos, Block(uuid, w))
    case ReceiveBlockId(pos, w, sendTo) =>
      val uuid = positionMapping.remove(str(pos))
      sendTo ! ReceiveStack(Stack(Seq(Block(uuid, w))))
    case GetOrCreateBlockId(pos) =>
      val uuid = positionMapping.getOrElseUpdate(str(pos), UUID.randomUUID)
      sender ! GetOrCreateBlockIdResponse(pos, uuid)
    case PutBlockId(pos, block, old, sendTo) =>
      if(block.id.isDefined) {
        positionMapping += str(pos) -> block.id.get
      }
      sendTo ! BlockDataUpdate(pos, old.toInt, block.w)
    case StoreData =>
      storeJson(PositionMappingFile, positionMapping.toMap.toJson)
  }


}

object BlockMetaActor {
  case object StoreData
  case class GetBlockId(pos: Position, w: Int, sendTo: ActorRef)
  case class ReceiveBlockId(pos: Position, w: Int, sendTo: ActorRef)
  case class PutBlockId(pos: Position, block: Block, old: Int, sendTo: ActorRef)
  def props(ns: String, jsonStorage: ActorRef): Props =
    Props(classOf[BlockMetaActor], ns, jsonStorage)
}
