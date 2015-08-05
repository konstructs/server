package konstructs

import java.util.UUID

import scala.collection.mutable

import akka.actor.{ Actor, Props, ActorRef, Stash }

import spray.json._

import konstructs.plugin.{ PluginConstructor, Config }
import konstructs.api._

class InventoryActor(val ns: String, val jsonStorage: ActorRef) extends Actor
    with Stash with JsonStorage with utils.Scheduled {
  import InventoryActor._
  import KonstructsJsonProtocol._

  schedule(5000, StoreData)

  loadJson(InventoriesFile)

  var inventories: mutable.HashMap[String, Inventory] = null

  private def put(blockId: UUID, slot: Int, stack: Stack): Option[Stack] = {
    if(inventories.contains(blockId.toString)) {
      val inventory = inventories(blockId.toString)
      inventories += blockId.toString -> inventory.withSlot(slot, stack)
      inventory.stackOption(slot)
    } else {
      Some(stack)
    }
  }

  private def get(blockId: UUID, slot: Int): Option[Stack] =
    inventories.get(blockId.toString).flatMap { i =>
      i.stackOption(slot)
    }

  private def remove(blockId: UUID, slot: Int): Option[Stack] =
    inventories.get(blockId.toString).flatMap { i =>
      inventories += blockId.toString -> i.withoutSlot(slot)
      i.stackOption(slot)
    }

  def receive = {
    case JsonLoaded(_, Some(json)) =>
      val i = json.convertTo[Map[String, Inventory]]
      inventories = mutable.HashMap[String, Inventory](i.toSeq: _*)
      context.become(ready)
      unstashAll()
    case JsonLoaded(_, None) =>
      inventories = mutable.HashMap[String, Inventory]()
      context.become(ready)
      unstashAll()
    case _ =>
      stash()
  }

  def ready: Receive = {
    case CreateInventory(blockId, size) =>
      if(!inventories.contains(blockId.toString)) {
        inventories += blockId.toString -> Inventory.createEmpty(size)
      }

    case GetInventory(blockId) =>
      sender ! GetInventoryResponse(blockId, inventories.get(blockId.toString))

    case PutStack(blockId, slot, stack) =>
      put(blockId, slot, stack).map { s =>
        sender ! ReceiveStack(s)
      }

    case RemoveStack(blockId, slot) =>
      val stack = remove(blockId, slot)
      if(stack.isDefined) {
        sender ! ReceiveStack(stack.get)
      } else {
        sender ! ReceiveStack(Stack.Empty)
      }

    case GetStack(blockId, slot) =>
      sender ! GetStackResponse(blockId, slot, get(blockId, slot))

    case DeleteInventory(blockId) =>
      inventories -= blockId.toString

    case MoveStack(fromBlockId, from, toBlockId, to) =>
      if(!get(toBlockId, to).isDefined) {
        remove(fromBlockId, from).map(put(toBlockId, to, _))
      }

    case StoreData =>
      storeJson(InventoriesFile, inventories.toMap.toJson)

  }
}

object InventoryActor {
  case object StoreData
  val InventoriesFile = "inventories"

  @PluginConstructor
  def props(name: String, universe: ActorRef,
    @Config(key = "json-storage") jsonStorage: ActorRef): Props =
    Props(classOf[InventoryActor], name, jsonStorage)
}
