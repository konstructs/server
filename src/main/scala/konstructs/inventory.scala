package konstructs

import java.util.UUID

import scala.collection.mutable

import akka.actor.{ Actor, Props, ActorRef }

import konstructs.plugin.PluginConstructor
import konstructs.api._

class InventoryActor extends Actor {

  val inventories: mutable.HashMap[UUID, Inventory] = mutable.HashMap[UUID, Inventory]()

  private def put(blockId: UUID, slot: Int, stack: Stack): Option[Stack] = {
    if(inventories.contains(blockId)) {
      val inventory = inventories(blockId)
      inventories += blockId -> inventory.withSlot(slot, stack)
      inventory.stackOption(slot)
    } else {
      Some(stack)
    }
  }

  private def get(blockId: UUID, slot: Int): Option[Stack] =
    inventories.get(blockId).flatMap { i =>
      i.stackOption(slot)
    }

  private def remove(blockId: UUID, slot: Int): Option[Stack] =
    inventories.get(blockId).flatMap { i =>
      inventories += blockId -> i.withoutSlot(slot)
      i.stackOption(slot)
    }

  def receive = {
    case CreateInventory(blockId, size) =>
      if(!inventories.contains(blockId)) {
        inventories += blockId -> Inventory.createEmpty(size)
      }

    case GetInventory(blockId) =>
      sender ! GetInventoryResponse(blockId, inventories.get(blockId))

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
      inventories -= blockId

    case MoveStack(fromBlockId, from, toBlockId, to) =>
      if(!get(toBlockId, to).isDefined) {
        remove(fromBlockId, from).map(put(toBlockId, to, _))
      }
  }
}

object InventoryActor {

  @PluginConstructor
  def props(name: String, universe: ActorRef): Props =
    Props(classOf[InventoryActor])
}
