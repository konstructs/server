package konstructs

import java.util.UUID

import scala.collection.mutable

import akka.actor.Actor

import konstructs.api._

class InventoryActor extends Actor {

  val inventories: mutable.HashMap[UUID, Inventory] = mutable.HashMap[UUID, Inventory]()

  private def put(blockId: UUID, slot: String, stack: Stack): Option[Stack] = {
    if(inventories.contains(blockId)) {
      val inventory = inventories(blockId)
      val items = inventory.items
      inventories += blockId -> Inventory(items + (slot -> stack))
      items.get(slot)
    } else {
      Some(stack)
    }
  }

  private def get(blockId: UUID, slot: String): Option[Stack] =
    inventories.get(blockId).flatMap { i =>
      i.items.get(slot)
    }

  private def remove(blockId: UUID, slot: String): Option[Stack] =
    inventories.get(blockId).flatMap { i =>
      val items = i.items
        inventories += blockId -> Inventory(items - slot)
      items.get(slot)
    }

  def receive = {
    case CreateInventory(blockId) =>
      if(!inventories.contains(blockId)) {
        inventories += blockId -> Inventory(Map())
      }

    case GetInventory(blockId) =>
      sender ! GetInventoryResponse(blockId, inventories.get(blockId))

    case PutStack(blockId, slot, stack) =>
      put(blockId, slot, stack).map { s =>
        sender ! ReceiveStack(s)
      }

    case GetSlot(blockId, slot) =>
      sender ! GetSlotResponse(blockId, slot, get(blockId, slot))

    case DeleteInventory(blockId) =>
      inventories -= blockId

    case MoveStack(fromBlockId, from, toBlockId, to) =>
      if(!get(toBlockId, to).isDefined) {
        remove(fromBlockId, from).map(put(toBlockId, to, _))
      }
  }
}
