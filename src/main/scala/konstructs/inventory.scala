package konstructs

import java.util.UUID

import scala.collection.mutable
import scala.collection.JavaConverters._

import akka.actor.{Actor, Props, ActorRef, Stash}

import com.google.gson.reflect.TypeToken
import konstructs.plugin.{PluginConstructor, Config}
import konstructs.api.messages._
import konstructs.api._

class InventoryActor(val ns: String, val jsonStorage: ActorRef)
    extends Actor
    with Stash
    with JsonStorage
    with utils.Scheduled {
  import InventoryActor._

  schedule(5000, StoreData)

  loadGson(InventoriesFile)

  val typeOfInventories = new TypeToken[java.util.Map[String, Inventory]]() {}.getType
  val typeOfOtherInventories =
    new TypeToken[java.util.Map[String, java.util.Map[String, Inventory]]]() {}.getType
  var storageInventories: java.util.Map[String, Inventory] = null
  var otherInventories: java.util.Map[String, java.util.Map[String, Inventory]] = null

  private def inventoryExists(blockId: UUID, inventoryId: InventoryId) =
    if (inventoryId == InventoryId.STORAGE) {
      storageInventories.containsKey(blockId.toString)
    } else {
      if (otherInventories.containsKey(blockId.toString)) {
        otherInventories.get(blockId.toString).containsKey(inventoryId.idString)
      } else {
        false
      }
    }

  private def getInventory(blockId: UUID, inventoryId: InventoryId) =
    if (inventoryId == InventoryId.STORAGE) {
      storageInventories.get(blockId.toString)
    } else {
      val blockInventories = otherInventories.get(blockId.toString)
      if (blockInventories != null) {
        blockInventories.get(inventoryId.idString)
      } else {
        null
      }
    }

  private def putInventory(blockId: UUID, inventoryId: InventoryId, inventory: Inventory): Unit = {
    if (inventoryId == InventoryId.STORAGE) {
      storageInventories.put(blockId.toString, inventory)
    } else {
      if (otherInventories.get(blockId.toString) == null) {
        otherInventories.put(blockId.toString, new java.util.HashMap[String, Inventory]())
      }
      otherInventories.get(blockId.toString).put(inventoryId.idString, inventory)
    }
  }

  private def removeInventory(blockId: UUID, inventoryId: InventoryId) =
    if (inventoryId == InventoryId.STORAGE) {
      storageInventories.remove(blockId.toString)
    } else {
      val blockInventories = otherInventories.get(blockId.toString)
      blockInventories.remove(inventoryId.idString)
      if (blockInventories.isEmpty) {
        otherInventories.remove(blockId.toString)
      }
    }

  private def put(blockId: UUID, inventoryId: InventoryId, slot: Int, stack: Stack): Stack = {
    if (inventoryExists(blockId, inventoryId)) {
      val inventory = getInventory(blockId, inventoryId)
      val oldStack = inventory.getStack(slot)

      if (oldStack != null) {
        if (oldStack.canAcceptPartOf(stack)) {
          val r = oldStack.acceptPartOf(stack)
          putInventory(blockId, inventoryId, inventory.withSlot(slot, r.getAccepting))
          r.getGiving
        } else {
          putInventory(blockId, inventoryId, inventory.withSlot(slot, stack))
          oldStack
        }
      } else {
        putInventory(blockId, inventoryId, inventory.withSlot(slot, stack))
        null;
      }
    } else {
      stack
    }
  }

  private def add(blockId: UUID, inventoryId: InventoryId, stack: Stack) =
    if (inventoryExists(blockId, inventoryId)) {
      val result = getInventory(blockId, inventoryId).acceptPartOf(stack)
      putInventory(blockId, inventoryId, result.getAccepting())
      result.getGiving
    } else {
      stack
    }

  private def get(blockId: UUID, inventoryId: InventoryId, slot: Int): Stack =
    if (inventoryExists(blockId, inventoryId)) {
      getInventory(blockId, inventoryId).getStack(slot)
    } else {
      null
    }

  private def remove(blockId: UUID, inventoryId: InventoryId, slot: Int, amount: StackAmount): Stack =
    if (inventoryExists(blockId, inventoryId)) {
      val i = getInventory(blockId, inventoryId)
      val s = i.getStack(slot)
      if (s == null)
        return null
      putInventory(blockId, inventoryId, i.withSlot(slot, s.drop(amount)))
      s.take(amount)
    } else {
      null
    }

  private def remove(blockId: UUID, inventoryId: InventoryId, blockTypeId: BlockTypeId, amount: Int): Stack =
    if (inventoryExists(blockId, inventoryId)) {
      val i = getInventory(blockId, inventoryId)
      putInventory(blockId, inventoryId, i.drop(blockTypeId, amount))
      i.take(blockTypeId, amount)
    } else {
      null
    }

  private def transfer(fromBlockId: UUID,
                       fromInventoryId: InventoryId,
                       toBlockId: UUID,
                       toInventoryId: InventoryId,
                       blockTypeId: BlockTypeId,
                       amount: Int) {
    if (inventoryExists(fromBlockId, fromInventoryId) && inventoryExists(toBlockId, toInventoryId)) {
      val from = getInventory(fromBlockId, fromInventoryId)
      val to = getInventory(toBlockId, toInventoryId)

      val stack = from.take(blockTypeId, amount)
      if (stack != null) {
        val leftovers = to.acceptPartOf(stack)

        /* Check if we could successfully add all blocks from the stack to the inventory */
        if (leftovers.getGiving == null) {
          /* Save the transfer */
          putInventory(fromBlockId, fromInventoryId, from.drop(blockTypeId, amount))
          putInventory(toBlockId, toInventoryId, leftovers.getAccepting)
        }
      }
    }
  }

  def receive = {
    case GsonLoaded(_, json) if json != null =>
      storageInventories = gson.fromJson(json, typeOfInventories)
      // This handles old inventories where empty stacks wasn't null
      val updatedInventories = new java.util.HashMap[String, Inventory]()
      storageInventories.asScala.toMap foreach {
        case (pos, inventory) =>
          updatedInventories.put(pos, Inventory.convertPre0_1(inventory))
      }
      storageInventories = updatedInventories
      context.become(loadOtherInventories())
      unstashAll()
    case GsonLoaded(_, _) =>
      storageInventories = new java.util.HashMap()
      context.become(loadOtherInventories())
      unstashAll()
    case _ =>
      stash()
  }

  def loadOtherInventories(): Receive = {
    loadGson(OtherInventoriesFile)
    val f: Receive = {
      case GsonLoaded(_, json) if json != null =>
        otherInventories = gson.fromJson(json, typeOfOtherInventories)
        context.become(ready)
        unstashAll()
      case GsonLoaded(_, _) =>
        otherInventories = new java.util.HashMap()
        context.become(ready)
        unstashAll()
      case _ =>
        stash()
    }
    f
  }

  def ready: Receive = {
    case c: CreateInventory =>
      if (!inventoryExists(c.getBlockId, c.getInventoryId)) {
        putInventory(c.getBlockId, c.getInventoryId, Inventory.createEmpty(c.getSize))
      }

    case g: GetInventory =>
      sender ! new GetInventoryResult(g.getBlockId, g.getInventoryId, getInventory(g.getBlockId, g.getInventoryId))

    case p: PutStackIntoSlot =>
      sender ! new ReceiveStack(put(p.getBlockId, p.getInventoryId, p.getSlot, p.getStack))

    case a: AddToInventory =>
      sender ! new ReceiveStack(add(a.getBlockId, a.getInventoryId, a.getStack))

    case r: RemoveFromInventory =>
      sender ! new ReceiveStack(remove(r.getBlockId, r.getInventoryId, r.getBlockTypeId, r.getAmount))

    case r: RemoveStackFromSlot =>
      sender ! new ReceiveStack(remove(r.getBlockId, r.getInventoryId, r.getSlot, r.getAmount))

    case t: TransferBetweenInventories =>
      transfer(t.getFromBlockId,
               t.getFromInventoryId,
               t.getToBlockId,
               t.getToInventoryId,
               t.getBlockTypeId,
               t.getAmount)

    case d: DeleteInventory =>
      removeInventory(d.getBlockId, d.getInventoryId)

    case StoreData =>
      storeGson(InventoriesFile, gson.toJsonTree(storageInventories, typeOfInventories))
      storeGson(OtherInventoriesFile, gson.toJsonTree(otherInventories, typeOfOtherInventories))

    case g: GetInventoriesView =>
      var view = View.EMPTY
      for ((inventoryId, inventoryView) <- g.getInventories.asScala) {
        val inventory = getInventory(g.getBlockId, inventoryId)
        if (inventory != null) {
          view = view.add(inventoryView, inventory)
        }
      }
      sender ! new UpdateView(view)
  }
}

object InventoryActor {
  case object StoreData
  val InventoriesFile = "inventories"
  val OtherInventoriesFile = "other_inventories"

  @PluginConstructor
  def props(name: String, universe: ActorRef, @Config(key = "json-storage") jsonStorage: ActorRef): Props =
    Props(classOf[InventoryActor], name, jsonStorage)
}
