package konstructs

import java.util.UUID

import scala.collection.mutable
import scala.collection.JavaConverters._

import akka.actor.{ Actor, Props, ActorRef, Stash }

import com.google.gson.reflect.TypeToken
import konstructs.plugin.{ PluginConstructor, Config }
import konstructs.api._



class InventoryActor(val ns: String, val jsonStorage: ActorRef) extends Actor
    with Stash with JsonStorage with utils.Scheduled {
  import InventoryActor._

  schedule(5000, StoreData)

  loadGson(InventoriesFile)

  val typeOfInventories = new TypeToken[java.util.Map[String, Inventory]](){}.getType
  var inventories: java.util.Map[String, Inventory] = null

  private def put(blockId: UUID, slot: Int, stack: Stack): Stack = {
    if(inventories.containsKey(blockId.toString)) {
      val inventory = inventories.get(blockId.toString)
      val oldStack = inventory.getStack(slot)

      if(oldStack != null) {
        if(oldStack.acceptsPartOf(stack)) {
          val r = oldStack.acceptPartOf(stack)
          inventories.put(blockId.toString, inventory.withSlot(slot, r.getAccepting))
          r.getGiving
        } else {
          inventories.put(blockId.toString, inventory.withSlot(slot, stack))
          oldStack
        }
      } else {
        inventories.put(blockId.toString, inventory.withSlot(slot, stack))
        null;
      }
    } else {
      stack
    }
  }

  private def get(blockId: UUID, slot: Int): Stack =
    if(inventories.containsKey(blockId.toString)) {
      inventories.get(blockId.toString).getStack(slot)
    } else {
      null
    }

  private def remove(blockId: UUID, slot: Int, amount: StackAmount): Stack =
    if(inventories.containsKey(blockId.toString)) {
      val i = inventories.get(blockId.toString)
      val s = i.getStack(slot)
      if(s == null)
        return null
      amount match {
        case FullStack =>
          inventories.put(blockId.toString, i.withoutSlot(slot))
          s
        case HalfStack =>
          val halfSize = s.size() / 2
          inventories.put(blockId.toString, i.withSlot(slot, s.drop(halfSize)))
          s.take(halfSize)
        case OneBlock =>
          inventories.put(blockId.toString, i.withSlot(slot, s.getTail()))
          Stack.createFromBlock(s.getHead())
      }
    } else {
      null
    }

  def receive = {
    case GsonLoaded(_, json) if json != null =>
      inventories = gson.fromJson(json, typeOfInventories)
        // This handles old inventories where empty stacks wasn't null
      val updatedInventories = new java.util.HashMap[String, Inventory]()
      inventories.asScala.toMap foreach {
        case (pos, inventory) =>
          updatedInventories.put(pos, Inventory.convertPre0_1(inventory))
      }
      inventories = updatedInventories
      context.become(ready)
      unstashAll()
    case GsonLoaded(_, _) =>
      inventories = new java.util.HashMap()
      context.become(ready)
      unstashAll()
    case _ =>
      stash()
  }

  def ready: Receive = {
    case CreateInventory(blockId, size) =>
      if(!inventories.containsKey(blockId.toString)) {
        inventories.put(blockId.toString, Inventory.createEmpty(size))
      }

    case GetInventory(blockId) =>
      sender ! GetInventoryResponse(blockId, Option(inventories.get(blockId.toString)))

    case PutStack(blockId, slot, stack) =>
      val leftovers = put(blockId, slot, stack)
      sender ! new ReceiveStack(leftovers)

    case RemoveStack(blockId, slot, amount) =>
      sender ! new ReceiveStack(remove(blockId, slot, amount))

    case GetStack(blockId, slot) =>
      sender ! GetStackResponse(blockId, slot, get(blockId, slot))

    case DeleteInventory(blockId) =>
      inventories.remove(blockId.toString)

    case StoreData =>
      storeGson(InventoriesFile, gson.toJsonTree(inventories, typeOfInventories))

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
