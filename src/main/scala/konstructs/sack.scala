package konstructs

import java.util.UUID

import akka.actor.{ Actor, Props, ActorRef, Stash }

import konstructs.plugin.PluginConstructor
import konstructs.api._

class SackActor(universe: ActorRef) extends Actor {
  import SackActor._

  def receive = {
    case i: InteractSecondaryFilter =>
      i.message match {
        case InteractSecondary(sender, _, _, Some(Block(Some(blockId), BlockId))) =>
          sender ! ReceiveStack(Stack.fromBlock(i.message.block.get))
          context.actorOf(SackViewActor.props(sender, universe, blockId))
          i.drop
        case _ =>
          i.continue
      }
  }
}

object SackActor {
  val BlockId = 32

  @PluginConstructor
  def props(name: String, universe: ActorRef) = Props(classOf[SackActor], universe)

}

class SackViewActor(player: ActorRef, inventoryManager: ActorRef, blockId: UUID)
    extends Actor with Stash {
  import SackViewActor._

  inventoryManager ! GetInventory(blockId)

  def receive = {
    case GetInventoryResponse(_, Some(inventory)) =>
      context.become(ready(inventory))
      player ! ConnectView(self, View.Empty.add(SackView, inventory))
    case _ =>
      context.stop(self)
  }


  def awaitInventory: Receive = {
    case GetInventoryResponse(_, Some(inventory)) =>
      context.become(ready(inventory))
      player ! UpdateView(View.Empty.add(SackView, inventory))
      unstashAll()
    case CloseInventory =>
      context.stop(self)
    case _ =>
      stash()

  }

  def ready(inventory: Inventory): Receive = {
    case MoveViewStack(from, to) =>
      if(SackView.contains(from) && SackView.contains(to)) {
        context.become(awaitInventory)
        inventoryManager ! MoveStack(blockId, SackView.translate(from), blockId, SackView.translate(to))
        inventoryManager ! GetInventory(blockId)
      }
    case PutViewStack(stack, to) =>
      if(SackView.contains(to)) {
        context.become(awaitInventory)
        inventoryManager.forward(PutStack(blockId, SackView.translate(to), stack))
        inventoryManager ! GetInventory(blockId)
      }
    case RemoveViewStack(from) =>
      if(SackView.contains(from)) {
        context.become(awaitInventory)
        inventoryManager.forward(RemoveStack(blockId, SackView.translate(from)))
        inventoryManager ! GetInventory(blockId)
      }
    case CloseInventory =>
      context.stop(self)
  }

}

object SackViewActor {
  val SackView = InventoryView(4, 0, 4, 4)
  def props(player: ActorRef, inventoryManager: ActorRef, blockId: UUID): Props =
    Props(classOf[SackViewActor], player, inventoryManager, blockId)
}
