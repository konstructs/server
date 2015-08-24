package konstructs

import akka.actor.{ Actor, Props, ActorRef }

import konstructs.plugin.PluginConstructor
import konstructs.api._

class ToolSackActor(universe: ActorRef) extends Actor {
  import ToolSackActor._

  def receive = {
    case i: InteractTertiaryFilter =>
      i.message match {
        case InteractTertiary(sender, _, _, Some(Block(Some(blockId), BlockId))) =>
          sender ! ReceiveStack(Stack.fromBlock(i.message.block.get))
          context.actorOf(KonstructingViewActor.props(sender, universe, blockId,
            SackView, KonstructingView, ResultView))
          i.drop
        case _ =>
          i.continue
      }
  }
}

object ToolSackActor {
  val BlockId = BlockTypeId("org/konstructs", "tool-sack")
  val SackView = InventoryView(4, 2, 4, 4)
  val KonstructingView = InventoryView(10,3,2,2)
  val ResultView = InventoryView(11,7,1,1)

  @PluginConstructor
  def props(name: String, universe: ActorRef) = Props(classOf[ToolSackActor], universe)

}
