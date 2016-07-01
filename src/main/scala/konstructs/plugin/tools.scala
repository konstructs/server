package konstructs.tools

import java.util.UUID
import akka.actor.{ Actor, Props, ActorRef }
import konstructs.plugin.PluginConstructor
import konstructs.KonstructingViewActor
import konstructs.api._
import konstructs.plugin.toolsack.ToolSackActor

class WorkTableActor(universe: ActorRef) extends Actor {
  import WorkTableActor._

  def receive = {
    case i: InteractTertiaryFilter =>
      i.message match {
        case InteractTertiary(sender, player, pos, block, blockAtPosition, true) if blockAtPosition != null && blockAtPosition.getType == BlockId =>
          if(block != null && block.getType() == ToolSackActor.BlockId && block.getId() != null) {
            context.actorOf(KonstructingViewActor.props(sender, universe, block.getId,
              ToolSackActor.SackView, KonstructingView, ResultView))
          } else {
            context.actorOf(KonstructingViewActor.props(sender, universe, null,
              EmptyView, KonstructingView, ResultView))
          }
          i.drop
        case _ =>
          i.continue
      }
  }
}

object WorkTableActor {
  val BlockId = new BlockTypeId("org/konstructs", "work-table")
  val EmptyView = new InventoryView(0, 0, 0, 0)
  val KonstructingView = new InventoryView(9,4,3,3)
  val ResultView = new InventoryView(11,9,1,1)

  @PluginConstructor
  def props(name: String, universe: ActorRef) = Props(classOf[WorkTableActor], universe)

}
