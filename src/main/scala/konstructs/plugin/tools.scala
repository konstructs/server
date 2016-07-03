package konstructs.tools

import java.util.UUID
import akka.actor.{ Actor, Props, ActorRef }
import konstructs.plugin.PluginConstructor
import konstructs.KonstructingViewActor
import konstructs.api._
import konstructs.api.messages._
import konstructs.plugin.toolsack.ToolSackActor

class WorkTableActor(universe: ActorRef) extends Actor {
  import WorkTableActor._

  def receive = {
    case f: InteractTertiaryFilter =>
      f.getMessage match {
        case i: InteractTertiary if i.isWorldPhase && i.getBlockAtPosition != null && i.getBlockAtPosition.getType == BlockId =>
          if(i.getBlock != null && i.getBlock.getType == ToolSackActor.BlockId && i.getBlock.getId() != null) {
            context.actorOf(KonstructingViewActor.props(i.getSender, universe, i.getBlock.getId,
              ToolSackActor.SackView, KonstructingView, ResultView))
          } else {
            context.actorOf(KonstructingViewActor.props(i.getSender, universe, null,
              EmptyView, KonstructingView, ResultView))
          }
          f.skip(self)
        case _ =>
          f.next(self)
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
