package konstructs.plugin.toolsack

import akka.actor.{ Actor, Props, ActorRef }
import konstructs.plugin.PluginConstructor
import konstructs.KonstructingViewActor
import konstructs.api._

class ToolSackActor(universe: ActorRef) extends Actor {
  import ToolSackActor._

  def receive = {
    case i: InteractTertiaryFilter =>
      i.message match {
        case InteractTertiary(sender, _, _, block) if block != null && block.getId != null && block.getType == BlockId =>
          context.actorOf(KonstructingViewActor.props(sender, universe, block.getId,
            SackView, KonstructingView, ResultView))
          i.drop
        case _ =>
          i.continue
      }
  }
}

object ToolSackActor {
  val BlockId = new BlockTypeId("org/konstructs", "tool-sack")
  val SackView = new InventoryView(2, 4, 4, 4)
  val KonstructingView = new InventoryView(7,5,2,2)
  val ResultView = new InventoryView(8,9,1,1)

  @PluginConstructor
  def props(name: String, universe: ActorRef) = Props(classOf[ToolSackActor], universe)

}
