package konstructs.plugin.toolsack

import java.util.UUID
import akka.actor.{ Actor, Props, ActorRef }
import konstructs.plugin.PluginConstructor
import konstructs.KonstructingViewActor
import konstructs.api._

class ToolSackActor(universe: ActorRef) extends Actor {
  import ToolSackActor._

  def receive = {
    case i: InteractTertiaryFilter =>
      i.message match {
        case InteractTertiary(sender, player, pos, block) if block != null && block.getType == BlockId =>
          println(block.getId)
          val b = if(block.getId == null) {
            val newBlock = block.withId(UUID.randomUUID)
            universe ! CreateInventory(newBlock.getId, 16)
            newBlock
          } else {
            block
          }
          context.actorOf(KonstructingViewActor.props(sender, universe, b.getId,
            SackView, KonstructingView, ResultView))
          i.dropWith(InteractTertiary(sender, player, pos, b))
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
