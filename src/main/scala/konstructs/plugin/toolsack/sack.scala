package konstructs.plugin.toolsack

import java.util.UUID
import akka.actor.{Actor, Props, ActorRef}
import konstructs.plugin.PluginConstructor
import konstructs.KonstructingViewActor
import konstructs.api._
import konstructs.api.messages._
class ToolSackActor(universe: ActorRef) extends Actor {
  import ToolSackActor._

  def receive = {
    case f: InteractTertiaryFilter =>
      f.getMessage match {
        case i: InteractTertiary if !i.isWorldPhase && i.getBlock != null && i.getBlock.getType == BlockId =>
          val b = if (i.getBlock.getId == null) {
            val newBlock = i.getBlock.withId(UUID.randomUUID)
            universe ! new CreateInventory(newBlock.getId, 16)
            newBlock
          } else {
            i.getBlock
          }
          context.actorOf(
            KonstructingViewActor.props(i.getSender, universe, b.getId, SackView, KonstructingView, ResultView))
          f.skipWith(self, i.withBlock(b))
        case i: InteractTertiary
            if i.isWorldPhase && i.getBlockAtPosition != null && i.getBlockAtPosition.getType == BlockId =>
          val b = if (i.getBlockAtPosition.getId == null) {
            val newBlock = i.getBlockAtPosition.withId(UUID.randomUUID)
            universe ! new CreateInventory(newBlock.getId, 16)
            newBlock
          } else {
            i.getBlockAtPosition
          }
          context.actorOf(
            KonstructingViewActor.props(i.getSender, universe, b.getId, SackView, KonstructingView, ResultView))
          f.skipWith(self, i.withBlockAtPosition(b))
        case _ =>
          f.next(self)
      }
  }
}

object ToolSackActor {
  val BlockId = new BlockTypeId("org/konstructs", "tool-sack")
  val SackView = new InventoryView(2, 4, 4, 4)
  val KonstructingView = new InventoryView(7, 5, 2, 2)
  val ResultView = new InventoryView(8, 9, 1, 1)

  @PluginConstructor
  def props(name: String, universe: ActorRef) = Props(classOf[ToolSackActor], universe)

}
