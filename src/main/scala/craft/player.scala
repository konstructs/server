package craft

import scala.math.max

import akka.actor.{ Actor, Props, ActorRef }

case class Item(amount: Int, w: Int)

case class Inventory(items: Map[Int, Item])

class PlayerActor(client: ActorRef, world: ActorRef, startingPosition: protocol.Position) extends Actor {
  import PlayerActor._
  import WorldActor._
  import World.ChunkSize

  var position = startingPosition

  var inventory = Inventory(Map[Int, Item](
    0 -> Item(64, 1),
    1 -> Item(64, 2),
    2 -> Item(64, 3),
    3 -> Item(64, 4),
    4 -> Item(64, 5)))

  var active = 3

  def chunk(p: Int, q: Int, v: Option[Int]) {
    val y = position.y.toInt
    val yMin = max(y - LoadYChunks * ChunkSize, 0)
    val yMax = y + LoadYChunks * ChunkSize

    for(y <- yMin until yMax by ChunkSize) {
      val k = y / ChunkSize
      world ! SendBlocks(sender, ChunkPosition(p, q, k), v)
    }
  }

  def action(pos: Position, button: Int) = {
    button match {
      case 1 =>
        world ! UpdateBlock(self, pos, 0)
      case 2 =>
        inventory.items.get(active).map { item =>
          val updatedItem = item.copy(amount = item.amount - 1)
          if(updatedItem.amount > 0)
            inventory = inventory.copy(items =
              inventory.items + (active -> updatedItem))
          else
            inventory = inventory.copy(items =
              inventory.items - active)
          world ! UpdateBlock(self, pos, item.w)
          sender ! InventoryUpdate(Map(active -> updatedItem))
        }
    }
  }

  def receive = {
    case protocol.Chunk(p, q, v) =>
      chunk(p, q, v)
    case p: protocol.Position =>
      position = p
    case b: protocol.SendBlock =>
      client ! b
    case SendInventory =>
      sender ! InventoryUpdate(inventory.items)
      sender ! InventoryActiveUpdate(active)
    case ActivateInventoryItem(newActive) if(newActive >= 0 && newActive < 9) =>
      active = newActive
      sender ! InventoryActiveUpdate(active)
    case Action(pos, button) =>
      action(pos, button)
  }
}

object PlayerActor {
  case object SendInventory
  case class ActivateInventoryItem(activate: Int)
  case class InventoryUpdate(items: Map[Int, Item])
  case class InventoryActiveUpdate(active: Int)
  case class Action(pos: Position, button: Int)
  val LoadYChunks = 5
  def props(client: ActorRef, world: ActorRef, startingPosition: protocol.Position) = Props(classOf[PlayerActor], client, world, startingPosition)
}
