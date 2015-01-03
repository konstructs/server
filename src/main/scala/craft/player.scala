package craft

import scala.math.max

import akka.actor.{ Actor, Props, ActorRef }

case class Item(amount: Int, w: Int)

case class Inventory(items: Map[Int, Item])

class PlayerActor(pid: Int, client: ActorRef, world: ActorRef, startingPosition: protocol.Position) extends Actor {
  import PlayerActor._
  import WorldActor._
  import World.ChunkSize

  var position = startingPosition

  var inventory = Inventory(Map())

  var active = 0

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
        world ! DestroyBlock(self, pos)
      case 2 =>
        inventory.items.get(active).map { item =>
          val updatedItem = item.copy(amount = item.amount - 1)
          if(updatedItem.amount > 0)
            inventory = inventory.copy(items =
              inventory.items + (active -> updatedItem))
          else
            inventory = inventory.copy(items =
              inventory.items - active)
          world ! PutBlock(self, pos, item.w)
          sender ! InventoryUpdate(Map(active -> updatedItem))
        }
    }
  }

  def putInInventory(block: Byte) {
    val item = Item(1, block.toInt)
    inventory.items.find {
      case (position, item) =>
        item.w == block && item.amount < 64
    } match {
      case Some((position, item)) =>
        val itemUpdate = position -> item.copy(amount = item.amount + 1)
        inventory = inventory.copy(items = inventory.items + itemUpdate)
        client ! InventoryUpdate(Map(itemUpdate))
      case None =>
        (0 until 9).find(!inventory.items.get(_).isDefined).map { i =>
          val itemUpdate = i -> Item(1, block)
          inventory = inventory.copy(items = inventory.items + itemUpdate)
          client ! InventoryUpdate(Map(itemUpdate))
        }
    }
  }

  def receive = {
    case protocol.Chunk(p, q, v) =>
      chunk(p, q, v)
    case p: protocol.Position =>
      position = p
      world ! PlayerMovement(pid, position)
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
    case ReceiveBlock(block) =>
      putInInventory(block)
    case p: PlayerMovement =>
      client ! p
  }
}

object PlayerActor {
  case object SendInventory
  case class ReceiveBlock(q: Byte)
  case class PlayerMovement(pid: Int, pos: protocol.Position)
  case class ActivateInventoryItem(activate: Int)
  case class InventoryUpdate(items: Map[Int, Item])
  case class InventoryActiveUpdate(active: Int)
  case class Action(pos: Position, button: Int)
  val LoadYChunks = 5
  def props(pid: Int, client: ActorRef, world: ActorRef, startingPosition: protocol.Position) = Props(classOf[PlayerActor], pid, client, world, startingPosition)
}
