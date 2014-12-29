package craft

import scala.math.max

import akka.actor.{ Actor, Props, ActorRef }

case class Item(amount: Int, w: Int)

case class Inventory(items: Map[Int, Item])

case class PlayerInventory(active: Int, inventory: Inventory)

class PlayerActor(client: ActorRef, world: ActorRef, startingPosition: protocol.Position) extends Actor {
  import PlayerActor._
  import WorldActor._
  import World.ChunkSize

  var position = startingPosition

  var inventory = PlayerInventory(3, Inventory(Map[Int, Item](
    0 -> Item(64, 1),
    1 -> Item(64, 2),
    2 -> Item(64, 3),
    3 -> Item(64, 4),
    4 -> Item(64, 5))))

  def chunk(p: Int, q: Int, v: Option[Int]) {
    val y = position.y.toInt
    val yMin = max(y - LoadYChunks * ChunkSize, 0)
    val yMax = y + LoadYChunks * ChunkSize

    for(y <- yMin until yMax by ChunkSize) {
      val k = y / ChunkSize
      world ! SendBlocks(sender, Chunk(p, q, k), v)
    }
  }

  def receive = {
    case b: protocol.Block =>
      println(s"Player: $b")
      world ! b
    case protocol.Chunk(p, q, v) =>
      chunk(p, q, v)
    case p: protocol.Position =>
      position = p
    case b: protocol.SendBlock =>
      client ! b
    case SendInventory =>
      sender ! InventoryUpdate(inventory.inventory.items)
      sender ! InventoryActiveUpdate(inventory.active)
    case ActivateInventoryItem(active) if(active > 0 && active < 9) =>
      inventory = inventory.copy(active = active)
      sender ! InventoryActiveUpdate(inventory.active)
  }
}

object PlayerActor {
  case object SendInventory
  case class ActivateInventoryItem(activate: Int)
  case class InventoryUpdate(items: Map[Int, Item])
  case class InventoryActiveUpdate(active: Int)
  val LoadYChunks = 1
  def props(client: ActorRef, world: ActorRef, startingPosition: protocol.Position) = Props(classOf[PlayerActor], client, world, startingPosition)
}
