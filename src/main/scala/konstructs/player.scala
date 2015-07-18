package konstructs

import scala.math.{ max, Ordering }
import scala.util.Sorting

import akka.actor.{ Actor, Props, ActorRef, Stash, PoisonPill }

import spray.json._

import konstructs.api._

case class Item(amount: Int, w: Int)

case class Inventory(items: Map[String, Item])

case class Player(nick: String, password: String, position: protocol.Position,
  active: Int, inventory: Inventory)

class PlayerActor(pid: Int, nick: String, password: String, client: ActorRef, db: ActorRef, universe: ActorRef, override val jsonStorage: ActorRef, startingPosition: protocol.Position) extends Actor with Stash with utils.Scheduled with JsonStorage {
  import PlayerActor._
  import Db.ChunkSize
  import DefaultJsonProtocol._
  import JsonStorage._

  val ns = "players"

  implicit val itemFormat = jsonFormat2(Item)
  implicit val inventoryFormat = jsonFormat1(Inventory)
  implicit val protocolFormat = jsonFormat5(protocol.Position)
  implicit val playerFormat = jsonFormat5(Player)
  var chunkLoader: ActorRef = null
  var data: Player = null

  schedule(5000, StoreData)

  loadJson(nick)

  def receive = {
    case JsonLoaded(_, Some(json)) =>
      val newData = json.convertTo[Player]
      if(newData.password == password) {
        data = newData
        chunkLoader = context.actorOf(ChunkLoaderActor.props(client, db, Position(data.position)))
        context.become(ready)
        client ! PlayerInfo(pid, nick, self, data.position)
        unstashAll()
      } else {
        client ! PoisonPill
        context.stop(self)
      }
    case JsonLoaded(_, None) =>
      data = Player(nick, password, startingPosition, 0, Inventory(Map()))
      chunkLoader = context.actorOf(ChunkLoaderActor.props(client, db, Position(data.position)))
      context.become(ready)
      client ! PlayerInfo(pid, nick, self, data.position)
      unstashAll()
    case _ =>
      stash()
  }

  def update(position: protocol.Position) {
    val pos = Position(position)
    data = data.copy(position = position)
    chunkLoader ! ChunkLoaderActor.UpdatePosition(pos)
  }

  def update(inventory: Inventory) {
    data = data.copy(inventory = inventory)
  }

  val random = new scala.util.Random

  val material = Set(2,3,4,5,6,8,10,11,12,13).toVector

  def action(pos: Position, button: Int) = {
    button match {
      case 1 =>
        db ! DestroyBlock(pos)
      case 2 =>
        val inventory = data.inventory
        val active = data.active.toString
        inventory.items.get(active).map { item =>
          val updatedItem = item.copy(amount = item.amount - 1)
          if(updatedItem.amount > 0) {
            update(inventory.copy(items =
              inventory.items + (active -> updatedItem)))
            sender ! InventoryUpdate(Map(active -> updatedItem))
          } else {
            if(data.active < 6) {
              val newItem = (active -> Item(64, material(random.nextInt(material.size))))
              update(inventory.copy(items =
                inventory.items + newItem))
              sender ! InventoryUpdate(Map(newItem))
            } else {
              update(inventory.copy(items =
                inventory.items - active))
              sender ! InventoryUpdate(Map(active -> updatedItem))
            }
          }
          db ! PutBlock(pos, item.w)
        }
    }
  }

  def putInInventory(block: Byte) {
    val item = Item(1, block.toInt)
    val inventory = data.inventory
    inventory.items.find {
      case (position, item) =>
        item.w == block && item.amount < 64
    } match {
      case Some((position, item)) =>
        val itemUpdate = position -> item.copy(amount = item.amount + 1)
        update(inventory.copy(items = inventory.items + itemUpdate))
        client ! InventoryUpdate(Map(itemUpdate))
      case None =>
        (0 until 9).find { i =>
          !inventory.items.get(i.toString).isDefined
        } map { i =>
          val itemUpdate = i.toString -> Item(1, block)
          update(inventory.copy(items = inventory.items + itemUpdate))
          client ! InventoryUpdate(Map(itemUpdate))
        }
    }
  }

  override def postStop {
    if(data != null)
      storeJson(nick, data.toJson)
    universe ! PlayerLogout(pid)
  }

  def ready: Receive = {
    case p: protocol.Position =>
      update(p)
      universe ! PlayerMovement(pid, data.position)
    case b: BlockUpdate =>
      val chunk = ChunkPosition(b.pos)
      client ! protocol.SendBlock(chunk.p, chunk.q, b.pos.x, b.pos.y, b.pos.z, b.newW)
    case SendInventory =>
      sender ! InventoryUpdate(data.inventory.items)
      sender ! InventoryActiveUpdate(data.active)
    case ActivateInventoryItem(newActive) if(newActive >= 0 && newActive < 9) =>
      data = data.copy(active = newActive)
      sender ! InventoryActiveUpdate(data.active)
    case Action(pos, button) =>
      action(pos, button)
    case ReceiveBlock(block) =>
      putInInventory(block)
    case p: PlayerMovement =>
      client ! p
    case p: PlayerNick =>
      client ! p
    case SendInfo(to) =>
      to ! PlayerMovement(pid, data.position)
      to ! PlayerNick(pid, data.nick)
    case StoreData =>
      storeJson(nick, data.toJson)
    case l: PlayerLogout =>
      client ! l
    case s: Say =>
      universe ! s
    case s: Said =>
      client.forward(s)
    case i: IncreaseChunks =>
      chunkLoader.forward(i)
  }
}

object PlayerActor {
  case object SendInventory
  case object StoreData
  case class ReceiveBlock(q: Byte)
  case class PlayerMovement(pid: Int, pos: protocol.Position)
  case class PlayerLogout(pid: Int)
  case class PlayerInfo(pid: Int, nick: String, actor: ActorRef, pos: protocol.Position)
  case class PlayerNick(pid: Int, nick: String)
  case class ActivateInventoryItem(activate: Int)
  case class InventoryUpdate(items: Map[String, Item])
  case class InventoryActiveUpdate(active: Int)
  case class Action(pos: Position, button: Int)
  case class SendInfo(to: ActorRef)
  case class IncreaseChunks(amount: Int)

  val LoadYChunks = 5
  def props(pid: Int, nick: String, password: String, client: ActorRef, db: ActorRef, universe: ActorRef, store: ActorRef, startingPosition: protocol.Position) = Props(classOf[PlayerActor], pid, nick, password, client, db, universe, store, startingPosition)

}

class ChunkLoaderActor(client: ActorRef, db: ActorRef, position: Position) extends Actor {
  import ChunkLoaderActor._
  import DbActor._
  import PlayerActor._

  var sentChunks = Map.empty[ChunkPosition, ChunkData]
  var requestedChunks = Set.empty[ChunkPosition]
  var chunksToSend = Seq.empty[ChunkPosition]
  var currentChunk = ChunkPosition(position)
  var visible = visibleChunks(currentChunk, 8)
  var maxChunksToSend = 0
  var centralChunks = neighbors(currentChunk)

  def visibleSentChunksNeighbours =
    sentChunks flatMap {
      case (position, data) =>
        data.chunks(position).filter(visible(_))
    } toSet

  def neighbors(chunk: ChunkPosition) =
    Set(
      chunk,
      chunk.copy(p = chunk.p - 1),
      chunk.copy(p = chunk.p + 1),
      chunk.copy(q = chunk.q - 1),
      chunk.copy(q = chunk.q + 1),
      chunk.copy(k = chunk.k - 1),
      chunk.copy(k = chunk.k + 1)
    )

  def update() {
    sentChunks = sentChunks filterKeys (sentChunks.keySet & visible)
    requestedChunks = requestedChunks & visible
    val filtered = visibleSentChunksNeighbours ++ centralChunks
    val ordering = (filtered &~ requestedChunks).toArray
    Sorting.quickSort(ordering)(Ordering.by[ChunkPosition, Double](_.distance(currentChunk)))
    chunksToSend = ordering.toSeq
  }

  def sendChunks() {
    val toSend = chunksToSend.take(maxChunksToSend)
    for(chunk <- toSend) {
      db ! SendBlocks(chunk, None)
      maxChunksToSend -= 1
    }
    requestedChunks ++= toSend
    chunksToSend = chunksToSend diff toSend
  }

  def receive = {
    case IncreaseChunks(amount) =>
      maxChunksToSend += amount
      sendChunks()
    case UpdatePosition(pos) =>
      val chunk = ChunkPosition(pos)
      if(chunk != currentChunk) {
        currentChunk = chunk
        visible = visibleChunks(currentChunk, 8)
        centralChunks = neighbors(currentChunk)
        update()
        sendChunks()
      }
    case b: BlockList =>
      client ! b
      sentChunks = sentChunks + (b.chunk -> b.data)
      if(sentChunks.keySet == requestedChunks) {
        update()
        sendChunks()
      }
  }

}

object ChunkLoaderActor {
  case class UpdatePosition(pos: Position)

  def visibleChunks(chunk: ChunkPosition, visibility: Int): Set[ChunkPosition] = {
    val range = -visibility to visibility
    (for(p <- range; q <- range; k <-range) yield {
      chunk.translate(p, q, k)
    }).filter(_.k >= 0).toSet
  }

  def props(client: ActorRef, db: ActorRef, position: Position) =
    Props(classOf[ChunkLoaderActor], client, db, position)

}
