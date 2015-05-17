package konstructs

import scala.math.{ max, Ordering }
import scala.util.Sorting

import akka.actor.{ Actor, Props, ActorRef, Stash, PoisonPill }

import spray.json._

case class Item(amount: Int, w: Int)

case class Inventory(items: Map[String, Item])

case class Player(nick: String, password: String, position: protocol.Position,
  active: Int, inventory: Inventory)

class PlayerActor(pid: Int, nick: String, password: String, client: ActorRef, db: ActorRef, override val jsonStorage: ActorRef, startingPosition: protocol.Position) extends Actor with Stash with utils.Scheduled with JsonStorage {
  import PlayerActor._
  import DbActor._
  import Db.ChunkSize
  import DefaultJsonProtocol._
  import JsonStorage._

  val ns = "players"

  implicit val itemFormat = jsonFormat2(Item)
  implicit val inventoryFormat = jsonFormat1(Inventory)
  implicit val protocolFormat = jsonFormat5(protocol.Position)
  implicit val playerFormat = jsonFormat5(Player)

  var sentChunks = Set.empty[ChunkPosition]
  var chunksToSend = Seq.empty[ChunkPosition]
  var currentChunk = ChunkPosition(Position(startingPosition))
  var data: Player = null
  var maxChunksToSend = 0

  schedule(5000, StoreData)

  load(nick)

  def receive = {
    case JsonLoaded(_, _, Some(json)) =>
      val newData = json.convertTo[Player]
      if(newData.password == password) {
        data = newData
        context.become(ready)
        client ! PlayerInfo(pid, nick, self, data.position)
        unstashAll()
      } else {
        client ! PoisonPill
        context.stop(self)
      }
    case JsonLoaded(_, _, None) =>
      data = Player(nick, password, startingPosition, 0, Inventory(Map()))
      context.become(ready)
      client ! PlayerInfo(pid, nick, self, data.position)
      unstashAll()
    case _ =>
      stash()
  }

  def updateChunk(position: Position) {
    val chunk = ChunkPosition(position)
    if(chunk != currentChunk) {
      val visible = visibleChunks(position, 8)
      sentChunks = sentChunks & visible
      val ordering = (visible &~ sentChunks).toArray
      Sorting.quickSort(ordering)(Ordering.by[ChunkPosition, Double](_.distance(chunk)))
      chunksToSend = ordering.toSeq
      currentChunk = chunk
    }
  }

  def sendChunks() {
    val toSend = chunksToSend.take(maxChunksToSend)
    for(chunk <- toSend) {
      db ! SendBlocks(client, chunk, None)
      maxChunksToSend -= 1
    }
    sentChunks ++= toSend
    chunksToSend = chunksToSend diff toSend
  }

  def update(position: protocol.Position) {
    val pos = Position(position)
    data = data.copy(position = position)
    updateChunk(pos)
    sendChunks()
  }

  def update(inventory: Inventory) {
    data = data.copy(inventory = inventory)
  }

  val random = new scala.util.Random

  def generateTree(pos: Position) {
    val l = LSystem(Seq(
      DeterministicProductionRule("cc", "c[&[c][-c][--c][+c]]c[&[c][-c][--c][+c]]"),
      DeterministicProductionRule("a","aa"),
      ProbabilisticProductionRule("c",
        Seq(
          (20, "c[&[d]]"),
          (20, "c[&[+d]]"),
          (20, "c[&[-d]]"),
          (20, "c[&[--d]]"),
          (20, "cc")
        )),
      ProbabilisticProductionRule("aa", Seq((40, "a[&[c][-c][--c][+c]]"), (60, "bbba")))
    ))
    val m = BlockMachine(Map('a'-> 5, 'b' -> 5, 'c' -> 15, 'd' -> 15))
    val tree = l.iterate("a[&[c][-c][--c][+c]]c", 4 + random.nextInt(5))
    val blocks = m.interpret(tree, pos.copy(y = pos.y - 1))
    for(b <- blocks)
      db ! PutBlock(db, b._1, b._2)
  }

  val material = Set(2,3,4,5,6,8,10,11,12,13).toVector

  def action(pos: Position, button: Int) = {
    button match {
      case 1 =>
        db ! DestroyBlock(self, pos)
      case 2 =>
        if(data.active == 8) {
          generateTree(pos)
        } else {
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
            db ! PutBlock(self, pos, item.w)
          }
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
      store(nick, data.toJson)
    db ! PlayerLogout(pid)
  }

  def ready: Receive = {
    case p: protocol.Position =>
      update(p)
      db ! PlayerMovement(pid, data.position)
    case b: protocol.SendBlock =>
      client ! b
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
      store(nick, data.toJson)
    case l: PlayerLogout =>
      client ! l
    case IncreaseChunks(amount) =>
      maxChunksToSend += amount
      sendChunks()
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
  def props(pid: Int, nick: String, password: String, client: ActorRef, db: ActorRef, store: ActorRef, startingPosition: protocol.Position) = Props(classOf[PlayerActor], pid, nick, password, client, db, store, startingPosition)


  def visibleChunks(position: Position, visibility: Int): Set[ChunkPosition] = {
    val range = -visibility to visibility
    val chunk = ChunkPosition(position)
    (for(p <- range; q <- range; k <-range) yield {
      chunk.translate(p, q, k)
    }).filter(_.k >= 0).toSet
  }
}
