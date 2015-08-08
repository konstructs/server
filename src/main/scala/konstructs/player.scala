package konstructs

import java.util.UUID

import scala.math.{ max, Ordering }
import scala.util.Sorting

import akka.actor.{ Actor, Props, ActorRef, Stash, PoisonPill }

import spray.json._

import konstructs.api._

case class Player(nick: String, password: String, position: protocol.Position,
  active: Int, inventory: Inventory)

class PlayerActor(pid: Int, nick: String, password: String, client: ActorRef, db: ActorRef, universe: ActorRef, override val jsonStorage: ActorRef, startingPosition: protocol.Position) extends Actor with Stash with utils.Scheduled with JsonStorage {
  import PlayerActor._
  import Db.ChunkSize
  import KonstructsJsonProtocol._

  val ns = "players"

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
        client ! PlayerInfo(pid, nick, self, data.position)
        context.become(sendBelt)
        unstashAll()
      } else {
        client ! PoisonPill
        context.stop(self)
      }
    case JsonLoaded(_, None) =>
      val inventoryBlock = Block.createWithId(konstructs.SackActor.BlockId)
      universe ! CreateInventory(inventoryBlock.id.get, 16)
      Inventory.createEmpty(9).accept(inventoryBlock) map { inventory =>
        data = Player(nick, password, startingPosition, 0, inventory)
      }
      chunkLoader = context.actorOf(ChunkLoaderActor.props(client, db, Position(data.position)))
      context.become(sendBelt)
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
    client ! BeltUpdate(inventory.stacks)
  }

  val random = new scala.util.Random

  val material = Set(2,3,4,5,6,8,10,11,12,13).toVector

  def getBeltBlock: Option[Block] = {
    val inventory = data.inventory
    val active = data.active
    val block = inventory.blockHeadOption(active)
    if(block.isDefined) {
      update(inventory.stackTail(active))
    }
    block
  }

  def action(pos: Position, button: Int) = {
    button match {
      case 1 =>
        universe ! InteractPrimary(self, nick, pos, getBeltBlock)
      case 2 =>
        getBeltBlock match {
          case None =>
            if(data.active < 6) {
              val m = material(random.nextInt(material.size))
              val stack = Stack.fromSeq(for(i <- 0 until Stack.MaxSize) yield { Block(None, m) })
              update(data.inventory.withSlot(data.active, stack))
            } else {
              universe ! InteractSecondary(self, nick, pos, None)
            }
          case b =>
            universe ! InteractSecondary(self, nick, pos, b)
        }
    }
  }

  def putInBelt(stack: Stack) {
    val block = stack.head
    val inventory = data.inventory
    inventory.accept(block) map { i =>
      update(i)
    }
  }

  def moveInBelt(from: Int, to: Int) {
    val inventory = data.inventory
    update(inventory.moveSlot(from, to))
  }

  override def postStop {
    if(data != null)
      storeJson(nick, data.toJson)
    universe ! PlayerLogout(pid)
  }

  def sendBelt: Receive = {
    /* Send belt*/
    client ! BeltUpdate(data.inventory.stacks)
    client ! BeltActiveUpdate(data.active.toString)
    ready
  }

  def ready: Receive = {
    case p: protocol.Position =>
      update(p)
      universe ! PlayerMovement(pid, data.position)
    case b: BlockDataUpdate =>
      val chunk = ChunkPosition(b.pos)
      client ! protocol.SendBlock(chunk.p, chunk.q, b.pos.x, b.pos.y, b.pos.z, b.newW)
    case ActivateBeltItem(newActive) if(newActive >= 0 && newActive < 9) =>
      data = data.copy(active = newActive)
      sender ! BeltActiveUpdate(data.active.toString)
    case Action(pos, button) =>
      action(pos, button)
    case ReceiveStack(stack) =>
      putInBelt(stack)
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
    case protocol.Say(msg) =>
      universe ! Say(nick, msg)
    case s: Said =>
      client.forward(s)
    case i: IncreaseChunks =>
      chunkLoader.forward(i)
    case ConnectView(inventoryActor, view) =>
      context.become(manageInventory(inventoryActor, view), discardOld = false)
      client ! InventoryUpdate(addBelt(view))
  }

  val BeltView = InventoryView(0,0,1,9)

  def addBelt(view: View) = view.add(BeltView, data.inventory)

  def manageInventory(inventoryActor: ActorRef, view: View): Receive = {
    case MoveItem(from, to) =>
      if(BeltView.contains(from) && BeltView.contains(to)) {
        moveInBelt(BeltView.translate(from), BeltView.translate(to))
        client ! InventoryUpdate(addBelt(view))
      } else if(BeltView.contains(from)) {
        val slot = BeltView.translate(from)
        data.inventory.stackOption(slot) map { stack =>
          update(data.inventory.withoutSlot(slot))
          inventoryActor ! PutViewStack(stack, to)
        }
      } else if(BeltView.contains(to)) {
        val slot = BeltView.translate(to)
        if(!data.inventory.stackOption(slot).isDefined) {
          inventoryActor ! RemoveViewStack(from)
          context.become(waitForStack(slot), discardOld = false)
        }
      } else {
        inventoryActor ! MoveViewStack(from, to)
      }
    case UpdateView(view) =>
      context.become(manageInventory(inventoryActor, view))
      client ! InventoryUpdate(addBelt(view))
    case CloseInventory =>
      inventoryActor ! CloseInventory
      context.unbecome()
      unstashAll()
    case _ =>
      stash()
  }

  def waitForStack(slot: Int): Receive = {
    case ReceiveStack(stack) =>
      update(data.inventory.withSlot(slot, stack))
      context.unbecome()
      unstashAll()
    case CloseInventory =>
      stash()
      context.unbecome()
      unstashAll()
    case _ =>
      stash()
  }
}

object PlayerActor {
  case object StoreData
  case class PlayerMovement(pid: Int, pos: protocol.Position)
  case class PlayerLogout(pid: Int)
  case class PlayerInfo(pid: Int, nick: String, actor: ActorRef, pos: protocol.Position)
  case class PlayerNick(pid: Int, nick: String)
  case class ActivateBeltItem(activate: Int)
  case class BeltUpdate(items: Array[Stack])
  case class BeltActiveUpdate(active: String)
  case class Action(pos: Position, button: Int)
  case class SendInfo(to: ActorRef)
  case class IncreaseChunks(amount: Int)
  case class InventoryUpdate(view: View)
  case object Konstruct
  case class MoveItem(from: Int, to: Int)

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
  var visible = visibleChunks(currentChunk, ChunksVisible)
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
        visible = visibleChunks(currentChunk, ChunksVisible)
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
  val ChunksVisible = 11
  case class UpdatePosition(pos: Position)

  def visibleChunks(chunk: ChunkPosition, visibility: Int): Set[ChunkPosition] = {
    val range = -visibility to visibility
    (for(p <- range; q <- range; k <-range) yield {
      (p, q, k)
    }).filter {
      case (p, q, k) =>
        math.sqrt(p*p + q*q + k*k) < visibility
    } map {
      case (p, q, k) =>
        chunk.translate(p, q, k)
    } toSet
  }

  def props(client: ActorRef, db: ActorRef, position: Position) =
    Props(classOf[ChunkLoaderActor], client, db, position)

}
