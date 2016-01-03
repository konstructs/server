package konstructs

import konstructs.plugin.toolsack.ToolSackActor

import scala.math.Ordering
import scala.util.Sorting

import akka.actor.{ Actor, Props, ActorRef, Stash, PoisonPill }

import spray.json._

import konstructs.api._

case class Player(nick: String, password: String, position: protocol.Position,
  active: Int, inventory: Inventory)

class PlayerActor(
                   pid: Int,
                   nick: String,
                   password: String,
                   client: ActorRef,
                   db: ActorRef,
                   universe: ActorRef,
                   override val jsonStorage: ActorRef,
                   startingPosition: protocol.Position
                 ) extends Actor with Stash with utils.Scheduled with JsonStorage {

  import PlayerActor._
  import KonstructsJsonProtocol._
  import DbActor.BlockList

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
        if(data.inventory.isEmpty) {
          val inventoryBlock = Block.createWithId(ToolSackActor.BlockId)
          universe ! CreateInventory(inventoryBlock.id.get, 16)
          val inventory = Inventory.createEmpty(9).withSlot(0, Stack.fromBlock(inventoryBlock))
          data = data.copy(inventory = inventory)
        }
        chunkLoader = context.actorOf(ChunkLoaderActor.props(client, db, Position(data.position)))
        client ! PlayerInfo(pid, nick, self, data.position)
        context.become(sendBelt)
        unstashAll()
      } else {
        client ! PoisonPill
        context.stop(self)
      }
    case JsonLoaded(_, None) =>
      val inventoryBlock = Block.createWithId(ToolSackActor.BlockId)
      universe ! CreateInventory(inventoryBlock.id.get, 16)
      val inventory = Inventory.createEmpty(9).withSlot(0, Stack.fromBlock(inventoryBlock))
      data = Player(nick, password, startingPosition, 0, inventory)
      chunkLoader = context.actorOf(ChunkLoaderActor.props(client, db, Position(data.position)))
      client ! PlayerInfo(pid, nick, self, data.position)
      context.become(sendBelt)
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

  val material = Set(BlockTypeId("org/konstructs", "sand"),BlockTypeId("org/konstructs", "stone-brick"),BlockTypeId("org/konstructs", "brick"),BlockTypeId("org/konstructs", "wood"),BlockTypeId("org/konstructs", "stone"),BlockTypeId("org/konstructs", "plnaks"),BlockTypeId("org/konstructs", "glass"),BlockTypeId("org/konstructs", "cobble"),BlockTypeId("org/konstructs", "white-stone"),BlockTypeId("org/konstructs", "framed-stone")).toVector

  def getBeltBlock: Option[Block] = {
    val inventory = data.inventory
    val active = data.active
    val block = inventory.blockHeadOption(active)
    if(block.isDefined) {
      update(inventory.stackTail(active))
    }
    block
  }

  def action(pos: Option[Position], button: Int) = {
    button match {
      case 1 =>
        universe ! InteractPrimary(self, nick, pos, getBeltBlock)
      case 2 =>
        universe ! InteractSecondary(self, nick, pos, getBeltBlock)
      case 3 =>
        universe ! InteractTertiary(self, nick, pos, getBeltBlock)
    }
  }

  def putInBelt(stack: Stack) {
    val inventory = data.inventory
    inventory.accept(stack) match {
      case Some((i, Stack.Empty)) =>
        update(i)
      case Some((i, stack)) =>
        update(i)
        println(s"The following stack was destroyed: $stack")
      case None =>
        println(s"The following stack was destroyed: $stack")
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
    case ActivateBeltItem(newActive) if(newActive >= 0 && newActive < 9) =>
      data = data.copy(active = newActive)
      sender ! BeltActiveUpdate(data.active.toString)
    case Action(pos, button) =>
      action(pos, button)
    case BlockRemoved(_, block) =>
      putInBelt(Stack.fromBlock(block))
    case UnableToPut(_, block) =>
      putInBelt(Stack.fromBlock(block))
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
      context.become(manageInventory(inventoryActor, view))
      client ! InventoryUpdate(addBelt(view))
    case bl: BlockList =>
      chunkLoader ! bl
  }

  val BeltView = InventoryView(0,4,1,9)

  def addBelt(view: View) = view.add(BeltView, data.inventory)

  def waitForSelectedStack(inventoryActor: ActorRef, view: View): Receive = {
    case ReceiveStack(stack) =>
      context.become(stackSelected(inventoryActor, view, stack))
      unstashAll()
    case CloseInventory =>
      context.become(ready)
      inventoryActor ! CloseInventory
      unstashAll()
    case _ =>
      stash()
  }

  def stackSelected(inventoryActor: ActorRef, view: View, stack: Stack): Receive = {
    if(stack.isEmpty) {
      client ! HeldStack(None)
    } else {
      client ! HeldStack(Some(stack))
    }

    val f: Receive = {
      case SelectItem(index) =>
        if(BeltView.contains(index)) {
          val beltIndex = BeltView.translate(index)
          val oldStack = data.inventory.stacks.get(beltIndex)
          oldStack.acceptStack(stack) match {
            case Some((newStack, left)) =>
              if(left != Stack.Empty) {
                context.become(stackSelected(inventoryActor, view, left))
              } else {
                context.become(manageInventory(inventoryActor, view))
              }
              update(data.inventory.withSlot(beltIndex, newStack))
            case None =>
              context.become(stackSelected(inventoryActor, view, oldStack))
              update(data.inventory.withSlot(beltIndex, stack))
          }
          client ! InventoryUpdate(addBelt(view))
        } else {
          context.become(manageInventory(inventoryActor, view))
          inventoryActor ! PutViewStack(stack, index)
        }
        unstashAll()
      case UpdateView(view) =>
        context.become(stackSelected(inventoryActor, view, stack))
        unstashAll()
        client ! InventoryUpdate(addBelt(view))
      case CloseInventory =>
        context.become(ready)
        inventoryActor ! CloseInventory
        unstashAll()
      case _ =>
        stash()
    }
    f
  }

  def manageInventory(inventoryActor: ActorRef, view: View): Receive = {
    client ! HeldStack(None)

    val f: Receive = {
      case SelectItem(index) =>
        if(BeltView.contains(index)) {
          val beltIndex = BeltView.translate(index)
          val stack = data.inventory.stacks.get(beltIndex)
          context.become(stackSelected(inventoryActor, view, stack))
          update(data.inventory.withoutSlot(beltIndex))
          client ! InventoryUpdate(addBelt(view))
        } else {
          context.become(waitForSelectedStack(inventoryActor, view))
          inventoryActor ! RemoveViewStack(index)
        }
      case UpdateView(view) =>
        context.become(manageInventory(inventoryActor, view))
        client ! InventoryUpdate(addBelt(view))
      case ReceiveStack(stack) =>
        context.become(stackSelected(inventoryActor, view, stack))
      case CloseInventory =>
        context.become(ready)
        inventoryActor ! CloseInventory
        unstashAll()
      case _ =>
        stash()
    }
    f
  }

}

object PlayerActor {
  case object StoreData
  case class PlayerMovement(pid: Int, pos: protocol.Position)
  case class PlayerLogout(pid: Int)
  case class PlayerInfo(pid: Int, nick: String, actor: ActorRef, pos: protocol.Position)
  case class PlayerNick(pid: Int, nick: String)
  case class ActivateBeltItem(activate: Int)
  case class BeltUpdate(items: java.util.List[Stack])
  case class BeltActiveUpdate(active: String)
  case class Action(pos: Option[Position], button: Int)
  case class SendInfo(to: ActorRef)
  case class IncreaseChunks(amount: Int)
  case class InventoryUpdate(view: View)
  case object Konstruct
  case class SelectItem(index: Int)
  case class HeldStack(held: Option[Stack])

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
      db ! SendBlocks(chunk)
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
