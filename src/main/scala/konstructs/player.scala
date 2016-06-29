package konstructs

import konstructs.plugin.toolsack.ToolSackActor

import akka.actor.{ Actor, Props, ActorRef, Stash, PoisonPill }

import konstructs.api._
import konstructs.api.messages._

case class Player(nick: String, password: String, position: protocol.Position,
  inventory: Inventory)

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
  import DbActor.{ BlockList, ChunkUpdate }

  val ns = "players"

  var data: Player = null
  var viewDistance = 10

  schedule(5000, StoreData)

  loadGson(nick)

  def receive = {
    case GsonLoaded(_, json) if json != null =>
      val newData = gson.fromJson(json, classOf[Player])
      if(newData.password == password) {
        data = newData
        if(data.inventory.isEmpty) {
          val inventoryBlock = Block.createWithId(ToolSackActor.BlockId)
          universe ! CreateInventory(inventoryBlock.getId, 16)
          val inventory = Inventory.createEmpty(9).withSlot(0, Stack.createFromBlock(inventoryBlock))
          data = data.copy(inventory = inventory)
        } else {
          data = data.copy(inventory = Inventory.convertPre0_1(data.inventory))
        }
        client ! PlayerInfo(pid, nick, self, data.position)
        context.become(sendBelt)
        unstashAll()
      } else {
        println(s"Stop player and client actors for ${newData.nick}, incorrect password provided.");
        client ! PoisonPill
        context.stop(self)
      }
    case GsonLoaded(_, _) =>
      val inventoryBlock = Block.createWithId(ToolSackActor.BlockId)
      universe ! CreateInventory(inventoryBlock.getId, 16)
      val inventory = Inventory.createEmpty(9).withSlot(0, Stack.createFromBlock(inventoryBlock))
      data = Player(nick, password, startingPosition, inventory)
      client ! PlayerInfo(pid, nick, self, data.position)
      context.become(sendBelt)
      unstashAll()
    case _ =>
      stash()
  }

  def update(position: protocol.Position) {
    data = data.copy(position = position)
  }

  def update(inventory: Inventory) {
    data = data.copy(inventory = inventory)
    client ! BeltUpdate(inventory.getStacks)
  }

  val random = new scala.util.Random

  def getBeltBlock(active: Int): Block = {
    val inventory = data.inventory
    val block = inventory.stackHead(active)
    if(block != null) {
      update(inventory.stackTail(active))
    }
    block
  }

  def actionPrimary(pos: Position, block: Block, active: Int): Receive = {
    case i: InteractResult =>
      if(i.position == pos) {
        if(i.block != null && block != null) {
          /* We got our tool block back, possibly damaged */
          val stack = data.inventory.getStack(active)
          update(data.inventory.withSlot(active, stack.replaceHead(i.block)))
        } else if(block != null && i.block == null) {
          /* We didn't get our tool block back, it was destroyed */
          update(data.inventory.stackTail(active))
        } else {
          /* We used a null tool, no need to update the stack (it's empty) */
        }
        context.become(ready orElse handleBasics)
        unstashAll()
      } else {
        throw new IllegalStateException(s"Invalid primary interaction result position: ${i.position}")
      }
  }

  def actionSecondary(pos: Position, block: Block, active: Int): Receive = {
    case i: InteractResult =>
      if(i.position == pos) {
        if(i.block == null && block != null) {
         /* We didn't get our block back, it was placed */
          update(data.inventory.stackTail(active))
        } else {
          /* We got our block back, couldn't be placed */
        }
        context.become(ready orElse handleBasics)
        unstashAll()
      } else {
        throw new IllegalStateException(s"Invalid secondary interaction result position: ${i.position}")
      }
  }

  def actionTertiary(pos: Position, block: Block, active: Int): Receive = {
    case i: InteractResult =>
      if(i.position == pos) {
        if(i.block == null && block != null) {
          /* We didn't get our block back, server needed it */
          update(data.inventory.stackTail(active))
        } else {
          /* We got our block back, wasn't needed */
        }
        context.become(ready orElse handleBasics)
        unstashAll()
      } else {
        throw new IllegalStateException(s"Invalid tertiary interaction result position: ${i.position}")
      }
  }

  def action(pos: Position, button: Int, active: Int): Receive = {
    val block = data.inventory.stackHead(active)
    button match {
      case 1 =>
        val responseHandler = actionPrimary(pos, block, active)
        universe ! InteractPrimary(self, nick, pos, block)
        responseHandler
      case 2 =>
        val responseHandler = actionSecondary(pos, block, active)
        universe ! InteractSecondary(self, nick, pos, block)
        responseHandler
      case 3 =>
        val responseHandler = actionTertiary(pos, block, active)
        universe ! InteractTertiary(self, nick, pos, block)
        responseHandler
    }
  }

  def putInBelt(stack: Stack) {
    if(stack != null) {
      val inventory = data.inventory
      val r = inventory.acceptPartOf(stack)
      if(r.getGiving == null) {
        update(r.getAccepting)
      } else {
        update(r.getAccepting)
        println(s"The following stack was destroyed: ${r.getGiving}")
      }
    }
  }

  def moveInBelt(from: Int, to: Int) {
    val inventory = data.inventory
    update(inventory.swapSlot(from, to))
  }

  override def postStop {
    if(data != null)
      storeGson(nick, gson.toJsonTree(data))
    universe ! PlayerLogout(pid)
  }

  def sendBelt: Receive = {
    /* Send belt*/
    client ! BeltUpdate(data.inventory.getStacks)
    /* Send time */
    client ! protocol.Time((new java.util.Date().getTime / 1000L) % 600)
    ready orElse handleBasics
  }

  def stashAll: Receive = {
    case _ =>
      stash()
  }

  def handleBasics: Receive = {
    case p: protocol.Position =>
      update(p)
      universe ! PlayerMovement(pid, data.position)
    case p: PlayerMovement =>
      client ! p
    case p: PlayerNick =>
      client ! p
    case SendInfo(to) =>
      to ! PlayerMovement(pid, data.position)
      to ! PlayerNick(pid, data.nick)
    case StoreData =>
      storeGson(nick, gson.toJsonTree(data))
    case l: PlayerLogout =>
      client ! l
    case protocol.Say(msg) =>
      universe ! Say(nick, msg)
    case s: Said =>
      client.forward(s)
    case bl: BlockList =>
      client ! bl
    case c: ChunkUpdate =>
      val distance = c.chunk.distance(ChunkPosition(data.position.toApiPosition))
      if(distance < 2) {
        /* Force update chunks nearby */
        client ! BlockList(c.chunk, c.data)
      } else if(distance < viewDistance){
        client ! protocol.ChunkUpdate(c.chunk.p, c.chunk.q, c.chunk.k)
      } else {
        /* Discard any chunk update that is too far away from the client */
      }
    case s: DbActor.SendBlocks =>
      db ! s
    case SetViewDistance(d) =>
      viewDistance = d
  }

  def ready: Receive = {
    case Action(pos, button, active) =>
      context.become(action(pos, button, active) orElse handleBasics orElse stashAll)
    case r: ReplaceBlockResult =>
      if(!r.getBlock.getType.equals(BlockTypeId.VACUUM)) {
        putInBelt(Stack.createFromBlock(r.getBlock))
      }
    case ReceiveStack(stack) =>
      putInBelt(stack)
    case ConnectView(inventoryActor, view) =>
      context.become(manageInventory(inventoryActor, view) orElse handleBasics orElse stashAll)
      client ! InventoryUpdate(addBelt(view))
  }

  val BeltView = new InventoryView(0,4,1,9)

  def addBelt(view: View) = view.add(BeltView, data.inventory)

  def stackSelected(inventoryActor: ActorRef, view: View, stack: Stack): Receive = {

    client ! HeldStack(stack)

    val f: Receive = {
      case SelectItem(index, button) =>
        if(BeltView.contains(index)) {
          val beltIndex = BeltView.translate(index)
          val oldStack = data.inventory.getStack(beltIndex)
          if(oldStack != null) {
            if(oldStack.acceptsPartOf(stack)) {
              val r = oldStack.acceptPartOf(stack)
              if(r.getGiving != null) {
                context.become(stackSelected(inventoryActor, view, r.getGiving) orElse handleBasics orElse stashAll)
              } else {
                context.become(manageInventory(inventoryActor, view) orElse handleBasics orElse stashAll)
              }
              update(data.inventory.withSlot(beltIndex, r.getAccepting))
            } else {
              context.become(stackSelected(inventoryActor, view, oldStack) orElse handleBasics orElse stashAll)
              if(stack != null)
                update(data.inventory.withSlot(beltIndex, stack))
            }
          } else {
            update(data.inventory.withSlot(beltIndex, stack))
            context.become(manageInventory(inventoryActor, view) orElse handleBasics orElse stashAll)
          }
          client ! InventoryUpdate(addBelt(view))
        } else {
          context.become(manageInventory(inventoryActor, view) orElse handleBasics orElse stashAll)
          inventoryActor ! PutViewStack(stack, index)
        }
        unstashAll()
      case UpdateView(view) =>
        context.become(stackSelected(inventoryActor, view, stack) orElse handleBasics orElse stashAll)
        unstashAll()
        client ! InventoryUpdate(addBelt(view))
      case CloseInventory =>
        context.become(ready orElse handleBasics)
        inventoryActor ! CloseInventory
        unstashAll()
    }
    f
  }

  def manageInventory(inventoryActor: ActorRef, view: View): Receive = {
    client ! HeldStack(null)

    val f: Receive = {
      case SelectItem(index, button) =>
        if(BeltView.contains(index)) {
          val beltIndex = BeltView.translate(index)
          val stack = data.inventory.getStack(beltIndex)
          if(stack != null) {
            button match {
              case 1 =>
                update(data.inventory.withoutSlot(beltIndex))
                context.become(stackSelected(inventoryActor, view, stack) orElse handleBasics orElse stashAll)
              case 3 =>
                val halfSize = stack.size() / 2
                update(data.inventory.withSlot(beltIndex, stack.drop(halfSize)))
                context.become(stackSelected(inventoryActor, view, stack.take(halfSize)) orElse handleBasics orElse stashAll)
              case 2 =>
                update(data.inventory.withSlot(beltIndex, stack.getTail()))
                context.become(stackSelected(inventoryActor, view, Stack.createFromBlock(stack.getHead())) orElse handleBasics orElse stashAll)
            }
            client ! InventoryUpdate(addBelt(view))
          }
        } else {
          val stackAmount: StackAmount = button match {
            case 1 => FullStack
            case 3 => HalfStack
            case 2 => OneBlock
            case i => throw new IllegalStateException(s"Undefined button: $i")
          }
          inventoryActor ! RemoveViewStack(index, stackAmount)
        }
      case UpdateView(view) =>
        context.become(manageInventory(inventoryActor, view) orElse handleBasics orElse stashAll)
        client ! InventoryUpdate(addBelt(view))
      case ReceiveStack(stack) =>
        if(stack != null)
          context.become(stackSelected(inventoryActor, view, stack) orElse handleBasics orElse stashAll)
      case CloseInventory =>
        context.become(ready orElse handleBasics)
        inventoryActor ! CloseInventory
        unstashAll()
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
  case class BeltUpdate(items: Array[Stack])
  case class Action(pos: Position, button: Int, active: Int)
  case class SendInfo(to: ActorRef)
  case class InventoryUpdate(view: View)
  case class SelectItem(index: Int, button: Int)
  case class HeldStack(held: Stack)
  case class SetViewDistance(distance: Int)

  def props(pid: Int, nick: String, password: String, client: ActorRef, db: ActorRef, universe: ActorRef, store: ActorRef, startingPosition: protocol.Position) = Props(classOf[PlayerActor], pid, nick, password, client, db, universe, store, startingPosition)

}
