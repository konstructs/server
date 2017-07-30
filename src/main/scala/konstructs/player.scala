package konstructs

import java.util.concurrent.TimeUnit

import scala.concurrent.duration.Duration

import konstructs.plugin.toolsack.ToolSackActor

import akka.actor.{Actor, Props, ActorRef, Stash, PoisonPill}

import konstructs.api._
import konstructs.api.messages._
import konstructs.shard.ChunkPosition

case class Player(nick: String, password: String, position: protocol.Position, inventory: Inventory)

class PlayerActor(
    pid: Int,
    nick: String,
    password: String,
    client: ActorRef,
    db: ActorRef,
    universe: ActorRef,
    override val jsonStorage: ActorRef,
    startingPosition: protocol.Position
) extends Actor
    with Stash
    with utils.Scheduled
    with JsonStorage {

  import PlayerActor._
  import DbActor.{BlockList, ChunkUpdate}

  val Stone = BlockTypeId.fromString("org/konstructs/stone")
  val ns = "players"

  var data: Player = null
  var viewDistance = 10

  schedule(5000, StoreData)

  loadGson(nick)

  def receive = {
    case GsonLoaded(_, json) if json != null =>
      val newData = gson.fromJson(json, classOf[Player])
      if (newData.password == password) {
        data = newData
        if (data.inventory.isEmpty) {
          val inventoryBlock = Block.createWithId(ToolSackActor.BlockId)
          universe ! new CreateInventory(inventoryBlock.getId, 16)
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
        context.stop(self)
      }
    case GsonLoaded(_, _) =>
      val inventoryBlock = Block.create(ToolSackActor.BlockId)
      val inventory = Inventory
        .createEmpty(9)
        .withSlot(8, Stack.createFromBlock(inventoryBlock))
        .withSlot(7, Stack.createOfSize(Stone, 1))
        .withSlot(6, Stack.createOfSize(Stone, 1))
      data = Player(nick, password, startingPosition, inventory)
      client ! PlayerInfo(pid, nick, self, data.position)
      context.become(sendBelt)
      /* Send welcome message */
      sendWelcomeText(1, s"Welcome $nick!")
      sendWelcomeText(3, "The yellow item on the rightmost position of your belt is your tool sack.")
      sendWelcomeText(3, "You can use your tool sack to craft different type of blocks and tools.")
      sendWelcomeText(3, "It can also be used as a storage space.")
      sendWelcomeText(
        4,
        "Activate it by first selecting it (press 9 or use the scroll wheel) and then press E or the middle mouse button.")
      sendWelcomeText(
        4,
        "By placing the two stone blocks in the 2x2 crafting area of the tool sack you can craft a hand axe which is a simple tool.")
      sendWelcomeText(
        4,
        "Use it to destroy other blocks and explore what can be crafted by placing other blocks into the 2x2 crafting area.")
      sendWelcomeText(0, "Happy playing!")
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
    if (block != null) {
      update(inventory.stackTail(active))
    }
    block
  }

  def actionPrimary(pos: Position, block: Block, active: Int): Receive = {
    case i: InteractResult =>
      if (i.getPosition == pos) {
        if (i.getBlock != null && block != null) {
          /* We got our tool block back, possibly damaged */
          val stack = data.inventory.getStack(active)
          update(data.inventory.withSlot(active, stack.replaceHead(i.getBlock)))
        } else if (block != null && i.getBlock == null) {
          /* We didn't get our tool block back, it was destroyed */
          update(data.inventory.stackTail(active))
        } else {
          /* We used a null tool, no need to update the stack (it's empty) */
        }
        context.become(ready orElse handleBasics)
        unstashAll()
      } else {
        throw new IllegalStateException(s"Invalid primary interaction result position: ${i.getPosition}")
      }
  }

  def actionSecondary(pos: Position, block: Block, active: Int): Receive = {
    case i: InteractResult =>
      if (i.getPosition == pos) {
        if (i.getBlock == null && block != null) {
          /* We didn't get our block back, it was placed */
          update(data.inventory.stackTail(active))
        } else if (i.getBlock != null) {
          /* We got our block back, it couldn't be placed, possibly updated */
          val stack = data.inventory.getStack(active)
          update(data.inventory.withSlot(active, stack.replaceHead(i.getBlock)))
        }
        context.become(ready orElse handleBasics)
        unstashAll()
      } else {
        throw new IllegalStateException(s"Invalid secondary interaction result position: ${i.getPosition}")
      }
  }

  def actionTertiary(pos: Position, block: Block, active: Int): Receive = {
    case i: InteractResult =>
      if (i.getPosition == pos) {
        if (i.getBlock == null && block != null) {
          /* We didn't get our block back, server needed it */
          update(data.inventory.stackTail(active))
        } else if (i.getBlock != null) {
          /* We got our block back, possibly updated */
          val stack = data.inventory.getStack(active)
          update(data.inventory.withSlot(active, stack.replaceHead(i.getBlock)))
        }
        context.become(ready orElse handleBasics)
        unstashAll()
      } else {
        throw new IllegalStateException(s"Invalid tertiary interaction result position: ${i.getPosition}")
      }
  }

  def action(pos: Position, orientation: Orientation, button: Int, active: Int): Receive = {
    val block = data.inventory.stackHead(active)
    button match {
      case 1 =>
        val responseHandler = actionPrimary(pos, block, active)
        universe ! new InteractPrimary(self, nick, pos, orientation, block)
        responseHandler
      case 2 =>
        val responseHandler = actionSecondary(pos, block, active)
        universe ! new InteractSecondary(self, nick, pos, orientation, block)
        responseHandler
      case 3 =>
        val responseHandler = actionTertiary(pos, block, active)
        universe ! new InteractTertiary(self, nick, pos, orientation, block, null, false)
        responseHandler
    }
  }

  def putInBelt(stack: Stack) {
    if (stack != null && stack.getTypeId != BlockTypeId.VACUUM) {
      val inventory = data.inventory
      val r = inventory.acceptPartOf(stack)
      if (r.getGiving == null) {
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
    if (data != null)
      storeGson(nick, gson.toJsonTree(data))
    universe ! PlayerLogout(pid)
    client ! PoisonPill
  }

  var delay = 0

  def sendWelcomeText(d: Int, text: String) {
    context.system.scheduler
      .scheduleOnce(Duration.create(delay, TimeUnit.SECONDS), client, new Said(text))(context.dispatcher)
    delay = delay + d
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
      universe ! new Say(nick, msg)
    case s: Said =>
      client.forward(s)
    case bl: BlockList =>
      client ! bl
    case c: ChunkUpdate =>
      val distance = c.chunk.distance(ChunkPosition(data.position.toApiPosition))
      if (distance < 2) {
        /* Force update chunks nearby */
        client ! BlockList(c.chunk, c.data)
      } else if (distance < viewDistance) {
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
    case Action(pos, orientation, button, active) =>
      context.become(action(pos, orientation, button, active) orElse handleBasics orElse stashAll)
    case r: ReplaceBlockResult =>
      if (!r.getBlock.getType.equals(BlockTypeId.VACUUM)) {
        putInBelt(Stack.createFromBlock(r.getBlock))
      }
    case r: ReceiveStack =>
      putInBelt(r.getStack)
    case c: ConnectView =>
      context.become(manageInventory(c.getManager, c.getView) orElse handleBasics orElse stashAll)
      client ! InventoryUpdate(addBelt(c.getView))
  }

  val BeltView = new InventoryView(0, 4, 1, 9)

  def addBelt(view: View) = view.add(BeltView, data.inventory)

  def stackSelected(inventoryActor: ActorRef, view: View, stack: Stack): Receive = {

    client ! HeldStack(stack)

    val f: Receive = {
      case SelectItem(index, button) =>
        if (BeltView.contains(index)) {
          val beltIndex = BeltView.translate(index)
          val oldStack = data.inventory.getStack(beltIndex)
          if (oldStack != null) {
            if (oldStack.canAcceptPartOf(stack)) {
              val r = oldStack.acceptPartOf(stack)
              if (r.getGiving != null) {
                context.become(stackSelected(inventoryActor, view, r.getGiving) orElse handleBasics orElse stashAll)
              } else {
                context.become(manageInventory(inventoryActor, view) orElse handleBasics orElse stashAll)
              }
              update(data.inventory.withSlot(beltIndex, r.getAccepting))
            } else {
              context.become(stackSelected(inventoryActor, view, oldStack) orElse handleBasics orElse stashAll)
              if (stack != null)
                update(data.inventory.withSlot(beltIndex, stack))
            }
          } else {
            update(data.inventory.withSlot(beltIndex, stack))
            context.become(manageInventory(inventoryActor, view) orElse handleBasics orElse stashAll)
          }
          client ! InventoryUpdate(addBelt(view))
        } else {
          context.become(manageInventory(inventoryActor, view) orElse handleBasics orElse stashAll)
          inventoryActor ! new PutViewStack(stack, index)
        }
        unstashAll()
      case u: UpdateView =>
        context.become(stackSelected(inventoryActor, u.getView, stack) orElse handleBasics orElse stashAll)
        unstashAll()
        client ! InventoryUpdate(addBelt(u.getView))
      case CloseView.MESSAGE =>
        context.become(ready orElse handleBasics)
        inventoryActor ! CloseView.MESSAGE
        unstashAll()
    }
    f
  }

  def stackAmount(button: Int): StackAmount = button match {
    case 1 => StackAmount.ALL
    case 3 => StackAmount.HALF
    case 2 => StackAmount.ONE
    case i => throw new IllegalStateException(s"Undefined button: $i")
  }

  def manageInventory(inventoryActor: ActorRef, view: View): Receive = {
    client ! HeldStack(null)

    val f: Receive = {
      case SelectItem(index, button) =>
        val amount = stackAmount(button)
        if (BeltView.contains(index)) {
          val beltIndex = BeltView.translate(index)
          val stack = data.inventory.getStack(beltIndex)
          if (stack != null) {
            update(data.inventory.withSlot(beltIndex, stack.drop(amount)))
            context.become(stackSelected(inventoryActor, view, stack.take(amount)) orElse handleBasics orElse stashAll)
            client ! InventoryUpdate(addBelt(view))
          }
        } else {
          inventoryActor ! new RemoveViewStack(amount, index)
        }
      case u: UpdateView =>
        context.become(manageInventory(inventoryActor, u.getView) orElse handleBasics orElse stashAll)
        client ! InventoryUpdate(addBelt(u.getView))
      case r: ReceiveStack =>
        if (r.getStack != null)
          context.become(stackSelected(inventoryActor, view, r.getStack) orElse handleBasics orElse stashAll)
      case CloseView.MESSAGE =>
        context.become(ready orElse handleBasics)
        inventoryActor ! CloseView.MESSAGE
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
  case class Action(pos: Position, orientation: Orientation, button: Int, active: Int)
  case class SendInfo(to: ActorRef)
  case class InventoryUpdate(view: View)
  case class SelectItem(index: Int, button: Int)
  case class HeldStack(held: Stack)
  case class SetViewDistance(distance: Int)

  def props(pid: Int,
            nick: String,
            password: String,
            client: ActorRef,
            db: ActorRef,
            universe: ActorRef,
            store: ActorRef,
            startingPosition: protocol.Position) =
    Props(classOf[PlayerActor], pid, nick, password, client, db, universe, store, startingPosition)

}
