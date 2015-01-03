package craft.protocol

import craft.{ Item, PlayerActor, WorldActor }

import akka.actor.{ Actor, Props, ActorRef, Stash, PoisonPill }
import akka.io.{ Tcp, TcpPipelineHandler }
import akka.util.ByteString
import TcpPipelineHandler.{ Init, WithinActorContext }

class Client(init: Init[WithinActorContext, ByteString, ByteString], world: ActorRef) extends Actor with Stash {
  import WorldActor.{ BlockList, CreatePlayer }
  import Client._
  import PlayerActor._
  implicit val bo = java.nio.ByteOrder.BIG_ENDIAN


  private def readData[T](conv: String => T, data: String): List[T] = {
    val comma = data.indexOf(',')
    if(comma > 0) {
      val i = conv(data.take(comma))
      i :: readData(conv, data.drop(comma + 1))
    } else {
      val i = conv(data)
      i :: Nil
    }
  }

  def handle(player: PlayerInfo, data: ByteString) = {
    val command = data.decodeString("ascii")
    println(s"RECV: |$command|")
    if(command.startsWith("C,")) {
      val ints = readData(_.toInt, command.drop(2))
      val key = ints(2)
      player.actor ! Chunk(ints(0), ints(1), if(key == 0) None else Some(key))
    } else if(command.startsWith("P,")) {
      val floats = readData(_.toFloat, command.drop(2))
      player.actor ! Position(floats(0), floats(1), floats(2), floats(3), floats(4))
    } else if(command.startsWith("I")) {
      player.actor ! SendInventory
    } else if(command.startsWith("A,")) {
      val ints = readData(_.toInt, command.drop(2))
      player.actor ! ActivateInventoryItem(ints(0))
    } else if(command.startsWith("M,")) {
      val ints = readData(_.toInt, command.drop(2))
      player.actor ! Action(craft.Position(ints(0), ints(1), ints(2)), ints(3))
    }
  }

  def receive = {
    case init.Event(data) =>
      val command = data.decodeString("ascii")
      if (command.startsWith("V,2,")) {
        val strings = readData(s => s, command.drop(4))

        world ! CreatePlayer(strings(0))
        context.become(waitForPlayer(sender))
      } else {
        context.stop(self)
      }
    case _: Tcp.ConnectionClosed =>
      context.stop(self)
  }

  def waitForPlayer(pipe: ActorRef): Receive = {
    case p: PlayerInfo =>
      send(pipe, s"U,${p.pid},${p.pos.x},${p.pos.y},${p.pos.z},${p.pos.rx},${p.pos.ry}")
      sendPlayerNick(pipe, p.pid, p.nick)
      unstashAll()
      context.become(ready(pipe, p))
    case init.Event(data) =>
      stash()
  }

  def ready(pipe: ActorRef, player: PlayerInfo): Receive = {
    case init.Event(command) =>
      handle(player, command)
    case BlockList(chunk, blocks) =>
      sendBlocks(pipe, chunk, blocks)
    case b: SendBlock =>
      sendBlock(pipe, b)
    case InventoryUpdate(items) =>
      sendInventory(pipe, items)
    case InventoryActiveUpdate(active) =>
      sendInventoryActive(pipe, active)
    case p: PlayerMovement =>
      sendPlayerMovement(pipe, p)
    case PlayerNick(pid, nick) =>
      sendPlayerNick(pipe, pid, nick)
    case _: Tcp.ConnectionClosed =>
      player.actor ! PoisonPill
      context.stop(self)
  }

  def sendPlayerNick(pipe: ActorRef, pid: Int, nick: String) {
    send(pipe, s"N,$pid,$nick")
  }

  def sendPlayerMovement(pipe: ActorRef, p: PlayerMovement) {
    send(pipe, s"P,${p.pid},${p.pos.x},${p.pos.y},${p.pos.z},${p.pos.rx},${p.pos.ry}")
  }

  def sendInventory(pipe: ActorRef, items: Map[String, Item]) {
    for((p, i) <- items) {
      send(pipe, s"I,${p},${i.amount},${i.w}")
    }
  }

  def sendInventoryActive(pipe: ActorRef, active: Int) {
    send(pipe, s"A,${active}")
  }

  def sendBlock(pipe: ActorRef, b: SendBlock) {
    send(pipe, s"B,${b.p},${b.q},${b.x},${b.y},${b.z},${b.w}")
  }

  def sendBlocks(pipe: ActorRef, chunk: craft.ChunkPosition, blocks: Array[Byte]) {
    val data = ByteString
      .newBuilder
      .putByte(C)
      .putInt(chunk.p)
      .putInt(chunk.q)
      .putInt(chunk.k)
      .putBytes(blocks)
      .result
    send(pipe, data)
  }

  def send(pipe: ActorRef, msg: ByteString) {
    pipe ! init.Command(msg)
  }

  def send(pipe: ActorRef, msg: String) {
    send(pipe,ByteString(msg, "ascii"))
  }
}

object Client {
  val C = 'C'.toByte
  val B = 'B'.toByte
  val V = 'V'.toByte
  val P = 'P'.toByte

  case object Setup
  def props(init: Init[WithinActorContext, ByteString, ByteString], world: ActorRef) = Props(classOf[Client], init, world)
}
