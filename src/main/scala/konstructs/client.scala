package konstructs.protocol

import akka.actor.{ Actor, Props, ActorRef, Stash, PoisonPill }
import akka.io.{ Tcp, TcpPipelineHandler }
import akka.util.ByteString
import TcpPipelineHandler.{ Init, WithinActorContext }

import konstructs.{ Item, PlayerActor, UniverseActor, DbActor }
import konstructs.api.{ Say, Said }

class Client(init: Init[WithinActorContext, ByteString, ByteString], universe: ActorRef) extends Actor with Stash {
  import DbActor.BlockList
  import UniverseActor.CreatePlayer
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
    if(command.startsWith("P,")) {
      val floats = readData(_.toFloat, command.drop(2))
      player.actor ! Position(floats(0), floats(1), floats(2), floats(3), floats(4))
    } else if(command.startsWith("C,")) {
      val ints = readData(_.toInt, command.drop(2))
      player.actor ! IncreaseChunks(ints(0))
    } else if(command.startsWith("I")) {
      player.actor ! SendInventory
    } else if(command.startsWith("A,")) {
      val ints = readData(_.toInt, command.drop(2))
      player.actor ! ActivateInventoryItem(ints(0))
    } else if(command.startsWith("M,")) {
      val ints = readData(_.toInt, command.drop(2))
      player.actor ! Action(konstructs.Position(ints(0), ints(1), ints(2)), ints(3))
    } else if(command.startsWith("T,")) {
      val message = command.substring(2)
      player.actor ! Say(message)
    } else if(command.startsWith("K")) {
      player.actor ! Konstruct
    }
  }

  def receive = {
    case init.Event(data) =>
      val command = data.decodeString("ascii")
      if (command.startsWith(s"V,$Version,")) {
        val strings = readData(s => s, command.drop(4))

        universe ! CreatePlayer(strings(0), strings(1))
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
    case BlockList(chunk, data) =>
      sendBlocks(pipe, chunk, data.data)
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
    case PlayerLogout(pid) =>
      sendPlayerLogout(pipe, pid)
    case Said(text) =>
      sendSaid(pipe, text)
    case _: Tcp.ConnectionClosed =>
      player.actor ! PoisonPill
      context.stop(self)
  }

  def sendPlayerNick(pipe: ActorRef, pid: Int, nick: String) {
    send(pipe, s"N,$pid,$nick")
  }

  def sendSaid(pipe: ActorRef, msg: String) {
    send(pipe, s"T,$msg")
  }

  def sendPlayerLogout(pipe: ActorRef, pid: Int) {
    send(pipe, s"D,$pid")
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

  def sendBlocks(pipe: ActorRef, chunk: konstructs.ChunkPosition, blocks: Array[Byte]) {
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
  val Version = 3
  case object Setup
  def props(init: Init[WithinActorContext, ByteString, ByteString], universe: ActorRef) = Props(classOf[Client], init, universe)
}
