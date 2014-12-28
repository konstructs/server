package craft.protocol

import craft.{ Player, WorldActor }

import akka.actor.{ Actor, Props, ActorRef }
import akka.io.{ Tcp, TcpPipelineHandler }
import akka.util.ByteString
import TcpPipelineHandler.{ Init, WithinActorContext }

class Client(init: Init[WithinActorContext, ByteString, ByteString], world: ActorRef) extends Actor {
  import WorldActor.{ BlockList, CreatePlayer }
  import Client._
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

  def handle(player: Player, data: ByteString) = {
    val command = data.decodeString("ascii")
    println(s"RECV: |$command|")
    if(command.startsWith("C,")) {
      val ints = readData(_.toInt, command.drop(2))
      val key = ints(2)
      player.actor ! Chunk(ints(0), ints(1), if(key == 0) None else Some(key))
    } else if(command.startsWith("B,")) {
      val ints = readData(_.toInt, command.drop(2))
      player.actor ! Block(ints(0), ints(1), ints(2), ints(3))
    } else if(command.startsWith("P,")) {
      val floats = readData(_.toFloat, command.drop(2))
      player.actor ! Position(floats(0), floats(1), floats(2), floats(3), floats(4))
    }
  }

  def receive = {
    case init.Event(data) =>
      val command = data.decodeString("ascii")
      if (command == "V,2") {
        world ! CreatePlayer
        context.become(waitForPlayer(sender))
      } else {
        context.stop(self)
      }
    case _: Tcp.ConnectionClosed =>
      context.stop(self)
  }

  def waitForPlayer(pipe: ActorRef): Receive = {
    case p: Player =>
      send(pipe, s"U,${p.pid},0,0,0,0,0")
      context.become(ready(pipe, p))
  }

  def ready(pipe: ActorRef, player: Player): Receive = {
    case init.Event(command) =>
      handle(player, command)
    case BlockList(chunk, blocks) =>
      sendBlocks(pipe, chunk, blocks)
    case _: Tcp.ConnectionClosed =>
      context.stop(self)
  }

  def sendBlocks(pipe: ActorRef, chunk: WorldActor.Chunk, blocks: Array[Byte]) {
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
