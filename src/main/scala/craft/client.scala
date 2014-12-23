package craft

import akka.actor.{ Actor, Props, ActorRef }
import akka.io.{ Tcp, TcpPipelineHandler }
import TcpPipelineHandler.{ Init, WithinActorContext }

class Client(init: Init[WithinActorContext, String, String], world: ActorRef) extends Actor {

  private def readData[T](conv: String => T, data: String): List[T] = {
    val comma = data.indexOf(',')
    if(comma > 0) {
      val i = conv(data.take(comma))
      i :: readData(conv, data.drop(comma + 1))
    } else {
      val newline = data.indexOf('\n')
      val i = conv(data.take(newline))
      i :: Nil
    }
  }

  def handle(player: Player, command: String) = {
    print(s"RECV: $command")
    if(command.startsWith("C,")) {
      val ints = readData(_.toInt, command.drop(2))
      val key = ints(2)
      world ! Chunk(ints(0), ints(1), if(key == 0) None else Some(key))
    } else if(command.startsWith("B,")) {
      val ints = readData(_.toInt, command.drop(2))
      player.actor ! Block(ints(0), ints(1), ints(2), ints(3))
    } else if(command.startsWith("P,")) {
      val floats = readData(_.toFloat, command.drop(2))
      world ! Position(floats(0), floats(1), floats(2), floats(3), floats(4))
    }
  }

  def receive = {
    case init.Event(command) =>
      if (command == "V,1\n") {
        world ! WorldActor.CreatePlayer
        context.become(waitForPlayer(sender))
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
    case _: Tcp.ConnectionClosed =>
      context.stop(self)
  }

  def send(pipe: ActorRef, msg: String) {
    println(s"SEND: $msg")
    pipe ! init.Command(s"$msg\n")
  }
}

object Client {
  case object Setup
  def props(init: Init[WithinActorContext, String, String], world: ActorRef) = Props(classOf[Client], init, world)
}
