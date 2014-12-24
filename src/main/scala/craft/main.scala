package craft

import akka.actor.{ ActorSystem, Props }
import akka.io.IO

import craft.protocol.Server

object Main extends App {
  implicit val system = ActorSystem("craft-server")

  val world = system.actorOf(WorldActor.props(), "world")
  val server = system.actorOf(Server.props(world), "server")
}
