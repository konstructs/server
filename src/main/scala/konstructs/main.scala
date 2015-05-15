package konstructs

import akka.actor.{ ActorSystem, Props }
import akka.io.IO

import konstructs.protocol.Server

object Main extends App {
  implicit val system = ActorSystem("main")

  val world = system.actorOf(WorldActor.props(), "world")
  val server = system.actorOf(Server.props(world), "server")
}
