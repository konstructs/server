package konstructs

import akka.actor.{ ActorSystem, Props }
import akka.io.IO

import konstructs.protocol.Server

object Main extends App {
  implicit val system = ActorSystem("main")

  val universe = system.actorOf(UniverseActor.props(), "universe")
  val server = system.actorOf(Server.props(universe), "server")
}
