package konstructs

import akka.actor.{ ActorSystem, Props }
import akka.io.IO

import konstructs.protocol.Server

object Main extends App {
  implicit val system = ActorSystem("main")

  val db = system.actorOf(DbActor.props(), "db")
  val server = system.actorOf(Server.props(db), "server")
}
