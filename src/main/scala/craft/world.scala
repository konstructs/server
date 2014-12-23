package craft

import akka.actor.{ Actor, ActorRef, Props }

import scala.collection.mutable

case class Player(pid: Int, actor: ActorRef)

class WorldActor extends Actor {
  import WorldActor._
  private var nextPid = 0

  def receive = {
    case CreatePlayer =>
      val player = context.actorOf(PlayerActor.props(sender, self), s"player-$nextPid")
      val p = Player(nextPid, player)
      nextPid = nextPid + 1
      sender ! p
    case c: Chunk =>
      println(c)
    case p: Position =>
      println(p)
  }
}

object WorldActor {
  case object CreatePlayer
  def props() = Props(classOf[WorldActor])
}
