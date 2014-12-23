package craft

import akka.actor.{ Actor, Props, ActorRef }

class PlayerActor(client: ActorRef, world: ActorRef) extends Actor {
  def receive = {
    case b: Block =>
      println(b)
  }
}

object PlayerActor {
  def props(client: ActorRef, world: ActorRef) = Props(classOf[PlayerActor], client, world)
}
