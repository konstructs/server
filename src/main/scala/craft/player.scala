package craft

import scala.math.max

import akka.actor.{ Actor, Props, ActorRef }

class PlayerActor(client: ActorRef, world: ActorRef, startingPosition: protocol.Position) extends Actor {
  import PlayerActor._
  import WorldActor._
  import World.ChunkSize

  var position = startingPosition

  def chunk(p: Int, q: Int, v: Option[Int]) {
    val y = position.y.toInt
    val yMin = max(y - LoadYChunks * ChunkSize, 0)
    val yMax = y + LoadYChunks * ChunkSize

    for(y <- yMin until yMax by ChunkSize) {
      val k = y / ChunkSize
      world ! SendBlocks(sender, Chunk(p, q, k), v)
    }
  }

  def receive = {
    case b: protocol.Block =>
      println(s"Player: $b")
      world ! b
    case protocol.Chunk(p, q, v) =>
      chunk(p, q, v)
    case p: protocol.Position =>
      position = p
    case b: protocol.SendBlock =>
      client ! b
  }
}

object PlayerActor {
  val LoadYChunks = 5
  def props(client: ActorRef, world: ActorRef, startingPosition: protocol.Position) = Props(classOf[PlayerActor], client, world, startingPosition)
}
