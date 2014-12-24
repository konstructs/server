package craft

import akka.actor.{ Actor, ActorRef, Props }
import WorldActor.{ Chunk, ChunkSize }

class ChunkActor(chunk: Chunk) extends Actor {
  import ChunkActor._

  def block(x: Int)(z: Int) = protocol.SendBlock(chunk.p, chunk.q, chunk.p * ChunkSize + x, 31, chunk.q * ChunkSize + z, 1)

  val blocks = if (chunk.k == 0) {
    (0 until ChunkSize).flatMap { x =>
      (0 until ChunkSize).map(block(x))
    }
  } else {
    Seq()
  }

  def receive = {
    case SendBlocks(to) =>
      blocks foreach { block =>
        to ! block
      }
  }
}

object ChunkActor {
  case class SendBlocks(to: ActorRef)

  def props(chunk: Chunk) = Props(classOf[ChunkActor], chunk)
}
