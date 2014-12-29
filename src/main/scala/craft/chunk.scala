package craft

import scala.concurrent.Future
import scala.collection.mutable.HashMap
import akka.actor.{ Actor, ActorRef, Props }
import akka.util.Timeout
import akka.pattern.{ ask, pipe }
import WorldActor.{ BlockList }

class ChunkActor(chunk: Chunk) extends Actor {
  import ChunkActor._
  import World.ChunkSize

  private def blockSeq: Seq[Byte] =
    for(
      x <- 0 until ChunkSize;
      y <- 0 until ChunkSize;
      z <- 0 until ChunkSize
    ) yield {
      if (chunk.k < 2 && y == 31) {
        (x + z).toByte
      } else {
        0.toByte
      }
    }

  private var blocks: Array[Byte] = Array( blockSeq :_*)

  def receive = {
    case SendBlocks(to) =>
      if(!blocks.isEmpty) {
        to ! BlockList(chunk, blocks)
      }
    case UpdateBlock(p, w) =>
      val local = p.local
      val copy = blocks.clone()
      val i = local.index
      val oldW = copy(i)
      copy(i) = w.toByte
      blocks = copy
      sender ! BlockUpdate(p, oldW.toInt, w)
  }

}

object ChunkActor {
  case class SendBlocks(to: ActorRef)
  case class UpdateBlock(pos: Position, w: Int)
  case class BlockUpdate(pos: Position, oldW: Int, newW: Int)
  def props(chunk: Chunk) = Props(classOf[ChunkActor], chunk)
}
