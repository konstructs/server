package craft

import scala.concurrent.Future
import scala.collection.mutable.HashMap
import akka.actor.{ Actor, ActorRef, Props }
import akka.util.Timeout
import akka.pattern.{ ask, pipe }
import WorldActor.{ Chunk, ChunkSize, BlockList }

class ChunkActor(chunk: Chunk) extends Actor {
  import ChunkActor._

  private def blockSeq: Seq[Byte] =
    if (chunk.k < 2 ) {
      (0 until ChunkSize * ChunkSize * ChunkSize).map { i =>
        1.toByte
      }
    } else {
      Seq()
    }

  private val blocks: Array[Byte] = Array( blockSeq :_*)

  def receive = {
    case SendBlocks(to) =>
      if(!blocks.isEmpty) {
        to ! BlockList(chunk, blocks)
      }
  }

}

object ChunkActor {
  case class SendBlocks(to: ActorRef)
  def props(chunk: Chunk) = Props(classOf[ChunkActor], chunk)
}
