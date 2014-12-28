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
        val builder = Seq.newBuilder[protocol.SendBlock]
        for(x <- 0 until ChunkSize; y <- 0 until ChunkSize; z <- 0 until ChunkSize) {
          val i = x + y * ChunkSize + z * ChunkSize * ChunkSize
          val b = blocks(i)
          if(b != null) {
            builder += protocol.SendBlock(chunk.p, chunk.q, chunk.p * ChunkSize + x, chunk.k * ChunkSize + y, chunk.q * ChunkSize + z, b.toInt)
          }
        }
        to ! BlockList(builder.result())
      }
  }

}

object ChunkActor {
  case class SendBlocks(to: ActorRef)
  def props(chunk: Chunk) = Props(classOf[ChunkActor], chunk)
}
