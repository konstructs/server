package craft

import java.util.zip.{ Inflater, Deflater }
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
  private val buffer = new Array[Byte](256*256)

  def receive = {
    case SendBlocks(to) =>
      val size = deflate(blocks, buffer)
      to ! BlockList(chunk, buffer.slice(0, size))
    case UpdateBlock(p, w) =>
      val local = p.local
      val copy = blocks.clone
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

  def deflate(data: Array[Byte], out: Array[Byte]): Int = {
    val compresser = new Deflater()
    compresser.setInput(data)
    compresser.finish()
    val size = compresser.deflate(out)
    compresser.end()
    size
  }
  def inflate(data: Array[Byte], out: Array[Byte]): Int = {
    val decompresser = new Inflater()
    decompresser.setInput(data, 0, data.size)
    val size = decompresser.inflate(out)
    decompresser.end()
    size
  }
}
