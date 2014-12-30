package craft

import java.util.zip.{ Inflater, Deflater }
import scala.concurrent.Future
import scala.collection.mutable.HashMap
import akka.actor.{ Actor, ActorRef, Props }
import akka.util.Timeout
import akka.pattern.{ ask, pipe }
import WorldActor.{ BlockList, UpdateBlock }
import World.ChunkSize

class ChunkActor(chunk: Chunk) extends Actor {
  import ChunkActor._

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

  private var blocks: Array[Byte] =  deflate(Array( blockSeq :_*))
  def receive = {
    case SendBlocks(to) =>
      to ! BlockList(chunk,blocks)
    case UpdateBlock(from, p, w) =>
      val local = p.local
      val unpacked =  inflate(blocks)
      val i = local.index
      val oldW = unpacked(i)
      unpacked(i) = w.toByte
      blocks = deflate(unpacked)
      sender ! BlockUpdate(p, oldW.toInt, w)
  }

}

object ChunkActor {
  case class SendBlocks(to: ActorRef)
  case class BlockUpdate(pos: Position, oldW: Int, newW: Int)
  def props(chunk: Chunk) = Props(classOf[ChunkActor], chunk)

  def deflate(data: Array[Byte]): Array[Byte] = {
    val out = new Array[Byte](ChunkSize * ChunkSize * ChunkSize)
    val compresser = new Deflater()
    compresser.setInput(data)
    compresser.finish()
    val size = compresser.deflate(out)
    compresser.end()
    out.slice(0, size)
  }

  def inflate(data: Array[Byte]): Array[Byte] = {
    val out = new Array[Byte](ChunkSize * ChunkSize * ChunkSize)
    val decompresser = new Inflater()
    decompresser.setInput(data, 0, data.size)
    val size = decompresser.inflate(out)
    decompresser.end()
    out
  }
}
