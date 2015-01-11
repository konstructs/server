package craft

import scala.util.Random
import akka.actor.{ Actor, Props }

case class Bounds(pMin: Int, qMin: Int, kMin: Int, pMax: Int, qMax: Int, kMax: Int) {
  def contains(chunk: ChunkPosition) =
    chunk.p >= pMin && chunk.p <= pMax && chunk.q >= qMin && chunk.q <= qMax && chunk.k >= kMin && chunk.k <= kMax
}

class GeneratorActor extends Actor {
  import GeneratorActor._
  import World.ChunkSize

  val rnd = new Random

  val bounds = Bounds(-10, -10, -10, 10, 10, 10)

  val blocks = Set(2,4,5,6,8,11,12,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,52,53,54,55,56,57,58,59,60,61,62,63).toVector

  val compressionBuffer = new Array[Byte](ChunkSize * ChunkSize * ChunkSize)

  private def blockSeq(chunk: ChunkPosition): Seq[Byte] =
    for(
      x <- 0 until ChunkSize;
      y <- 0 until ChunkSize;
      z <- 0 until ChunkSize
    ) yield {
      if (chunk.k < 1) {
        44.toByte
      } else if(chunk.p == bounds.pMin || chunk.p == bounds.pMax || chunk.q == bounds.qMin || chunk.q == bounds.qMax) {
        if(chunk.k < 3)
          58.toByte
        else
          0.toByte
      } else if(chunk.k < 2 || (chunk.k < 3 && y < ChunkSize / 2)){
        if(rnd.nextInt(10) < 8)
          6.toByte
        else
          blocks(rnd.nextInt(blocks.size)).toByte
      } else if(chunk.k < 3) {
        7.toByte
      } else if(chunk.k < 4 && y == 0) {
        1.toByte
      } else {
        0.toByte
      }
    }

  private def blocks(chunk: ChunkPosition): Array[Byte] =
    compress.deflate(Array( blockSeq(chunk) :_*), compressionBuffer)

  def receive = {
    case Generate(chunk) =>
      if(bounds contains chunk)
        sender ! Generated(chunk, blocks(chunk))
  }
}

object GeneratorActor {
  case class Generate(chunk: ChunkPosition)
  case class Generated(chunk: ChunkPosition, blocks: Array[Byte])
  def props() = Props(classOf[GeneratorActor])
}
