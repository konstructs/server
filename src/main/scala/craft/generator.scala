package craft

import akka.actor.{ Actor, Props }

class GeneratorActor extends Actor {
  import GeneratorActor._
  import World.ChunkSize

  val compressionBuffer = new Array[Byte](ChunkSize * ChunkSize * ChunkSize)

  private def blockSeq(chunk: ChunkPosition): Seq[Byte] =
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

  private def blocks(chunk: ChunkPosition): Array[Byte] =
    compress.deflate(Array( blockSeq(chunk) :_*), compressionBuffer)

  def receive = {
    case Generate(chunk) =>
      sender ! Generated(chunk, blocks(chunk))
  }
}

object GeneratorActor {
  case class Generate(chunk: ChunkPosition)
  case class Generated(chunk: ChunkPosition, blocks: Array[Byte])
  def props() = Props(classOf[GeneratorActor])
}
