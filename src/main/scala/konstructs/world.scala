package konstructs

import scala.util.Random
import akka.actor.{ Actor, Props }

class FlatWorldActor(end: Position) extends Actor {
  import World._
  import GeneratorActor.Generated
  import Db.ChunkSize

  val rnd = new Random

  val compressionBuffer = new Array[Byte](ChunkSize * ChunkSize * ChunkSize)

  val size = 1024

  val diamond0 = new DiamondSquareHeightMap(0.1f, size, Position(0, 0, 0), EmptyHeightMap)

  val diamond1 = new DiamondSquareHeightMap(0.2f, size, Position(0, 0, size), diamond0)

  val diamond2 = new DiamondSquareHeightMap(0.3f, size, Position(0, 0, 2 * size), diamond0 orElse diamond1)

  val diamond3 = new DiamondSquareHeightMap(0.4f, size, Position(size, 0, 0), diamond0 orElse diamond1 orElse diamond2)

  val diamond4 = new DiamondSquareHeightMap(0.5f, size, Position(size, 0, size), diamond0 orElse diamond1 orElse diamond2 orElse diamond3)

  val diamond5 = new DiamondSquareHeightMap(0.6f, size, Position(size, 0, 2 * size), diamond0 orElse diamond1 orElse diamond2 orElse diamond3 orElse diamond4)

  val diamond6 = new DiamondSquareHeightMap(0.7f, size, Position(2 * size, 0, 0), diamond0 orElse diamond1 orElse diamond2 orElse diamond3 orElse diamond4 orElse diamond5)

  val diamond7 = new DiamondSquareHeightMap(0.8f, size, Position(2 * size, 0, size), diamond0 orElse diamond1 orElse diamond2 orElse diamond3 orElse diamond4 orElse diamond5 orElse diamond6)

  val diamond8 = new DiamondSquareHeightMap(0.9f, size, Position(2* size, 0, 2* size), diamond0 orElse diamond1 orElse diamond2 orElse diamond3 orElse diamond4 orElse diamond5 orElse diamond6 orElse diamond7)


  val map = diamond0 orElse diamond1 orElse diamond2 orElse diamond3 orElse diamond4 orElse diamond5 orElse diamond6 orElse diamond7 orElse diamond8
  val random = new scala.util.Random
  private def blockSeq(chunk: ChunkPosition): Seq[Byte] = {
    for(
      z <- 0 until ChunkSize;
      y <- 0 until ChunkSize;
      x <- 0 until ChunkSize
    ) yield {
      val global = Position(chunk, x, y, z)
      val height = map(global) + 32
      if(global.y < height) {
        if(global.y < 10) {
          16.toByte
        } else if(global.y < 12 ) {
          2.toByte
        } else if(global.y == height - 1) {
          if(global.y + random.nextInt(10) - 5 < 64)
            1.toByte
          else {
            if(global.y + random.nextInt(4) - 2 < 128) {
              9.toByte
            } else {
              14.toByte
            }
          }
        } else if(global.y > height - 10) {
          if(global.y >= 128) {
            14.toByte
          } else {
            7.toByte
          }
        } else {
          6.toByte
        }
      } else if (global.y < 10) {
        16.toByte
      } else if(global.y < height + 1 && global.y < 40 && global.y > 12 && random.nextInt(25) == 1) {
        if(random.nextInt(8) == 1) { // One in 8 grass is a random flower
          (18 + random.nextInt(4)).toByte
        } else {
          17.toByte
        }
      } else {
        0.toByte
      }
    }
  }

  private def blocks(chunk: ChunkPosition): Chunk =
    Chunk(compress.deflate(Array( blockSeq(chunk) :_*), compressionBuffer))

  def receive = {
    case Generate(real, chunk) =>
      sender ! Generated(real, blocks(chunk))
  }

}

object FlatWorldActor {
  def props(end: Position) = Props(classOf[FlatWorldActor], end)
}

object World {
  case class Generate(real: ChunkPosition, position: ChunkPosition)
}
