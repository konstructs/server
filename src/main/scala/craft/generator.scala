package craft

import java.io.File
import scala.util.Random
import akka.actor.{ Actor, Props }
import com.sksamuel.scrimage.Image

case class Bounds(pMin: Int, qMin: Int, kMin: Int, pMax: Int, qMax: Int, kMax: Int) {
  def contains(chunk: ChunkPosition) =
    chunk.p >= pMin && chunk.p <= pMax && chunk.q >= qMin && chunk.q <= qMax && chunk.k >= kMin && chunk.k <= kMax
}

class GeneratorActor extends Actor {
  import GeneratorActor._
  import World.ChunkSize

  val rnd = new Random

  val compressionBuffer = new Array[Byte](ChunkSize * ChunkSize * ChunkSize)

  val size = 1024

  val diamond0 = new DiamondSquareHeightMap(0.1f, size, Position(-size, 0, -size), EmptyHeightMap)

  val diamond1 = new DiamondSquareHeightMap(0.2f, size, Position(-size, 0, 0), diamond0)

  val diamond2 = new DiamondSquareHeightMap(0.3f, size, Position(-size, 0, size), diamond0 orElse diamond1)

  val diamond3 = new DiamondSquareHeightMap(0.4f, size, Position(0, 0, -size), diamond0 orElse diamond1 orElse diamond2)

  val diamond4 = new DiamondSquareHeightMap(0.5f, size, Position(0, 0, 0), diamond0 orElse diamond1 orElse diamond2 orElse diamond3)

  val diamond5 = new DiamondSquareHeightMap(0.6f, size, Position(0, 0, size), diamond0 orElse diamond1 orElse diamond2 orElse diamond3 orElse diamond4)

  val diamond6 = new DiamondSquareHeightMap(0.7f, size, Position(size, 0, -size), diamond0 orElse diamond1 orElse diamond2 orElse diamond3 orElse diamond4 orElse diamond5)

  val diamond7 = new DiamondSquareHeightMap(0.8f, size, Position(size, 0, 0), diamond0 orElse diamond1 orElse diamond2 orElse diamond3 orElse diamond4 orElse diamond5 orElse diamond6)

  val diamond8 = new DiamondSquareHeightMap(0.9f, size, Position(size, 0, size), diamond0 orElse diamond1 orElse diamond2 orElse diamond3 orElse diamond4 orElse diamond5 orElse diamond6 orElse diamond7)


  val map = diamond0 orElse diamond1 orElse diamond2 orElse diamond3 orElse diamond4 orElse diamond5 orElse diamond6 orElse diamond7 orElse diamond8 orElse FlatHeightMap(12)
  val random = new scala.util.Random
  private def blockSeq(chunk: ChunkPosition): Seq[Byte] = {
    for(
      z <- 0 until ChunkSize;
      y <- 0 until ChunkSize;
      x <- 0 until ChunkSize
    ) yield {
      val pos = LocalPosition(x, y, z)
      val global = pos.global(chunk)
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
            if(global.y < 128) {
              9.toByte
            } else {
              12.toByte
            }
          }
        } else if(global.y > height - 10) {
          if(global.y >= 128) {
            12.toByte
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
