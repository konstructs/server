package konstructs

import scala.util.Random
import akka.actor.{ Actor, ActorRef, Props, Stash }
import spray.json._

case class FlatWorld(map: ArrayHeightMap)

class FlatWorldActor(name: String, end: Position, val jsonStorage: ActorRef)
    extends Actor with Stash with JsonStorage {
  import World._
  import GeneratorActor.Generated
  import Db.ChunkSize
  import JsonStorage._
  import DefaultJsonProtocol._

  implicit val arrayHeightMapFormat = jsonFormat3(ArrayHeightMap.apply)
  implicit val flatWorldFormat = jsonFormat1(FlatWorld)

  val ns = "worlds"

  load(name)

  var map: PartialFunction[Position, Int] = null

  val random = new Random

  def receive = {
    case JsonLoaded(_, _, Some(json)) =>
      map = GlobalHeightMap(Position(0, 0, 0), json.convertTo[FlatWorld].map)
      context.become(ready)
      unstashAll()
    case JsonLoaded(_, _, None) =>
      val size = 1024
      val newMap = {
        val diamond0 = new DiamondSquareHeightMap(0.1f, size, Position(0, 0, 0), EmptyHeightMap)
        val diamond1 = new DiamondSquareHeightMap(0.2f, size, Position(0, 0, size), diamond0)
        val diamond2 = new DiamondSquareHeightMap(0.1f, size, Position(0, 0, 2 * size), diamond0 orElse diamond1)
        val diamond3 = new DiamondSquareHeightMap(0.3f, size, Position(size, 0, 0), diamond0 orElse diamond1 orElse diamond2)
        val diamond4 = new DiamondSquareHeightMap(0.7f, size, Position(size, 0, size), diamond0 orElse diamond1 orElse diamond2 orElse diamond3)
        val diamond5 = new DiamondSquareHeightMap(0.1f, size, Position(size, 0, 2 * size), diamond0 orElse diamond1 orElse diamond2 orElse diamond3 orElse diamond4)
        val diamond6 = new DiamondSquareHeightMap(0.3f, size, Position(2 * size, 0, 0), diamond0 orElse diamond1 orElse diamond2 orElse diamond3 orElse diamond4 orElse diamond5)
        val diamond7 = new DiamondSquareHeightMap(0.2f, size, Position(2 * size, 0, size), diamond0 orElse diamond1 orElse diamond2 orElse diamond3 orElse diamond4 orElse diamond5 orElse diamond6)
        val diamond8 = new DiamondSquareHeightMap(0.1f, size, Position(2 * size, 0, 2 * size), diamond0 orElse diamond1 orElse diamond2 orElse diamond3 orElse diamond4 orElse diamond5 orElse diamond6 orElse diamond7)
        diamond0 orElse diamond1 orElse diamond2 orElse diamond3 orElse diamond4 orElse diamond5 orElse diamond6 orElse diamond7 orElse diamond8
      }
      val localMap = ArrayHeightMap.fromExistingHeightMap(newMap, size*3, size*3)
      store(name, FlatWorld(localMap).toJson)
      map = GlobalHeightMap(Position(0, 0, 0), localMap)
      context.become(ready)
      unstashAll()
    case _ =>
      stash()
  }

  val compressionBuffer = new Array[Byte](ChunkSize * ChunkSize * ChunkSize)

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

  def ready: Receive = {
    case Generate(real, chunk) =>
      sender ! Generated(real, blocks(chunk))
  }

}

object FlatWorldActor {
  def props(name: String, end: Position, jsonStorage: ActorRef) =
    Props(classOf[FlatWorldActor], name, end, jsonStorage)
}

object World {
  case class Generate(real: ChunkPosition, position: ChunkPosition)
}
