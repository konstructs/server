package konstructs

import scala.util.Random
import akka.actor.{ Actor, ActorRef, Props, Stash }
import konstructs.api._
import konstructs.utils._

case class FlatWorld(sizeX: Int, sizeZ: Int)

class FlatWorldActor(name: String, end: Position, factory: BlockFactory,
  val jsonStorage: ActorRef, val binaryStorage: ActorRef)
    extends Actor with Stash with JsonStorage with BinaryStorage {
  import World._
  import GeneratorActor.Generated
  import Db.ChunkSize

  val ns = "worlds"

  loadGson(name)

  val random = new Random
  val size = 1024

  def receive = {
    case GsonLoaded(_, json) if json != null =>
      val world = gson.fromJson(json, classOf[FlatWorld])
      loadBinary(name)
      context.become(loadHeightMap(world))
      unstashAll()
    case GsonLoaded(_, _) =>
      val world = FlatWorld(size*3, size*3)
      storeGson(name, gson.toJsonTree(world))
      loadBinary(name)
      context.become(loadHeightMap(world))
      unstashAll()
    case _ =>
      stash()
  }

  def loadHeightMap(world: FlatWorld): Receive = {
    case BinaryLoaded(_, Some(data)) =>
      val localMap = ArrayHeightMap.fromByteArray(data, world.sizeX, world.sizeZ)
      val map = GlobalHeightMap(new Position(0, 0, 0), localMap)
      context.become(ready(world, map))
    case BinaryLoaded(_, None) =>
      val newMap = {
        val diamond0 = new DiamondSquareHeightMap(0.1f, size, new Position(0, 0, 0), EmptyHeightMap)
        val diamond1 = new DiamondSquareHeightMap(0.2f, size, new Position(0, 0, size), diamond0)
        val diamond2 = new DiamondSquareHeightMap(0.1f, size, new Position(0, 0, 2 * size), diamond0 orElse diamond1)
        val diamond3 = new DiamondSquareHeightMap(0.3f, size, new Position(size, 0, 0), diamond0 orElse diamond1 orElse diamond2)
        val diamond4 = new DiamondSquareHeightMap(0.7f, size, new Position(size, 0, size), diamond0 orElse diamond1 orElse diamond2 orElse diamond3)
        val diamond5 = new DiamondSquareHeightMap(0.1f, size, new Position(size, 0, 2 * size), diamond0 orElse diamond1 orElse diamond2 orElse diamond3 orElse diamond4)
        val diamond6 = new DiamondSquareHeightMap(0.3f, size, new Position(2 * size, 0, 0), diamond0 orElse diamond1 orElse diamond2 orElse diamond3 orElse diamond4 orElse diamond5)
        val diamond7 = new DiamondSquareHeightMap(0.2f, size, new Position(2 * size, 0, size), diamond0 orElse diamond1 orElse diamond2 orElse diamond3 orElse diamond4 orElse diamond5 orElse diamond6)
        val diamond8 = new DiamondSquareHeightMap(0.1f, size, new Position(2 * size, 0, 2 * size), diamond0 orElse diamond1 orElse diamond2 orElse diamond3 orElse diamond4 orElse diamond5 orElse diamond6 orElse diamond7)
        diamond0 orElse diamond1 orElse diamond2 orElse diamond3 orElse diamond4 orElse diamond5 orElse diamond6 orElse diamond7 orElse diamond8
      }
      val localMap = ArrayHeightMap.fromExistingHeightMap(newMap, size*3, size*3)
      storeBinary(name, localMap.toByteArray)
      val map = GlobalHeightMap(new Position(0, 0, 0), localMap)
      context.become(ready(world, map))
      unstashAll()
    case _ =>
      stash()
  }

  private def w(ns: String, name: String) =
    factory.getW(new BlockTypeId(ns, name)).toByte

  private val Konstructs = "org/konstructs"
  private val Flowers = Seq("flower-yellow", "flower-red", "flower-purple", "sunflower",
    "flower-white", "flower-blue")
  private def blockSeq(chunk: ChunkPosition, map: HeightMap): Seq[Byte] = {
    for(
      z <- 0 until ChunkSize;
      y <- 0 until ChunkSize;
      x <- 0 until ChunkSize
    ) yield {
      val global = chunk.position(x, y, z)
      val height = map(global) + 32
      val gy = global.getY
      if(gy < height) {
        w(Konstructs, "dirt")
      } else if (gy < 10) {
        w(Konstructs, "water")
      } else {
        w(Konstructs, "vacuum")
      }
    }
  }

  private def blocks(chunk: ChunkPosition, map: HeightMap): Array[Byte] =
    Array(blockSeq(chunk, map) :_*)

  def ready(world: FlatWorld, map: HeightMap): Receive = {
    case Generate(real, chunk) =>
      sender ! Generated(real, blocks(chunk, map))
  }

}

object FlatWorldActor {
  def props(name: String, end: Position, factory: BlockFactory,
    jsonStorage: ActorRef, binaryStorage: ActorRef) =
    Props(classOf[FlatWorldActor], name, end, factory, jsonStorage, binaryStorage)
}

object World {
  case class Generate(real: ChunkPosition, position: ChunkPosition)
}
