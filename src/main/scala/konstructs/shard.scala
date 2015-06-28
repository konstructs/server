package konstructs

import akka.actor.{ Actor, Stash, ActorRef, Props }

import konstructs.api._

class ShardActor(db: ActorRef, shard: ShardPosition, val binaryStorage: ActorRef, chunkGenerator: ActorRef)
    extends Actor with Stash with utils.Scheduled with BinaryStorage {
  import ShardActor._
  import BinaryStorage._
  import GeneratorActor._
  import DbActor._
  import PlayerActor.ReceiveBlock
  import Db._

  val ns = "chunks"

  private val blocks = new Array[Byte](ChunkSize * ChunkSize * ChunkSize)
  private val compressionBuffer = new Array[Byte](ChunkSize * ChunkSize * ChunkSize)
  private val chunks = new Array[Option[Array[Byte]]](ShardSize * ShardSize * ShardSize)
  private var dirty: Set[ChunkPosition] = Set()

  private def chunkId(c: ChunkPosition): String =
    s"${c.p}/${c.q}/${c.k}"

  private def chunkFromId(id: String): ChunkPosition = {
    val pqk = id.split('/').map(_.toInt)
    ChunkPosition(pqk(0), pqk(1), pqk(2))
  }



  schedule(5000, StoreChunks)

  def loadChunk(chunk: ChunkPosition): Option[Array[Byte]] = {
    val i = index(chunk)
    val blocks = chunks(i)
    if(blocks != null) {
      if(!blocks.isDefined)
        stash()
      blocks
    } else {
      loadBinary(chunkId(chunk))
      chunks(i) = None
      stash()
      None
    }
  }

  def updateChunk(pos: Position)(update: Byte => Byte) {
    val chunk = ChunkPosition(pos)
    loadChunk(chunk).map { compressedBlocks =>
      dirty = dirty + chunk
      val size = compress.inflate(compressedBlocks, blocks)
      assert(size == blocks.size)
      val i = index(chunk, pos)
      val oldBlock = blocks(i)
      val block = update(oldBlock)
      blocks(i) = block
      val compressed = compress.deflate(blocks, compressionBuffer)
      chunks(index(chunk)) = Some(compressed)
    }
  }

  def receive() = {
    case SendBlocks(chunk, _) =>
      val s = sender
      loadChunk(chunk).map { blocks =>
        s ! BlockList(chunk, blocks)
      }
    case PutBlock(p, w) =>
      val s = sender
      updateChunk(p) { old =>
        if(old == 0) {
          db ! BlockUpdate(p, old.toInt, w)
          w.toByte
        } else {
          s ! ReceiveBlock(w.toByte)
          old
        }
      }
    case DestroyBlock(p) =>
      val s = sender
      updateChunk(p) { old =>
        s ! ReceiveBlock(old)
          db ! BlockUpdate(p, old.toInt, 0)
        0
      }
    case BinaryLoaded(id, blocksOption) => {
      val chunk = chunkFromId(id)
      blocksOption match {
        case Some(blocks) =>
          chunks(index(chunk)) = Some(blocks)
          unstashAll()
        case None =>
          chunkGenerator ! Generate(chunk)
      }
    }
    case Generated(position, chunk) =>
      chunks(index(position)) = Some(chunk.data)
      dirty = dirty + position
      unstashAll()
    case StoreChunks =>
      dirty.map { chunk =>
        chunks(index(chunk)).map { blocks =>
          storeBinary(chunkId(chunk), blocks)
        }
      }
      dirty = Set()
  }

}

object ShardActor {
  case object StoreChunks

  def index(c: ChunkPosition, p: Position): Int = {
    val x = p.x - c.p * Db.ChunkSize
    val y = p.y - c.k * Db.ChunkSize
    val z = p.z - c.q * Db.ChunkSize
    x + y * Db.ChunkSize + z * Db.ChunkSize * Db.ChunkSize
  }

  def index(c: ChunkPosition): Int = {
    val lp = math.abs(c.p % Db.ShardSize)
    val lq = math.abs(c.q % Db.ShardSize)
    val lk = math.abs(c.k % Db.ShardSize)
    lp + lq * Db.ShardSize + lk * Db.ShardSize * Db.ShardSize
  }

  def props(db: ActorRef, shard: ShardPosition, binaryStorage: ActorRef, chunkGenerator: ActorRef) =
    Props(classOf[ShardActor], db, shard, binaryStorage, chunkGenerator)
}
