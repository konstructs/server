package konstructs

import akka.actor.{ Actor, Stash, ActorRef, Props }

class ShardActor(shard: ShardPosition, chunkStore: ActorRef, chunkGenerator: ActorRef)
    extends Actor with Stash with utils.Scheduled{
  import ShardActor._
  import StorageActor._
  import GeneratorActor._
  import DbActor._
  import PlayerActor.ReceiveBlock
  import Db._
  private val blocks = new Array[Byte](ChunkSize * ChunkSize * ChunkSize)
  private val compressionBuffer = new Array[Byte](ChunkSize * ChunkSize * ChunkSize)
  private val chunks = new Array[Option[Array[Byte]]](ShardSize * ShardSize * ShardSize)
  private var dirty: Set[ChunkPosition] = Set()

  schedule(5000, StoreChunks)

  def loadChunk(chunk: ChunkPosition): Option[Array[Byte]] = {
    val i = index(chunk)
    val blocks = chunks(i)
    if(blocks != null) {
      if(!blocks.isDefined)
        stash()
      blocks
    } else {
      chunkStore ! Load(chunk)
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
    case SendBlocks(to, chunk, _) =>
      loadChunk(chunk).map { blocks =>
        to ! BlockList(chunk, blocks)
      }
    case PutBlock(by, p, w) =>
      updateChunk(p) { old =>
        if(old == 0) {
          sender ! BlockUpdate(p, old.toInt, w)
          w.toByte
        } else {
          by ! ReceiveBlock(w.toByte)
          old
        }
      }
    case DestroyBlock(by, p) =>
      updateChunk(p) { old =>
        by ! ReceiveBlock(old)
        sender ! BlockUpdate(p, old.toInt, 0)
        0
      }
    case Loaded(chunk, blocksOption) =>
      blocksOption match {
        case Some(blocks) =>
          chunks(index(chunk)) = Some(blocks)
          unstashAll()
        case None =>
          chunkGenerator ! Generate(chunk)
      }
    case Generated(position, chunk) =>
      chunks(index(position)) = Some(chunk.data)
      dirty = dirty + position
      unstashAll()
    case StoreChunks =>
      dirty.map { chunk =>
        chunks(index(chunk)).map { blocks =>
          chunkStore ! Store(chunk, blocks)
        }
      }
      dirty = Set()
  }

}

object ShardActor {
  case object StoreChunks
  case class BlockUpdate(pos: Position, oldW: Int, newW: Int)

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

  def props(shard: ShardPosition, chunkStore: ActorRef, chunkGenerator: ActorRef) =
    Props(classOf[ShardActor], shard, chunkStore, chunkGenerator)
}
