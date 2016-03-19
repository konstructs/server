package konstructs

import java.util.UUID

import scala.collection.mutable

import akka.actor.{ Actor, Stash, ActorRef, Props }

import com.google.gson.reflect.TypeToken

import konstructs.api.{ BinaryLoaded, Position, Block, GsonLoaded,
                        BlockTypeId, BlockFilter, BlockFactory,
                        BlockUpdate }
import konstructs.api.messages.{ BoxQuery, BoxQueryResult, ReplaceBlock,
                                 ReplaceBlockResult, ViewBlock, ViewBlockResult,
                                 BlockUpdateEvent }
import konstructs.utils.compress

case class OpaqueFaces(pn: Boolean, pp: Boolean, qn: Boolean, qp: Boolean, kn: Boolean, kp: Boolean) {
  import OpaqueFaces._
  def toByte = {
    var v = 0
    if(pn) {
      v += PN
    }
    if(pp) {
      v += PP
    }
    if(qn) {
      v += QN
    }
    if(qp) {
      v += QP
    }
    if(kn) {
      v += KN
    }
    if(kp) {
      v += KP
    }
    v.toByte
  }
}

object OpaqueFaces {
  import ChunkData._
  val PN = 0x01
  val PP = 0x02
  val QN = 0x04
  val QP = 0x08
  val KN = 0x10
  val KP = 0x20

  def apply(blockBuffer: Array[Byte]): OpaqueFaces = {
    val pn = !(for(
      y <- 0 until Db.ChunkSize;
      z <- 0 until Db.ChunkSize) yield {
      val x = 0
      isOpaque(blockBuffer(index(x, y, z)))
    }).exists(!_)
    val pp = !(for(
      y <- 0 until Db.ChunkSize;
      z <- 0 until Db.ChunkSize) yield {
      val x = Db.ChunkSize - 1
      isOpaque(blockBuffer(index(x, y, z)))
    }).exists(!_)
    val kn = !(for(
      x <- 0 until Db.ChunkSize;
      z <- 0 until Db.ChunkSize) yield {
      val y = 0
      isOpaque(blockBuffer(index(x, y, z)))
    }).exists(!_)
    val kp = !(for(
      x <- 0 until Db.ChunkSize;
      z <- 0 until Db.ChunkSize) yield {
      val y = Db.ChunkSize - 1
      isOpaque(blockBuffer(index(x, y, z)))
    }).exists(!_)
    val qn = !(for(
      x <- 0 until Db.ChunkSize;
      y <- 0 until Db.ChunkSize) yield {
      val z = 0
      isOpaque(blockBuffer(index(x, y, z)))
    }).exists(!_)
    val qp = !(for(
      x <- 0 until Db.ChunkSize;
      y <- 0 until Db.ChunkSize) yield {
      val z = Db.ChunkSize - 1
      isOpaque(blockBuffer(index(x, y, z)))
    }).exists(!_)
    apply(pn, pp, qn, qp, kn, kp)
  }

  def apply(data: Byte): OpaqueFaces = {
    apply((data & PN) > 0, (data & PP) > 0, (data & QN) > 0, (data & QP) > 0, (data & KN) > 0, (data & KP) > 0)
  }
}

case class ChunkData(data: Array[Byte]) {
  import ChunkData._
  import Db._

  val faces = OpaqueFaces(data(1))
  val version = data(0)

  def unpackTo(blockBuffer: Array[Byte]) {
    val size = compress.inflate(data, blockBuffer, Header, data.size - Header)
    assert(size == ChunkSize * ChunkSize * ChunkSize)
  }

  def block(c: ChunkPosition, p: Position, blockBuffer: Array[Byte]): Byte = {
    unpackTo(blockBuffer)
    blockBuffer(index(c, p))
  }

  def chunks(position: ChunkPosition): Set[ChunkPosition] = {
    val chunks = mutable.Set[ChunkPosition]()
    if(!faces.pn) {
      chunks += position.copy(p = position.p - 1)
    }
    if(!faces.pp) {
      chunks += position.copy(p = position.p + 1)
    }
    if(!faces.qn) {
      chunks += position.copy(q = position.q - 1)
    }
    if(!faces.qp) {
      chunks += position.copy(q = position.q + 1)
    }
    if(!faces.kn) {
      chunks += position.copy(k = position.k - 1)
    }
    if(!faces.kp) {
      chunks += position.copy(k = position.k + 1)
    }
    chunks.toSet
  }
}

object ChunkData {
  import Db._
  val EMPTY = 0
  val GLASS = 10
  val LEAVES = 15
  val TALL_GRASS = 17
  val YELLOW_FLOWER = 18
  val RED_FLOWER = 19
  val PURPLE_FLOWER = 20
  val SUN_FLOWER = 21
  val WHITE_FLOWER = 22
  val BLUE_FLOWER = 23

  def apply(blocks: Array[Byte], buffer: Array[Byte]): ChunkData = {
    val compressed = compress.deflate(blocks, buffer, Header)
    compressed(0) = Db.Version
    compressed(1) = OpaqueFaces(blocks).toByte
    apply(compressed)
  }

  def isOpaque(w: Byte): Boolean = w match {
    case EMPTY => false
    case GLASS => false
    case LEAVES => false
    case TALL_GRASS => false
    case YELLOW_FLOWER => false
    case RED_FLOWER => false
    case PURPLE_FLOWER => false
    case SUN_FLOWER => false
    case WHITE_FLOWER => false
    case BLUE_FLOWER => false
    case _ => true
  }

  def index(x: Int, y: Int, z: Int): Int =
    x + y * Db.ChunkSize + z * Db.ChunkSize * Db.ChunkSize

  def index(c: ChunkPosition, p: Position): Int = {
    val x = p.getX - c.p * Db.ChunkSize
    val y = p.getY - c.k * Db.ChunkSize
    val z = p.getZ - c.q * Db.ChunkSize
    index(x, y, z)
  }

}

class ShardActor(db: ActorRef, shard: ShardPosition, val binaryStorage: ActorRef,
                 val jsonStorage: ActorRef, blockUpdateEvents: Seq[ActorRef],
                 chunkGenerator: ActorRef, blockFactory: BlockFactory)
    extends Actor with Stash with utils.Scheduled with BinaryStorage with JsonStorage {
  import ShardActor._
  import GeneratorActor._
  import DbActor._
  import Db._

  val TypeOfPositionMapping = new TypeToken[java.util.Map[String, UUID]](){}.getType

  val ns = "chunks"

  private val blockBuffer = new Array[Byte](ChunkSize * ChunkSize * ChunkSize)
  private val compressionBuffer = new Array[Byte](ChunkSize * ChunkSize * ChunkSize + Header)
  private val chunks = new Array[Option[ChunkData]](ShardSize * ShardSize * ShardSize)

  private var dirty: Set[ChunkPosition] = Set()
  private var positionMappingDirty = true
  private def chunkId(c: ChunkPosition): String =
    s"${c.p}/${c.q}/${c.k}"

  private val shardId = s"${shard.m}-${shard.n}-${shard.o}"

  private val positionMappingFile = s"${shardId}-position-mapping"

  private def chunkFromId(id: String): ChunkPosition = {
    val pqk = id.split('/').map(_.toInt)
    ChunkPosition(pqk(0), pqk(1), pqk(2))
  }

  private def str(p: Position) = s"${p.getX}-${p.getY}-${p.getZ}"

  schedule(5000, StoreChunks)

  def sendEvent(events: java.util.Map[Position, BlockUpdate]) {
    val msg = new BlockUpdateEvent(events)
    for(l <- blockUpdateEvents) {
      l ! msg
    }
  }

  def sendEvent(position: Position, from: Block, to: Block) {
    val events = new java.util.HashMap[Position, BlockUpdate]()
    events.put(position, new BlockUpdate(from, to))
    sendEvent(events)
  }

  def loadChunk(chunk: ChunkPosition): Option[ChunkData] = {
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

  def runQuery(query: BoxQuery, sender: ActorRef) = {
    val box = query.getBox
    val chunk = ChunkPosition(box.getFrom)
    loadChunk(chunk).map { c =>
      c.unpackTo(blockBuffer)
      val data = new Array[BlockTypeId](box.getNumberOfBlocks + 1)
      for(
        x <- box.getFrom.getX until box.getUntil.getX;
        y <- box.getFrom.getY until box.getUntil.getY;
        z <- box.getFrom.getZ until box.getUntil.getZ) {
        val p = new Position(x, y, z)
        data(box.arrayIndex(p)) =
          blockFactory.getBlockTypeId(blockBuffer(ChunkData.index(chunk, p)).toInt)

      }
      sender ! new BoxQueryResult(box, data)
    }
  }

  def readChunk(pos: Position)(read: Byte => Unit) = {
    val chunk = ChunkPosition(pos)
    loadChunk(chunk).map { c =>
      val block = c.block(chunk, pos, blockBuffer)
      read(block)
    }
  }

  def updateChunk(pos: Position)(update: Byte => Byte) {
    val chunk = ChunkPosition(pos)
    loadChunk(chunk).map { c =>
      dirty = dirty + chunk
      c.unpackTo(blockBuffer)
      val i = ChunkData.index(chunk, pos)
      val oldBlock = blockBuffer(i)
      val block = update(oldBlock)
      blockBuffer(i) = block
      val data = ChunkData(blockBuffer, compressionBuffer)
      chunks(index(chunk)) = Some(data)
      db ! BlockList(chunk, data)
    }
  }

  def replaceBlocks(chunk: ChunkPosition,
    filter: BlockFilter, blocks: Map[Position, BlockTypeId],
    positionMapping: java.util.Map[String, UUID]) {
    loadChunk(chunk).map { c =>
      var isDirty = false
      c.unpackTo(blockBuffer)
      val events = new java.util.HashMap[Position, BlockUpdate]()
      for((position, newTypeId) <- blocks) {
        val i = ChunkData.index(chunk, position)
        val typeId = blockFactory.getBlockTypeId(blockBuffer(i))
        if(filter.matches(typeId, blockFactory.getBlockType(typeId))) {
          val id = positionMapping.remove(str(position))
          events.put(position, new BlockUpdate(Block.create(id, typeId), Block.create(newTypeId)))
          blockBuffer(i) = blockFactory.getW(newTypeId).toByte
          isDirty = true
        }
      }
      if(isDirty) {
        val data = ChunkData(blockBuffer, compressionBuffer)
        chunks(index(chunk)) = Some(data)
        db ! BlockList(chunk, data)
        sendEvent(events)
        dirty = dirty + chunk
      }
    }
  }

  /*
   * Load id mapping for blocks
   */
  loadGson(positionMappingFile)

  /*
   * Receive position mapping before going ready
   */
  def receive() = {
    case GsonLoaded(_, json) if json != null =>
      val positionMapping: java.util.Map[String, UUID] = gson.fromJson(json, TypeOfPositionMapping)
      context.become(ready(positionMapping))
      positionMappingDirty = false
    case GsonLoaded(_, _) =>
      val positionMapping = new java.util.HashMap[String, UUID]()
      context.become(ready(positionMapping))
    case _ =>
      stash()
  }

  def ready(positionMapping: java.util.Map[String, UUID]): Receive = {
    case SendBlocks(chunk) =>
      val s = sender
      loadChunk(chunk).map { c =>
        s ! BlockList(chunk, c)
      }
    case r: ReplaceBlock =>
      val s = sender
      val filter = r.getFilter
      val p = r.getPosition
      val block = r.getBlock
      updateChunk(p) { old =>
        val typeId = blockFactory.getBlockTypeId(old)
        if(filter.matches(typeId, blockFactory.getBlockType(typeId))) {
          val oldId = if(block.getId() != null) {
            positionMapping.put(str(p), block.getId())
          } else {
            positionMapping.remove(str(p))
          }
          val oldBlock = Block.create(oldId, typeId)
          s ! new ReplaceBlockResult(p, oldBlock, true)
          sendEvent(p, oldBlock, block)
          blockFactory.getW(block).toByte
        } else {
          s ! new ReplaceBlockResult(p, block, false)
          old
        }
      }
    case ReplaceBlocks(chunk, filter, blocks) =>
      replaceBlocks(chunk, filter, blocks, positionMapping)
    case v: ViewBlock =>
      val s = sender
      val p = v.getPosition
      readChunk(p) { w =>
        s ! new ViewBlockResult(p, blockFactory.createBlock(positionMapping.get(str(p)), w.toInt))
      }
    case q: BoxQuery =>
      runQuery(q, sender)
    case BinaryLoaded(id, dataOption) =>
      val chunk = chunkFromId(id)
      dataOption match {
        case Some(data) =>
          chunks(index(chunk)) = Some(ChunkData(data))
          unstashAll()
        case None =>
          chunkGenerator ! Generate(chunk)
      }
    case Generated(position, data) =>
      chunks(index(position)) = Some(ChunkData(data, compressionBuffer))
      dirty = dirty + position
      unstashAll()
    case StoreChunks =>
      dirty.map { chunk =>
        chunks(index(chunk)).map { c =>
          storeBinary(chunkId(chunk), c.data)
        }
      }
      dirty = Set()
      if(positionMappingDirty) {
        storeGson(positionMappingFile, gson.toJsonTree(positionMapping, TypeOfPositionMapping))
        positionMappingDirty = false
      }
  }

}

object ShardActor {
  case object StoreChunks

  case class ReplaceBlocks(chunk: ChunkPosition,
    filter: BlockFilter, blocks: Map[Position, BlockTypeId])

  def index(c: ChunkPosition): Int = {
    val lp = math.abs(c.p % Db.ShardSize)
    val lq = math.abs(c.q % Db.ShardSize)
    val lk = math.abs(c.k % Db.ShardSize)
    lp + lq * Db.ShardSize + lk * Db.ShardSize * Db.ShardSize
  }

  def props(db: ActorRef, shard: ShardPosition,
    binaryStorage: ActorRef, jsonStorage: ActorRef,
    blockUpdateEvents: Seq[ActorRef], chunkGenerator: ActorRef,
    blockFactory: BlockFactory) =
    Props(classOf[ShardActor], db, shard, binaryStorage, jsonStorage,
      blockUpdateEvents, chunkGenerator, blockFactory)
}
