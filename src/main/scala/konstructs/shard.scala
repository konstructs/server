package konstructs

import java.util.UUID

import scala.collection.mutable

import akka.actor.{ Actor, Stash, ActorRef, Props }

import com.google.gson.reflect.TypeToken

import konstructs.api.{ BinaryLoaded, Position, Block, GsonLoaded,
                        BlockTypeId, BlockFilter, BlockFilterFactory,
                        BlockFactory, BlockState,
                        BlockUpdate, Health, ReceiveStack, Stack }
import konstructs.api.messages.{ BoxQuery, BoxQueryResult, ReplaceBlock,
                                 ReplaceBlockResult, ViewBlock, ViewBlockResult,
                                 BlockUpdateEvent, InteractResult, InteractTertiaryFilter,
                                 InteractTertiary }
import konstructs.utils.compress

case class BlockData(w: Int, health: Int) {
  def write(data: Array[Byte], i: Int) {
    BlockData.write(data, i, w, health)
  }
}

object BlockData {

  def w(data: Array[Byte], i: Int): Int =
    (data(i * Db.BlockSize) & 0xFF) + ((data(i * Db.BlockSize + 1) & 0xFF) << 8)

  def hp(data: Array[Byte], i: Int): Int =
    (data(i * Db.BlockSize + 2) & 0xFF) + ((data(i * Db.BlockSize + 3) & 0x07) << 8)

  def apply(data: Array[Byte], i: Int): BlockData = {
    apply(w(data, i), hp(data, i))
  }

  def write(data: Array[Byte], i: Int, w: Int, health: Int) {
    writeW(data, i, w)
    writeHealth(data, i, health)
  }

  def writeW(data: Array[Byte], i: Int, w: Int) {
    data(i * Db.BlockSize) = (w & 0xFF).toByte
    data(i * Db.BlockSize + 1) = ((w >> 8) & 0xFF).toByte
  }

  def writeHealth(data: Array[Byte], i: Int, health: Int) {
    data(i * Db.BlockSize + 2) = (health & 0xFF).toByte
    data(i * Db.BlockSize + 3) = ((health >> 8) & 0x07).toByte
  }

}

case class ChunkData(version: Int, data: Array[Byte]) {
  import ChunkData._
  import Db._

  def unpackTo(blockBuffer: Array[Byte]) {
    val size = compress.inflate(data, blockBuffer, Header, data.size - Header)
  }

  def block(c: ChunkPosition, p: Position, blockBuffer: Array[Byte]): Byte = {
    unpackTo(blockBuffer)
    blockBuffer(index(c, p) * Db.BlockSize)
  }

}

object ChunkData {
  val Size = Db.ChunkSize * Db.ChunkSize * Db.ChunkSize * Db.BlockSize

  def apply(blocks: Array[Byte], buffer: Array[Byte]): ChunkData = {
    val compressed = compress.deflate(blocks, buffer, Db.Header)
    compressed(0) = Db.Version
    compressed(1) = 0.toByte
    apply(Db.Version, compressed)
  }

  def loadOldFormat(version: Int, data: Array[Byte], blockBuffer: Array[Byte], compressionBuffer: Array[Byte]): ChunkData = {
    val size = compress.inflate(data, blockBuffer, Db.Header, data.size - Db.Header)
    convertFromOldFormat(blockBuffer, size)
    apply(blockBuffer, compressionBuffer)
  }

  private def convertFromOldFormat(buf: Array[Byte], size: Int) {
    val tmp = java.util.Arrays.copyOf(buf, size)
    for(i <- 0 until size) {
      buf(i * 4) = tmp(i)
      buf(i * 4 + 1) = 0.toByte
      buf(i * 4 + 2) = 0xFF.toByte
      buf(i * 4 + 3) = 0x07.toByte
    }
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
                 chunkGenerator: ActorRef, blockFactory: BlockFactory,
                 tertiaryInteractionFilters: Seq[ActorRef], universe: ActorRef)
    extends Actor with Stash with utils.Scheduled with BinaryStorage with JsonStorage {
  import ShardActor.{ ReplaceBlocks, index, StoreChunks, str }
  import GeneratorActor._
  import DbActor._
  import Db._

  val TypeOfPositionMapping = new TypeToken[java.util.Map[String, UUID]](){}.getType
  val ReplaceFilter = BlockFilterFactory.withBlockState(BlockState.LIQUID).or(BlockFilterFactory.withBlockState(BlockState.GAS)).or(BlockFilterFactory.VACUUM)
  val ns = "chunks"

  private val blockBuffer = new Array[Byte](ChunkData.Size)
  private val compressionBuffer = new Array[Byte](ChunkData.Size + Header)
  private val chunks = new Array[Option[ChunkData]](ShardSize * ShardSize * ShardSize)

  private var dirty: Set[ChunkPosition] = Set()
  private var positionMappingDirty = true
  private def chunkId(c: ChunkPosition): String =
    s"${c.p}/${c.q}/${c.k}"

  private val shardId = ShardActor.shardId(shard)

  private val positionMappingFile = ShardActor.positionMappingFile(shardId)

  private def chunkFromId(id: String): ChunkPosition = {
    val pqk = id.split('/').map(_.toInt)
    ChunkPosition(pqk(0), pqk(1), pqk(2))
  }

  private val VacuumData = new BlockData(blockFactory.getW(BlockTypeId.VACUUM), Health.PRISTINE.getHealth())
  private val VacuumBlock = Block.create(BlockTypeId.VACUUM)
  private val toolDurabilityBonus = 10.0f

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
          blockFactory.getBlockTypeId(BlockData.w(blockBuffer, ChunkData.index(chunk, p)))

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

  def updateChunk(pos: Position)(update: BlockData => BlockData) {
    val chunk = ChunkPosition(pos)
    loadChunk(chunk).map { c =>
      dirty = dirty + chunk
      c.unpackTo(blockBuffer)
      val i = ChunkData.index(chunk, pos)
      val oldBlock = BlockData(blockBuffer, i)
      val block = update(oldBlock)
      block.write(blockBuffer, i)
      val data = ChunkData(blockBuffer, compressionBuffer)
      chunks(index(chunk)) = Some(data)
      db ! ChunkUpdate(chunk, data)
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
        val typeId = blockFactory.getBlockTypeId(BlockData.w(blockBuffer, i))
        if(filter.matches(typeId, blockFactory.getBlockType(typeId))) {
          val id = positionMapping.remove(str(position))
          events.put(position, new BlockUpdate(Block.create(id, typeId), Block.create(newTypeId)))
          BlockData.writeW(blockBuffer, i, blockFactory.getW(newTypeId))
          isDirty = true
          if(id != null)
            positionMappingDirty = true
        }
      }
      if(isDirty) {
        val data = ChunkData(blockBuffer, compressionBuffer)
        chunks(index(chunk)) = Some(data)
        db ! ChunkUpdate(chunk, data)
        sendEvent(events)
        dirty = dirty + chunk
      }
    }
  }

  def damageBlock(position: Position, dealing: Block,
    positionMapping: java.util.Map[String, UUID])(ready: (Block, Block) => Unit) {
    updateChunk(position) { old =>
      val receivingTypeId = blockFactory.getBlockTypeId(old.w)
      val receivingType = blockFactory.getBlockType(receivingTypeId)
      val dealingType = blockFactory.getBlockType(dealing.getType())
      val dealingDamage = dealingType.getDamageWithMultiplier(receivingTypeId, receivingType)
      val receivingHealth = Health.get(old.health).damage(dealingDamage, receivingType.getDurability())
      val dealingHealth = dealing.getHealth().damage(receivingType.getDamage(), dealingType.getDurability() * toolDurabilityBonus)
      val dealingBlock = if(dealingHealth.isDestroyed) {
        null
      } else {
        new Block(dealing.getId(), dealing.getType(), dealingHealth)
      }

      val (receivingBlock, data) = if(receivingHealth.isDestroyed()) {
        val id = positionMapping.remove(str(position))
        if(id != null)
          positionMappingDirty = true
        val oldBlock = new Block(id, receivingTypeId, Health.PRISTINE)
        sendEvent(position, oldBlock, VacuumBlock)
        (oldBlock, VacuumData)
      } else {
        (null, new BlockData(old.w, receivingHealth.getHealth()))
      }
      ready(dealingBlock, receivingBlock)
      data
    }
  }

  def replaceBlock(filter: BlockFilter, position: Position, block: Block,
    positionMapping: java.util.Map[String, UUID])(ready: Block => Unit) = {
    updateChunk(position) { old =>
      val typeId = blockFactory.getBlockTypeId(old.w)
      if(filter.matches(typeId, blockFactory.getBlockType(typeId))) {
        val oldId = if(block.getId() != null) {
          positionMappingDirty = true
          positionMapping.put(str(position), block.getId())
        } else {
          val id = positionMapping.remove(str(position))
          if(id != null)
            positionMappingDirty = true
          id
        }
        val oldBlock = new Block(oldId, typeId, Health.get(old.health))
        ready(oldBlock)
        sendEvent(position, oldBlock, block)
        BlockData(blockFactory.getW(block), block.getHealth().getHealth())
      } else {
        old
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
    case i: InteractPrimaryUpdate =>
      val s = sender
      val block = Option(i.block).getOrElse(VacuumBlock)
      damageBlock(i.position, block, positionMapping) { (using, damaged) =>
        if(damaged != null) {
          s ! ReceiveStack(Stack.createFromBlock(damaged))
        }
        if(block != null)
          s ! new InteractResult(i.position, using, damaged)
        else
          s ! new InteractResult(i.position, null, damaged)
      }
    case i: InteractSecondaryUpdate =>
      val s = sender
      replaceBlock(ReplaceFilter, i.position, i.block, positionMapping) { b =>
        if(b == i.block) {
          s ! new InteractResult(i.position, i.block, null)
        } else {
          s ! new InteractResult(i.position, null, null)
        }
      }
    case i: InteractTertiaryUpdate =>
      val filters = tertiaryInteractionFilters :+ self
      val message = i.message
      val p = message.getPosition
      readChunk(p) { w =>
        val b = blockFactory.createBlock(positionMapping.get(str(p)), w.toInt)
        val filters = tertiaryInteractionFilters :+ self
        filters.head ! new InteractTertiaryFilter(
          filters.tail.toArray,
          message.withBlockAtPosition(b).withWorldPhase(true))
      }
    case i: InteractTertiaryFilter if i.getMessage.isWorldPhase =>
      val filters = tertiaryInteractionFilters :+ self
      filters.head ! new InteractTertiaryFilter(filters.tail.toArray, i.getMessage.withWorldPhase(false))
    case i: InteractTertiaryFilter if !i.getMessage.isWorldPhase =>
      val message = i.getMessage
      val position = message.getPosition
      val blockAtPosition = message.getBlockAtPosition
      /* Update the block with any changes made by the filter */
      replaceBlock(BlockFilterFactory.EVERYTHING, position, blockAtPosition, positionMapping) { b =>
        message.getSender ! new InteractResult(position, message.getBlock, blockAtPosition)
      }
    case r: ReplaceBlock =>
      val s = sender
      replaceBlock(r.getFilter, r.getPosition, r.getBlock, positionMapping) { b =>
        if(b == r.getBlock) {
          s ! new ReplaceBlockResult(r.getPosition, b, true)
        } else {
          s ! new ReplaceBlockResult(r.getPosition, b, false)
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
          val version = data(0)
          chunks(index(chunk)) = Some(if(version < Db.Version) {
            dirty = dirty + chunk
            ChunkData.loadOldFormat(version, data, blockBuffer, compressionBuffer)
          } else {
            ChunkData(version, data)
          })
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

  def shardId(shard: ShardPosition) = s"${shard.m}-${shard.n}-${shard.o}"

  def positionMappingFile(shardId: String) = s"${shardId}-position-mapping"

  def str(p: Position) = s"${p.getX}-${p.getY}-${p.getZ}"

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
    blockFactory: BlockFactory, tertiaryInteractionFilters: Seq[ActorRef],
    universe: ActorRef) =
    Props(classOf[ShardActor], db, shard, binaryStorage, jsonStorage,
      blockUpdateEvents, chunkGenerator, blockFactory, tertiaryInteractionFilters,
      universe)
}
