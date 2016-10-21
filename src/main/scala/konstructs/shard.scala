package konstructs

import java.util.UUID
import java.nio.{ ByteBuffer, ByteOrder }

import scala.collection.mutable

import akka.actor.{ Actor, Stash, ActorRef, Props }

import com.google.gson.reflect.TypeToken

import konstructs.api.{ BinaryLoaded, Position, Block, GsonLoaded,
                        BlockTypeId, BlockFilter, BlockFilterFactory,
                        BlockFactory, BlockState, Direction, Rotation,
                        Orientation, LightLevel, Colour,
                        BlockUpdate, Health, ReceiveStack, Stack }
import konstructs.api.messages.{ BoxQuery, BoxQueryResult, ReplaceBlock,
                                 ReplaceBlockResult, ViewBlock, ViewBlockResult,
                                 BlockUpdateEvent, InteractResult, InteractTertiaryFilter,
                                 InteractTertiary }
import konstructs.utils.compress

case class BlockData(w: Int, health: Int, direction: Int, rotation: Int, ambient: Int,
  red: Int, green: Int, blue: Int, light: Int) {
  def write(data: Array[Byte], i: Int) {
    BlockData.write(data, i, w, health, direction, rotation, ambient, red, green, blue, light)
  }

  def block(id: UUID, blockTypeId: BlockTypeId) =
    new Block(id, blockTypeId, Health.get(health), Orientation.get(direction, rotation))
}

object BlockData {

  def w(data: Array[Byte], i: Int): Int =
    (data(i * Db.BlockSize) & 0xFF) + ((data(i * Db.BlockSize + 1) & 0xFF) << 8)

  def hp(data: Array[Byte], i: Int): Int =
    (data(i * Db.BlockSize + 2) & 0xFF) + ((data(i * Db.BlockSize + 3) & 0x07) << 8)

  def direction(data: Array[Byte], i: Int): Int =
    ((data(i * Db.BlockSize + 3) & 0xE0) >> 5)

  def rotation(data: Array[Byte], i: Int): Int =
    ((data(i * Db.BlockSize + 3) & 0x18) >> 3)

  def ambientLight(data: Array[Byte], i: Int): Int =
    ((data(i * Db.BlockSize + 4) & 0x0F))

  def red(data: Array[Byte], i: Int): Int =
    ((data(i * Db.BlockSize + 4) & 0xF0) >> 4)

  def green(data: Array[Byte], i: Int): Int =
    ((data(i * Db.BlockSize + 5) & 0x0F))

  def blue(data: Array[Byte], i: Int): Int =
    ((data(i * Db.BlockSize + 5) & 0xF0) >> 4)

  def light(data: Array[Byte], i: Int): Int =
    ((data(i * Db.BlockSize + 6) & 0x0F))

  def apply(data: Array[Byte], i: Int): BlockData = {
    apply(
      w(data, i),
      hp(data, i),
      direction(data, i),
      rotation(data, i),
      ambientLight(data, i),
      red(data, i),
      green(data, i),
      blue(data, i),
      light(data, i))
  }

  def apply(w: Int, block: Block, ambient: Int, colour: Colour, level: LightLevel): BlockData = {
    apply(
      w,
      block.getHealth.getHealth,
      block.getOrientation.getDirection.getEncoding,
      block.getOrientation.getRotation.getEncoding,
      ambient,
      colour.getRed,
      colour.getGreen,
      colour.getBlue,
      level.getLevel
    )
  }

  def write(data: Array[Byte], i: Int, w: Int, health: Int, direction: Int, rotation: Int, ambient: Int,
    red: Int, green: Int, blue: Int, light: Int) {
    writeW(data, i, w)
    writeHealthAndOrientation(data, i, health, direction, rotation)
    writeLight(data, i, ambient, red, green, blue, light)
  }

  def write(data: Array[Byte], i: Int, w: Int, block: Block, ambientLight: LightLevel, colour: Colour, light: LightLevel) {
    write(data, i, w, block.getHealth.getHealth,
      block.getOrientation.getDirection.getEncoding,
      block.getOrientation.getRotation.getEncoding,
      ambientLight.getLevel,
      colour.getRed,
      colour.getGreen,
      colour.getBlue,
      light.getLevel
    )
  }

  def writeW(data: Array[Byte], i: Int, w: Int) {
    data(i * Db.BlockSize) = (w & 0xFF).toByte
    data(i * Db.BlockSize + 1) = ((w >> 8) & 0xFF).toByte
  }

  def writeHealthAndOrientation(data: Array[Byte], i: Int, health: Int, direction: Int, rotation: Int) {
    data(i * Db.BlockSize + 2) = (health & 0xFF).toByte
    val b = (((direction << 5) & 0xE0) + ((rotation << 3) & 0x18) + ((health >> 8) & 0x07)).toByte
    data(i * Db.BlockSize + 3) = b
  }

  def writeLight(data: Array[Byte], i: Int, ambient: Int, red: Int, green: Int, blue: Int, light: Int) {
    val b4 = ((ambient & 0x0F) + ((red << 4) & 0xF0)).toByte
    data(i * Db.BlockSize + 4) = b4
    val b5 = ((green & 0x0F) + ((blue << 4) & 0xF0)).toByte
    data(i * Db.BlockSize + 5) = b5
    data(i * Db.BlockSize + 6) = (light & 0x0F).toByte
  }

}

case class ChunkData(version: Int, data: Array[Byte]) {
  import ChunkData._
  import Db._

  val revision = readRevision(data, 2)

  def unpackTo(blockBuffer: Array[Byte]) {
    val size = compress.inflate(data, blockBuffer, Header, data.size - Header)
  }

  def block(c: ChunkPosition, p: Position, blockBuffer: Array[Byte]): BlockData = {
    unpackTo(blockBuffer)
    BlockData(blockBuffer, index(c, p))
  }

}

object ChunkData {
  val InitialRevision = 1
  val Size = Db.ChunkSize * Db.ChunkSize * Db.ChunkSize * Db.BlockSize

  /* Writes revision as a Little Endian 4 byte unsigned integer
   * Long is required since all Java types are signed
   */
  def writeRevision(data: Array[Byte], revision: Long, offset: Int) {
    if(revision > 4294967295L) {
      throw new IllegalArgumentException("Must be smaller than 4294967295")
    }
    data(offset + 0) = (revision & 0xFF).toByte
    data(offset + 1) = ((revision >> 8) & 0xFF).toByte
    data(offset + 2) = ((revision >> 16) & 0xFF).toByte
    data(offset + 3) = ((revision >> 24) & 0xFF).toByte
  }

  /* Read revision as a Little Endian 4 byte unsigned integer
   * Returns long as all Java types are signed
   */
  def readRevision(data: Array[Byte], offset: Int): Long = {
    (data(offset + 0) & 0xFF).toLong +
      ((data(offset + 1) & 0xFF) << 8).toLong +
      ((data(offset + 2) & 0xFF) << 16).toLong +
      ((data(offset + 3) & 0x7F) << 24).toLong
  }

  def apply(revision: Long, blocks: Array[Byte], buffer: Array[Byte]): ChunkData = {
    val compressed = compress.deflate(blocks, buffer, Db.Header)
    compressed(0) = Db.Version
    compressed(1) = 0.toByte
    writeRevision(compressed, revision, 2)
    apply(Db.Version, compressed)
  }

  def loadOldFormat(version: Int, data: Array[Byte], blockBuffer: Array[Byte], compressionBuffer: Array[Byte]): ChunkData = {

    if(version == 1) {
      val size = compress.inflate(data, blockBuffer, Db.Version1Header, data.size - Db.Version1Header)
      convertFromOldFormat1(blockBuffer, size)
    } else {
      val size = compress.inflate(data, blockBuffer, Db.Header, data.size - Db.Header)
      convertFromOldFormat2(blockBuffer, size)
    }
    apply(0, blockBuffer, compressionBuffer)
  }

  private def convertFromOldFormat1(buf: Array[Byte], size: Int) {
    val tmp = java.util.Arrays.copyOf(buf, size)
    for(i <- 0 until size) {
      buf(i * Db.BlockSize) = tmp(i)
      buf(i * Db.BlockSize + 1) = 0.toByte
      buf(i * Db.BlockSize + 2) = 0xFF.toByte
      buf(i * Db.BlockSize + 3) = 0x07.toByte
      buf(i * Db.BlockSize + 4) = 0x00.toByte
      buf(i * Db.BlockSize + 5) = 0x00.toByte
      buf(i * Db.BlockSize + 6) = 0x00.toByte
    }
  }

  private def convertFromOldFormat2(buf: Array[Byte], size: Int) {
    val tmp = java.util.Arrays.copyOf(buf, size)
    for(i <- 0 until (size / 4)) {
      buf(i * Db.BlockSize) = tmp(i * 4)
      buf(i * Db.BlockSize + 1) = tmp(i * 4 + 1)
      buf(i * Db.BlockSize + 2) = tmp(i * 4 + 2)
      buf(i * Db.BlockSize + 3) = tmp(i * 4 + 3)
      buf(i * Db.BlockSize + 4) = 0x00.toByte
      buf(i * Db.BlockSize + 5) = 0x00.toByte
      buf(i * Db.BlockSize + 6) = 0x00.toByte
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

  private val VacuumData = BlockData(
    blockFactory.getW(BlockTypeId.VACUUM),
    Health.PRISTINE.getHealth(),
    Direction.UP_ENCODING,
    Rotation.IDENTITY_ENCODING,
    LightLevel.FULL_ENCODING,
    0, 0, 0,
    LightLevel.DARK_ENCODING
  )
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

  def readChunk(pos: Position)(read: BlockData => Unit) = {
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
      updateLight(pos, chunk)
      val data = ChunkData(c.revision + 1, blockBuffer, compressionBuffer)
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
          val oldBlock = BlockData(blockBuffer, i).block(id, typeId)
          val newBlock = Block.create(newTypeId)
          events.put(position, new BlockUpdate(oldBlock, newBlock))
          BlockData.write(blockBuffer, i, blockFactory.getW(newTypeId), newBlock, LightLevel.FULL, Colour.BLACK, LightLevel.DARK)
          isDirty = true
          if(id != null)
            positionMappingDirty = true
        }
      }
      if(isDirty) {
        val data = ChunkData(c.revision + 1, blockBuffer, compressionBuffer)
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
        dealing.withHealth(dealingHealth)
      }
      val id = positionMapping.remove(str(position))
      val oldBlock = old.block(id, receivingTypeId)
      val (receivingBlock, data) = if(receivingHealth.isDestroyed()) {
        if(id != null)
          positionMappingDirty = true
        val block = if(receivingType.getDestroyedAs() == BlockTypeId.SELF) {
          oldBlock
            .withOrientation(Orientation.NORMAL)
            .withHealth(Health.PRISTINE)
        } else {
          oldBlock
            .withOrientation(Orientation.NORMAL)
            .withHealth(Health.PRISTINE)
            .withType(receivingType.getDestroyedAs())
        }
        sendEvent(position, oldBlock, VacuumBlock)
        (block, VacuumData)
      } else {
        (null, old.copy(health = receivingHealth.getHealth()))
      }
      ready(dealingBlock, receivingBlock)
      data
    }
  }

  def replaceBlock(filter: BlockFilter, position: Position, block: Block,
    positionMapping: java.util.Map[String, UUID])(ready: Block => Unit) = {
    updateChunk(position) { old =>
      val typeId = blockFactory.getBlockTypeId(old.w)
      val blockType = blockFactory.getBlockType(typeId)
      if(filter.matches(typeId, blockType)) {
        val oldId = if(block.getId() != null) {
          positionMappingDirty = true
          positionMapping.put(str(position), block.getId())
        } else {
          val id = positionMapping.remove(str(position))
          if(id != null)
            positionMappingDirty = true
          id
        }
        val oldBlock = old.block(oldId, typeId)
        ready(oldBlock)
        sendEvent(position, oldBlock, block)
        val newBlockType =  blockFactory.getBlockType(block.getType)
        println(newBlockType)
        BlockData(blockFactory.getW(block.getType()), block, old.ambient,
          newBlockType.getLightColour, newBlockType.getLightLevel)
      } else {
        old
      }
    }
  }

  def updateLight(block: Position, chunk: ChunkPosition): Set[Position] = {
    val queue = mutable.Queue[Position](block)
    val buffer = mutable.ArrayBuffer[Position]()

    while(!queue.isEmpty) {
      val pos = queue.dequeue
      val b = BlockData(blockBuffer, ChunkData.index(chunk, pos))
      for(adj <- pos.getAdjacent()) {
        if(chunk.contains(adj)) {
          val i = ChunkData.index(chunk, adj)
          val a = BlockData(blockBuffer, i)
          val aType = blockFactory.getBlockType(blockFactory.getBlockTypeId(a.w))
          if(aType.isTransparent && a.light + 1 < b.light) {
            a.copy(light = b.light - 1, red = b.red, green = b.green, blue = b.blue).write(blockBuffer, i)
            queue.enqueue(adj)
          }
        } else {
          buffer+=adj
        }
      }

    }
    buffer.toSet
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
      val blockType = blockFactory.getBlockType(i.block.getType)

      val rb = if(blockType.isOrientable) {
        i.block.withOrientation(i.orientation)
      } else {
        i.block
      }
      replaceBlock(ReplaceFilter, i.position, rb, positionMapping) { b =>
        if(b == rb) {
          s ! new InteractResult(i.position, i.block, null)
        } else {
          s ! new InteractResult(i.position, null, null)
        }
      }
    case i: InteractTertiaryUpdate =>
      val filters = tertiaryInteractionFilters :+ self
      val message = i.message
      val p = message.getPosition
      readChunk(p) { block =>
        val b = block.block(positionMapping.get(str(p)), blockFactory.getBlockTypeId(block.w))
        val filters = tertiaryInteractionFilters :+ self
        filters.head ! new InteractTertiaryFilter(
          filters.tail.toArray,
          message.withBlockAtPosition(b).withWorldPhase(true))
      }
    case i: InteractTertiaryFilter if i.getMessage.isWorldPhase =>
      val filters = tertiaryInteractionFilters :+ self
      filters.head ! new InteractTertiaryFilter(filters.tail.toArray, i.getMessage.withWorldPhase(false))
    case s: InteractTertiaryFilter.Skipped =>
      // If first phase was skipped, update the world and return to user
      // This avoids running the second phase
      val i = s.getFilter
      val message = i.getMessage
      val position = message.getPosition
      val blockAtPosition = message.getBlockAtPosition
      /* Update the block with any changes made by the filter */
      replaceBlock(BlockFilterFactory.EVERYTHING, position, blockAtPosition, positionMapping) { b =>
        message.getSender ! new InteractResult(position, message.getBlock, blockAtPosition)
      }
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
      readChunk(p) { block =>
        val b = block.block(positionMapping.get(str(p)), blockFactory.getBlockTypeId(block.w))
        s ! new ViewBlockResult(p, b)
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
      chunks(index(position)) = Some(ChunkData(ChunkData.InitialRevision, data, compressionBuffer))
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

  case class LightUpdate(updated: Set[ChunkPosition], update: Seq[(Position, LightLevel)])
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
