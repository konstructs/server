package konstructs.shard

import java.util.UUID

import scala.collection.mutable

import akka.actor.{Actor, Stash, ActorRef, Props}
import akka.util.ByteString

import com.google.gson.reflect.TypeToken

import konstructs.api.{
  BinaryLoaded,
  Position,
  Block,
  GsonLoaded,
  BlockTypeId,
  BlockFilter,
  BlockFilterFactory,
  BlockFactory,
  BlockState,
  Direction,
  Rotation,
  Orientation,
  LightLevel,
  Colour,
  BlockType,
  BlockUpdate,
  Health,
  Stack,
  MetricId
}
import konstructs.api.messages.{
  BoxQuery,
  BoxQueryResult,
  ReceiveStack,
  ReplaceBlock,
  ReplaceBlockResult,
  ViewBlock,
  ViewBlockResult,
  BlockUpdateEvent,
  InteractResult,
  InteractTertiaryFilter,
  InteractTertiary,
  IncreaseMetric
}

import konstructs.utils.Scheduled

import konstructs.{Db, BinaryStorage, JsonStorage, DbActor, GeneratorActor}

class ShardActor(db: ActorRef,
                 shard: ShardPosition,
                 val binaryStorage: ActorRef,
                 val jsonStorage: ActorRef,
                 blockUpdateEvents: Seq[ActorRef],
                 chunkGenerator: ActorRef,
                 tertiaryInteractionFilters: Seq[ActorRef],
                 universe: ActorRef,
                 implicit val blockFactory: BlockFactory)
    extends Actor
    with Stash
    with Scheduled
    with BinaryStorage
    with JsonStorage {
  import ShardActor._
  import GeneratorActor._
  import DbActor._
  import Db._
  import Light._

  val TypeOfPositionMapping = new TypeToken[java.util.Map[String, UUID]]() {}.getType
  val ReplaceFilter = BlockFilterFactory
    .withBlockState(BlockState.LIQUID)
    .or(BlockFilterFactory.withBlockState(BlockState.GAS))
    .or(BlockFilterFactory.VACUUM)
  val ns = "chunks"

  private implicit val blockBuffer = new Array[Byte](ChunkData.Size)
  private val compressionBuffer = new Array[Byte](ChunkData.Size + ChunkData.Header)
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
    LightLevel.DARK_ENCODING,
    Colour.WHITE.getRed(),
    Colour.WHITE.getGreen(),
    Colour.WHITE.getBlue(),
    LightLevel.DARK_ENCODING
  )
  private val VacuumBlock = Block.create(BlockTypeId.VACUUM)
  private val toolDurabilityBonus = 10.0f
  private val SpaceVacuumW = blockFactory.getW(BlockTypeId.fromString("org/konstructs/space/vacuum"))

  schedule(5000, StoreChunks)

  val BlockUpdatedMetric = MetricId.fromString("org/konstructs/updated-blocks")
  val BlockQueriedMetric = MetricId.fromString("org/konstructs/queried-blocks")

  val ChunkDecompressedMetric = MetricId.fromString("org/konstructs/decompressed-chunks")
  val ChunkCompressedMetric = MetricId.fromString("org/konstructs/compressed-chunks")
  val ChunkSentMetric = MetricId.fromString("org/konstructs/sent-chunks")

  val LightUpdatedMetric = MetricId.fromString("org/konstructs/updated-blocks-light")

  def sendEvent(events: java.util.Map[Position, BlockUpdate]) {
    universe ! new IncreaseMetric(BlockUpdatedMetric, events.size)
    val msg = new BlockUpdateEvent(events)
    for (l <- blockUpdateEvents) {
      l ! msg
    }
  }

  def sendEvent(position: Position, from: Block, to: Block) {
    val events = new java.util.HashMap[Position, BlockUpdate]()
    events.put(position, new BlockUpdate(from, to))
    sendEvent(events)
  }

  def loadChunk(chunk: ChunkPosition): Option[ChunkData] = {
    val i = index(chunk, shard)
    val blocks = chunks(i)
    if (blocks != null) {
      if (!blocks.isDefined)
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
    updateChunk(chunk) { () =>
      val data = new Array[BlockTypeId](box.getNumberOfBlocks + 1)
      for (x <- box.getFrom.getX until box.getUntil.getX;
           y <- box.getFrom.getY until box.getUntil.getY;
           z <- box.getFrom.getZ until box.getUntil.getZ) {
        val p = new Position(x, y, z)
        data(box.arrayIndex(p)) = blockFactory.getBlockTypeId(BlockData.w(blockBuffer, ChunkData.index(chunk, p)))

      }
      sender ! new BoxQueryResult(box, data)
      universe ! new IncreaseMetric(BlockQueriedMetric, box.getNumberOfBlocks)
      false /* Indicate that we did not update the chunk */
    }
  }

  def readBlock(pos: Position)(read: BlockData => Unit) = {
    val chunk = ChunkPosition(pos)
    loadChunk(chunk).map { c =>
      val block = c.block(chunk, pos, blockBuffer, compressionBuffer)
      read(block)
    }
  }

  def updateChunk(chunk: ChunkPosition)(update: () => Boolean) {
    loadChunk(chunk).map { c =>
      c.unpackTo(blockBuffer, compressionBuffer)
      universe ! new IncreaseMetric(ChunkDecompressedMetric, 1)
      if (update()) {
        dirty = dirty + chunk
        val data = ChunkData(c.revision + 1, blockBuffer, compressionBuffer)
        chunks(index(chunk, shard)) = Some(data)
        db ! ChunkUpdate(chunk, data)
        universe ! new IncreaseMetric(ChunkCompressedMetric, 1)
      }
    }
  }

  def replaceBlocks(chunk: ChunkPosition,
                    filter: BlockFilter,
                    blocks: Map[Position, BlockTypeId],
                    positionMapping: java.util.Map[String, UUID]) {
    updateChunk(chunk) { () =>
      val events = new java.util.HashMap[Position, BlockUpdate]()
      val updates = mutable.Set[(Position, BlockData, BlockData)]()
      for ((position, newTypeId) <- blocks) {
        val i = ChunkData.index(chunk, position)
        val typeId = blockFactory.getBlockTypeId(BlockData.w(blockBuffer, i))
        val oldType = blockFactory.getBlockType(typeId)
        if (filter.matches(typeId, oldType)) {
          val id = positionMapping.remove(str(position))
          val newType = blockFactory.getBlockType(newTypeId)
          val oldBlock = BlockData(blockBuffer, i)
          val newBlock = BlockData(blockFactory.getW(newTypeId),
                                   Health.PRISTINE.getHealth(),
                                   Direction.UP_ENCODING,
                                   Rotation.IDENTITY_ENCODING,
                                   LightLevel.DARK_ENCODING,
                                   newType.getLightColour.getRed,
                                   newType.getLightColour.getGreen,
                                   newType.getLightColour.getBlue,
                                   newType.getLightLevel.getLevel)
          events.put(position, new BlockUpdate(oldBlock.block(id, typeId), newBlock.block(null, newTypeId)))
          updates += ((position, oldBlock, newBlock))
          newBlock.write(blockBuffer, i)
          if (id != null)
            positionMappingDirty = true
        }
      }
      if (!events.isEmpty) {
        sendEvent(events)
        universe ! new IncreaseMetric(LightUpdatedMetric, updateLight(updates.toSet, chunk, db))
        true // We updated the chunk
      } else {
        false // We didn't update the chunk
      }
    }
  }

  def damageBlock(position: Position, dealing: Block, positionMapping: java.util.Map[String, UUID])(
      ready: (Block, Block) => Unit) {
    val chunk = ChunkPosition(position)
    updateChunk(chunk) { () =>
      val i = ChunkData.index(chunk, position)
      val old = BlockData(blockBuffer, i)
      val receivingTypeId = blockFactory.getBlockTypeId(old.w)
      val receivingType = blockFactory.getBlockType(receivingTypeId)
      val dealingType = blockFactory.getBlockType(dealing.getType())
      val dealingDamage = dealingType.getDamageWithMultiplier(receivingTypeId, receivingType)
      val receivingHealth = Health.get(old.health).damage(dealingDamage, receivingType.getDurability())
      val dealingHealth =
        dealing.getHealth().damage(receivingType.getDamage(), dealingType.getDurability() * toolDurabilityBonus)
      val dealingBlock = if (dealingHealth.isDestroyed) {
        null
      } else {
        dealing.withHealth(dealingHealth)
      }
      if (receivingHealth.isDestroyed()) {
        val id = positionMapping.remove(str(position))
        val oldBlock = old.block(id, receivingTypeId)
        if (id != null)
          positionMappingDirty = true
        val block = if (receivingType.getDestroyedAs() == BlockTypeId.SELF) {
          oldBlock.withOrientation(Orientation.NORMAL).withHealth(Health.PRISTINE)
        } else {
          oldBlock
            .withOrientation(Orientation.NORMAL)
            .withHealth(Health.PRISTINE)
            .withType(receivingType.getDestroyedAs())
        }
        sendEvent(position, oldBlock, VacuumBlock)
        VacuumData.write(blockBuffer, i)
        universe ! new IncreaseMetric(LightUpdatedMetric, updateLight(Set((position, old, VacuumData)), chunk, db))
        ready(dealingBlock, block)
      } else {
        old.copy(health = receivingHealth.getHealth()).write(blockBuffer, i)
        universe ! new IncreaseMetric(BlockUpdatedMetric, 1)
        ready(dealingBlock, null)
      }
      true
    }
  }

  def replaceBlock(filter: BlockFilter,
                   position: Position,
                   block: Block,
                   positionMapping: java.util.Map[String, UUID])(ready: Block => Unit) = {
    val chunk = ChunkPosition(position)
    updateChunk(chunk) { () =>
      val i = ChunkData.index(chunk, position)
      val old = BlockData(blockBuffer, i)
      val typeId = blockFactory.getBlockTypeId(old.w)
      val blockType = blockFactory.getBlockType(typeId)
      if (filter.matches(typeId, blockType)) {
        val oldId = if (block.getId() != null) {
          positionMappingDirty = true
          positionMapping.put(str(position), block.getId())
        } else {
          val id = positionMapping.remove(str(position))
          if (id != null)
            positionMappingDirty = true
          id
        }
        val oldBlock = old.block(oldId, typeId)
        ready(oldBlock)
        if (oldBlock != block) {
          val newBlockType = blockFactory.getBlockType(block.getType)
          val newData = BlockData(blockFactory.getW(block.getType()),
                                  block,
                                  LightLevel.DARK_ENCODING,
                                  newBlockType.getLightColour,
                                  newBlockType.getLightLevel)
          newData.write(blockBuffer, i)
          sendEvent(position, oldBlock, block)
          universe ! new IncreaseMetric(LightUpdatedMetric, updateLight(Set((position, old, newData)), chunk, db))
          true // We updated the chunk
        } else {
          false
        }
      } else {
        false // We didn't update the chunk
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
        if (damaged != null) {
          s ! new ReceiveStack(Stack.createFromBlock(damaged))
        }
        if (block != null)
          s ! new InteractResult(i.position, using, damaged)
        else
          s ! new InteractResult(i.position, null, damaged)
      }
    case i: InteractSecondaryUpdate =>
      val s = sender
      val blockType = blockFactory.getBlockType(i.block.getType)

      val rb = if (blockType.isOrientable) {
        i.block.withOrientation(i.orientation)
      } else {
        i.block
      }
      replaceBlock(ReplaceFilter, i.position, rb, positionMapping) { b =>
        if (b == rb) {
          s ! new InteractResult(i.position, i.block, null)
        } else {
          s ! new InteractResult(i.position, null, null)
        }
      }
    case i: InteractTertiaryUpdate =>
      val filters = tertiaryInteractionFilters :+ self
      val message = i.message
      val p = message.getPosition
      readBlock(p) { block =>
        val b = block.block(positionMapping.get(str(p)), blockFactory.getBlockTypeId(block.w))
        val filters = tertiaryInteractionFilters :+ self
        filters.head ! new InteractTertiaryFilter(filters.tail.toArray,
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
        if (b == r.getBlock) {
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
      readBlock(p) { block =>
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
          chunks(index(chunk, shard)) = Some(if (version < ChunkData.Version) {
            dirty = dirty + chunk
            db ! refreshChunkAbove(chunk)
            ChunkData.loadOldFormat(version, data, blockBuffer, compressionBuffer, chunk, SpaceVacuumW)
          } else {
            ChunkData(version, data)
          })
          unstashAll()
        case None =>
          chunkGenerator ! Generate(chunk)
      }
    case Generated(position, data) =>
      chunks(index(position, shard)) = Some(ChunkData(ChunkData.InitialRevision, data, compressionBuffer))
      dirty = dirty + position
      unstashAll()
    case StoreChunks =>
      dirty.map { chunk =>
        chunks(index(chunk, shard)).map { c =>
          storeBinary(chunkId(chunk), c.data)
        }
      }
      dirty = Set()
      if (positionMappingDirty) {
        storeGson(positionMappingFile, gson.toJsonTree(positionMapping, TypeOfPositionMapping))
        positionMappingDirty = false
      }
    case f: FloodLight =>
      updateChunk(f.chunk) { () =>
        val updated = floodLight(f, db)
        if (updated > 0) {
          universe ! new IncreaseMetric(LightUpdatedMetric, updated)
          true //Chunk was updated
        } else {
          false
        }
      }
    case r: RemoveLight =>
      updateChunk(r.chunk) { () =>
        val updated = removeLight(r, db)
        if (updated > 0) {
          universe ! new IncreaseMetric(LightUpdatedMetric, updated)
          true //Chunk was updated
        } else {
          false
        }
      }
    case r: RemoveAmbientLight =>
      updateChunk(r.chunk) { () =>
        val updated = removeAmbientLight(r, db)
        if (updated > 0) {
          universe ! new IncreaseMetric(LightUpdatedMetric, updated)
          true //Chunk was updated
        } else {
          false
        }
      }
    case f: FloodAmbientLight =>
      updateChunk(f.chunk) { () =>
        val updated = floodAmbientLight(f, db)
        if (updated > 0) {
          universe ! new IncreaseMetric(LightUpdatedMetric, updated)
          true //Chunk was updated
        } else {
          false
        }
      }
    case r: RefreshLight =>
      updateChunk(r.chunk) { () =>
        val updated = refreshLight(r, db)
        if (updated > 0) {
          universe ! new IncreaseMetric(LightUpdatedMetric, updated)
          true //Chunk was updated
        } else {
          false
        }
      }
    case r: RefreshAmbientLight =>
      updateChunk(r.chunk) { () =>
        val updated = refreshAmbientLight(r, db)
        if (updated > 0) {
          universe ! new IncreaseMetric(LightUpdatedMetric, updated)
          true //Chunk was updated
        } else {
          false
        }
      }
  }

}

object ShardActor {
  case object StoreChunks

  def shardId(shard: ShardPosition) = s"${shard.m}-${shard.n}-${shard.o}"

  def positionMappingFile(shardId: String) = s"${shardId}-position-mapping"

  def str(p: Position) = s"${p.getX}-${p.getY}-${p.getZ}"

  case class ReplaceBlocks(chunk: ChunkPosition, filter: BlockFilter, blocks: Map[Position, BlockTypeId])

  def index(c: ChunkPosition, shard: ShardPosition): Int = {
    val local = shard.local(c)
    local.p + local.q * Db.ShardSize + local.k * Db.ShardSize * Db.ShardSize
  }

  def props(db: ActorRef,
            shard: ShardPosition,
            binaryStorage: ActorRef,
            jsonStorage: ActorRef,
            blockUpdateEvents: Seq[ActorRef],
            chunkGenerator: ActorRef,
            blockFactory: BlockFactory,
            tertiaryInteractionFilters: Seq[ActorRef],
            universe: ActorRef) =
    Props(classOf[ShardActor],
          db,
          shard,
          binaryStorage,
          jsonStorage,
          blockUpdateEvents,
          chunkGenerator,
          tertiaryInteractionFilters,
          universe,
          blockFactory)
}
