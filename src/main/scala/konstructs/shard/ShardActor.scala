package konstructs.shard

import java.util.UUID

import scala.collection.mutable
import scala.collection.JavaConverters._

import akka.actor.{ Actor, Stash, ActorRef, Props }

import com.google.gson.reflect.TypeToken

import konstructs.api.{ BinaryLoaded, Position, Block, GsonLoaded,
                        BlockTypeId, BlockFilter, BlockFilterFactory,
                        BlockFactory, BlockState, Direction, Rotation,
                        Orientation, LightLevel, Colour, BlockType,
                        BlockUpdate, Health, ReceiveStack, Stack }
import konstructs.api.messages.{ BoxQuery, BoxQueryResult, ReplaceBlock,
                                 ReplaceBlockResult, ViewBlock, ViewBlockResult,
                                 BlockUpdateEvent, InteractResult, InteractTertiaryFilter,
                                 InteractTertiary }

import konstructs.utils.Scheduled

import konstructs.{ Db, BinaryStorage, JsonStorage, DbActor,
                    GeneratorActor }

class ShardActor(db: ActorRef, shard: ShardPosition, val binaryStorage: ActorRef,
                 val jsonStorage: ActorRef, blockUpdateEvents: Seq[ActorRef],
                 chunkGenerator: ActorRef, blockFactory: BlockFactory,
                 tertiaryInteractionFilters: Seq[ActorRef], universe: ActorRef)
    extends Actor with Stash with Scheduled with BinaryStorage with JsonStorage {
  import ShardActor._
  import GeneratorActor._
  import DbActor._
  import Db._

  val TypeOfPositionMapping = new TypeToken[java.util.Map[String, UUID]](){}.getType
  val ReplaceFilter = BlockFilterFactory.withBlockState(BlockState.LIQUID).or(BlockFilterFactory.withBlockState(BlockState.GAS)).or(BlockFilterFactory.VACUUM)
  val ns = "chunks"

  private val blockBuffer = new Array[Byte](ChunkData.Size)
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
    Colour.WHITE.getRed(), Colour.WHITE.getGreen(), Colour.WHITE.getBlue(),
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
    updateChunk(chunk) { () =>
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
      false /* Indicate that we did not update the chunk */
    }
  }

  def readBlock(pos: Position)(read: BlockData => Unit) = {
    val chunk = ChunkPosition(pos)
    loadChunk(chunk).map { c =>
      val block = c.block(chunk, pos, blockBuffer)
      read(block)
    }
  }

  def updateChunk(chunk: ChunkPosition)(update: () => Boolean) {
    loadChunk(chunk).map { c =>
      c.unpackTo(blockBuffer)
      if(update()) {
        dirty = dirty + chunk
        val data = ChunkData(c.revision + 1, blockBuffer, compressionBuffer)
        chunks(index(chunk)) = Some(data)
        db ! ChunkUpdate(chunk, data)
      }
    }
  }

  def replaceBlocks(chunk: ChunkPosition,
    filter: BlockFilter, blocks: Map[Position, BlockTypeId],
    positionMapping: java.util.Map[String, UUID]) {
    updateChunk(chunk) { () =>
      val events = new java.util.HashMap[Position, BlockUpdate]()
      val updates = mutable.Set[(Position, BlockData, BlockData)]()
      for((position, newTypeId) <- blocks) {
        val i = ChunkData.index(chunk, position)
        val typeId = blockFactory.getBlockTypeId(BlockData.w(blockBuffer, i))
        val oldType = blockFactory.getBlockType(typeId)
        if(filter.matches(typeId, oldType)) {
          val id = positionMapping.remove(str(position))
          val newType = blockFactory.getBlockType(newTypeId)
          val oldBlock = BlockData(blockBuffer, i)
          val newBlock = BlockData(blockFactory.getW(newTypeId), Health.PRISTINE.getHealth(),
            Direction.UP_ENCODING, Rotation.IDENTITY_ENCODING, LightLevel.DARK_ENCODING,
            newType.getLightColour.getRed, newType.getLightColour.getGreen, newType.getLightColour.getBlue,
            newType.getLightLevel.getLevel)
          events.put(position, new BlockUpdate(oldBlock.block(id, typeId), newBlock.block(null, newTypeId)))
          updates += ((position, oldBlock, newBlock))
          newBlock.write(blockBuffer, i)
          if(id != null)
            positionMappingDirty = true
        }
      }
      if(!events.isEmpty) {
        sendEvent(events)
        updateLight(updates.toSet, chunk)
        true // We updated the chunk
      } else {
        false // We didn't update the chunk
      }
    }
  }

  def damageBlock(position: Position, dealing: Block,
    positionMapping: java.util.Map[String, UUID])(ready: (Block, Block) => Unit) {
    val chunk = ChunkPosition(position)
    updateChunk(chunk) { () =>
      val i = ChunkData.index(chunk, position)
      val old = BlockData(blockBuffer, i)
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
      if(receivingHealth.isDestroyed()) {
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
        VacuumData.write(blockBuffer, i)
        updateLight(Set((position, old, VacuumData)), chunk)
        ready(dealingBlock, block)
      } else {
        old.copy(health = receivingHealth.getHealth()).write(blockBuffer, i)
        ready(dealingBlock, null)
      }
      true
    }
  }

  def replaceBlock(filter: BlockFilter, position: Position, block: Block,
    positionMapping: java.util.Map[String, UUID])(ready: Block => Unit) = {
    val chunk = ChunkPosition(position)
    updateChunk(chunk) { () =>
      val i = ChunkData.index(chunk, position)
      val old = BlockData(blockBuffer, i)
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
        val newBlockType = blockFactory.getBlockType(block.getType)
        val newData = BlockData(blockFactory.getW(block.getType()), block, LightLevel.DARK_ENCODING,
          newBlockType.getLightColour, newBlockType.getLightLevel)
        newData.write(blockBuffer, i)
        sendEvent(position, oldBlock, block)
        updateLight(Set((position, old, newData)), chunk)
        true // We updated the chunk
      } else {
        false // We didn't update the chunk
      }
    }
  }

  // This function validates lightning of blocks that just changed
  // It handles light sources as well as normal blocks
  def updateLight(positions: Set[(Position, BlockData, BlockData)], chunk: ChunkPosition) {
    // Blocks that should be refreshed in this chunk
    val refresh = mutable.Set[Position]()
    val refreshAmbient = mutable.Set[Position]()
    // Blocks that should be refreshed in other chunks
    val refreshOthers = mutable.HashMap[ChunkPosition, mutable.Set[Position]]()
    val refreshAmbientOthers = mutable.HashMap[ChunkPosition, mutable.Set[Position]]()

    // Light sources to remove in this chunk
    val remove = mutable.Set[LightRemoval]()
    // Light sources to remove in other chunks
    val removeOthers = mutable.HashMap[ChunkPosition, mutable.Set[LightRemoval]]()

    // Light sources to remove in this chunk
    val removeAmbient = mutable.Set[AmbientLightRemoval]()
    // Light sources to remove in other chunks
    val removeAmbientOthers = mutable.HashMap[ChunkPosition, mutable.Set[AmbientLightRemoval]]()

    // Iterate through all updated blocks
    for((position, oldBlock, block) <- positions) {
      // Old block is a light source or was lit
      if(oldBlock.light > 1) {

        // Find all adjacent blocks and add them for light removal
        for(adj <- position.getAdjacent) {
          val adjChunk = ChunkPosition(adj)
          if(adjChunk == chunk) {
            // Add for removal in this chunk
            remove += LightRemoval(adj, position, oldBlock.light - 1)
          } else {
            // Add for removal in another chunk
            val s = removeOthers.getOrElseUpdate(adjChunk, mutable.Set[LightRemoval]())
            s += LightRemoval(adj, position, oldBlock.light - 1)
          }
        }
      }

      // Old block had ambient light
      if(oldBlock.ambient > 1) {

        // Find all adjacent blocks and add them for light removal
        for(adj <- position.getAdjacent) {
          val adjChunk = ChunkPosition(adj)
          val ambient = if(position.getY > adj.getY && oldBlock.ambient == LightLevel.FULL_ENCODING) {
            oldBlock.ambient
          } else {
            oldBlock.ambient - 1
          }
          if(adjChunk == chunk) {
            // Add for removal in this chunk
            removeAmbient += AmbientLightRemoval(adj, position, ambient)
          } else {
            // Add for removal in another chunk
            val s = removeAmbientOthers.getOrElseUpdate(adjChunk, mutable.Set[AmbientLightRemoval]())
            s += AmbientLightRemoval(adj, position, ambient)
          }
        }
      }

      // The new block has light of itself
      if(block.light > 1) {
        // Add block to blocks that requires refresh
        refresh += position
        refreshAmbient += position
      }

      // Iterate through all adjacent blocks
      for(adj <- position.getAdjacent) {
        val adjChunk = ChunkPosition(adj)
        if(adjChunk == chunk) {
          // Add for refresh in this chunk
          refresh += adj
          refreshAmbient += adj
        } else {
          // Add for refresh in other chunk
          val s = refreshOthers.getOrElseUpdate(adjChunk, mutable.Set[Position]())
          s += adj
          val sa = refreshAmbientOthers.getOrElseUpdate(adjChunk, mutable.Set[Position]())
          sa += adj
        }
      }
    }

    removeAmbientLight(RemoveAmbientLight(chunk, removeAmbient.toSet), removeAmbientOthers, refreshAmbient)

    // Refresh all lights that requires refresh in this chunk
    refreshAmbientLight(RefreshAmbientLight(chunk, refreshAmbient.toSet))

    // Send messages to refresh all other chunks
    refreshAmbientOthers foreach {
      case (chunk, set) =>
        db ! RefreshAmbientLight(chunk, set.toSet)
    }

    // Remove all lights that requires removal in this and other chunks
    // (This function  sends remove messages to other chunks)
    // This also adds blocks that requires refresh due to neighbour removal
    removeLight(RemoveLight(chunk, remove.toSet), removeOthers, refresh)

    // Refresh all lights that requires refresh in this chunk
    refreshLight(RefreshLight(chunk, refresh.toSet))

    // Send messages to refresh all other chunks
    refreshOthers foreach {
      case (chunk, set) =>
        db ! RefreshLight(chunk, set.toSet)
    }

  }

  // Helper method to remove and refresh light
  // This is done when receiving a remove light message
  def removeLight(removal: RemoveLight) {
    val refresh = mutable.Set[Position]()
    removeLight(removal, mutable.HashMap[ChunkPosition, mutable.Set[LightRemoval]](), refresh)
    refreshLight(RefreshLight(removal.chunk, refresh.toSet))
  }

  // Removes light and enqueue neighbours that require refresh
  def removeLight(removal: RemoveLight, buffer: mutable.HashMap[ChunkPosition, mutable.Set[LightRemoval]],
    refresh: mutable.Set[Position]) {

    // Queue for BFS search
    val queue = mutable.Queue[LightRemoval]()

    // Add all lights that require removal
    queue ++= removal.removal

    val chunk = removal.chunk

    // BFS flood removal
    while(!queue.isEmpty) {
      val r = queue.dequeue
      val i = ChunkData.index(chunk, r.position)
      val a = BlockData(blockBuffer, i)
      val aType = blockFactory.getBlockType(blockFactory.getBlockTypeId(a.w))

      // If the block is transparent and has light set
      if(aType.isTransparent && a.light != 0) {

        // If the light of the block is equal or smaller to the light to be removed, remove it
        // Else the block needs to be added to refresh list, since it might flood the area where
        // light has been removed with another light
        if(a.light <= r.light) {
          // Remove light from the block
          a.copy(light = 0, red = Colour.WHITE.getRed, green = Colour.WHITE.getGreen, blue = Colour.WHITE.getBlue).write(blockBuffer, i)

          // If the light to remove is > 1 continue to remove light from all neighbours
          if(r.light > 1) {
            for(adj <- r.position.getAdjacent) {
              if(adj != r.from) {
                val adjChunk = ChunkPosition(adj)
                if(chunk == adjChunk) {
                  // Add light to be removed in this chunk
                  queue.enqueue(LightRemoval(adj, r.position, r.light - 1))
                } else {
                  // Add light to be removed in another chunk
                  val set = buffer.getOrElseUpdate(adjChunk, mutable.Set[LightRemoval]())
                  set += LightRemoval(adj, r.position, r.light - 1)
                }
              }
            }
          }
        } else {
          // Add block to be refreshed
          refresh += r.position
        }
      }
    }

    // Send lights to be removed to other chunks
    buffer foreach {
      case (chunk, set) =>
        db ! RemoveLight(chunk, set.toSet)
    }

  }

  // Helper method to remove and refresh light
  // This is done when receiving a remove light message
  def removeAmbientLight(removal: RemoveAmbientLight) {
    val refresh = mutable.Set[Position]()
    removeAmbientLight(removal, mutable.HashMap[ChunkPosition, mutable.Set[AmbientLightRemoval]](), refresh)
    refreshAmbientLight(RefreshAmbientLight(removal.chunk, refresh.toSet))
  }

  // Removes light and enqueue neighbours that require refresh
  def removeAmbientLight(removal: RemoveAmbientLight,
    buffer: mutable.HashMap[ChunkPosition, mutable.Set[AmbientLightRemoval]],
    refresh: mutable.Set[Position]) {

    // Queue for BFS search
    val queue = mutable.Queue[AmbientLightRemoval]()

    // Add all lights that require removal
    queue ++= removal.removal

    val chunk = removal.chunk

    // BFS flood removal
    while(!queue.isEmpty) {
      val r = queue.dequeue
      val i = ChunkData.index(chunk, r.position)
      val a = BlockData(blockBuffer, i)
      val aType = blockFactory.getBlockType(blockFactory.getBlockTypeId(a.w))

      // If the block is transparent and has light set
      if(aType.isTransparent && a.ambient != 0) {

        // If the light of the block is equal or smaller to the light to be removed, remove it
        // Else the block needs to be added to refresh list, since it might flood the area where
        // light has been removed with another light
        if(a.ambient <= r.ambient) {
          // Remove light from the block
          a.copy(ambient = 0).write(blockBuffer, i)

          // If the light to remove is > 1 continue to remove light from all neighbours
          if(r.ambient > 1) {
            for(adj <- r.position.getAdjacent) {
              if(adj != r.from) {
                val adjChunk = ChunkPosition(adj)
                val ambient = if(r.position.getY > adj.getY && r.ambient == LightLevel.FULL_ENCODING) {
                  r.ambient
                } else {
                  r.ambient - 1
                }
                if(chunk == adjChunk) {
                  // Add light to be removed in this chunk
                  queue.enqueue(AmbientLightRemoval(adj, r.position, ambient))
                } else {
                  // Add light to be removed in another chunk
                  val set = buffer.getOrElseUpdate(adjChunk, mutable.Set[AmbientLightRemoval]())
                  set += AmbientLightRemoval(adj, r.position, ambient)
                }
              }
            }
          }
        } else {
          // Add block to be refreshed
          refresh += r.position
        }
      }
    }

    // Send lights to be removed to other chunks
    buffer foreach {
      case (chunk, set) =>
        db ! RemoveAmbientLight(chunk, set.toSet)
    }

  }

  // This function looks at the positions given and
  // if the position contains light, tries to propagate
  // it to refresh any updated or newly placed block
  def refreshLight(refresh: RefreshLight) {
    // Blocks where to flood light in this chunk
    val flood = mutable.Set[LightFlood]()

    // Blocks where to flood light in other chunks
    val floodOthers = mutable.HashMap[ChunkPosition, mutable.Set[LightFlood]]()

    val chunk = refresh.chunk

    // Iterate through all blocks that require a refresh
    for(position <- refresh.positions) {
      val i = ChunkData.index(chunk, position)
      val block = BlockData(blockBuffer, i)

      // If block has a light level higher than one
      // it can spread light around
      if(block.light > 1) {
        for(adj <- position.getAdjacent) {
          val adjChunk = ChunkPosition(adj)
          if(adjChunk == chunk) {
            // Add light to flood in this chunk
            flood += LightFlood(adj, position, block.light - 1, block.red, block.green, block.blue)
          } else {
            // Add light to flood in other chunk
            val s = floodOthers.getOrElseUpdate(adjChunk, mutable.Set[LightFlood]())
            s += LightFlood(adj, position, block.light - 1, block.red, block.green, block.blue)
          }
        }
      }
    }

    // Flood all lights found
    floodLight(FloodLight(chunk, flood.toSet), floodOthers)
  }

  def refreshAmbientLight(refreshAmbient: RefreshAmbientLight) {

    val ambientFlood = mutable.Set[AmbientLightFlood]()
    val ambientFloodOthers = mutable.HashMap[ChunkPosition, mutable.Set[AmbientLightFlood]]()

    val chunk = refreshAmbient.chunk

    // Iterate through all blocks that require a refresh
    for(position <- refreshAmbient.positions) {
      val i = ChunkData.index(chunk, position)
      val block = BlockData(blockBuffer, i)
      val blockType = blockFactory.getBlockType(blockFactory.getBlockTypeId(block.w))
      // Block has ambient lightning
      if(blockType.isTransparent && block.ambient > 1) {

        // Find all adjacent blocks and add them for ambient flooding
        for(adj <- position.getAdjacent) {
          val adjChunk = ChunkPosition(adj)
          val ambient = if(position.getY > adj.getY && block.ambient == LightLevel.FULL_ENCODING) {
            block.ambient
          } else {
            block.ambient - 1
          }
          if(adjChunk == chunk) {
            ambientFlood += AmbientLightFlood(adj, position, ambient)
          } else {
            val s = ambientFloodOthers.getOrElseUpdate(adjChunk, mutable.Set[AmbientLightFlood]())
            s += AmbientLightFlood(adj, position, ambient)
          }
        }
      }
    }
    floodAmbientLight(FloodAmbientLight(chunk, ambientFlood.toSet), ambientFloodOthers)
  }

  // Helper method to flood lights received via FloodLight message
  def floodLight(update: FloodLight) {
    floodLight(update, mutable.HashMap[ChunkPosition, mutable.Set[LightFlood]]())
  }

  // This function propagates light by flooding using a BFS
  def floodLight(update: FloodLight, buffer: mutable.HashMap[ChunkPosition, mutable.Set[LightFlood]]) {

    // The BFS queue
    val queue = mutable.Queue[LightFlood]()
    queue ++= update.update

    val chunk = update.chunk

    // The BFS
    while(!queue.isEmpty) {
      val f = queue.dequeue
      val i = ChunkData.index(chunk, f.position)
      val a = BlockData(blockBuffer, i)
      val aType = blockFactory.getBlockType(blockFactory.getBlockTypeId(a.w))

      // If the block has a lower light level than what is to be flooded
      // update it to the new level and continue flooding
      if(aType.isTransparent && a.light < f.light) {
        a.copy(light = f.light, red = f.red, green = f.green, blue = f.blue).write(blockBuffer, i)

        // If the flood light level is bigger than 1 continue flooding
        if(f.light > 1) {
          for(adj <- f.position.getAdjacent) {
            if(adj != f.from) {
              val adjChunk = ChunkPosition(adj)
              if(chunk == adjChunk) {
                // Add block to flood in this chunk
                queue.enqueue(LightFlood(adj, f.position, f.light - 1, f.red, f.green, f.blue))
              } else {
                // Add block to flood in other chunk
                val set = buffer.getOrElseUpdate(adjChunk, mutable.Set[LightFlood]())
                set += LightFlood(adj, f.position, f.light - 1, f.red, f.green, f.blue)
              }
            }
          }
        }
      }
    }

    // Send messages to flood all other chunks
    buffer foreach {
      case (chunk, set) =>
        db ! FloodLight(chunk, set.toSet)
    }
  }

  // Helper method to flood ambient light received via FloodAmbientLight message
  def floodAmbientLight(update: FloodAmbientLight) {
    floodAmbientLight(update, mutable.HashMap[ChunkPosition, mutable.Set[AmbientLightFlood]]())
  }

  def floodAmbientLight(update: FloodAmbientLight, buffer: mutable.HashMap[ChunkPosition, mutable.Set[AmbientLightFlood]]) {

    // The BFS queue
    val queue = mutable.Queue[AmbientLightFlood]()
    queue ++= update.update

    val chunk = update.chunk

    // The BFS
    while(!queue.isEmpty) {
      val f = queue.dequeue
      val i = ChunkData.index(chunk, f.position)
      val a = BlockData(blockBuffer, i)
      val aType = blockFactory.getBlockType(blockFactory.getBlockTypeId(a.w))

      // If the block has a lower light level than what is to be flooded
      // update it to the new level and continue flooding
      if(aType.isTransparent && a.ambient < f.ambient) {
        a.copy(ambient = f.ambient).write(blockBuffer, i)

        // If the flood light level is bigger than 1 continue flooding
        if(f.ambient > 1) {
          for(adj <- f.position.getAdjacent) {
            if(adj != f.from) {
              val adjChunk = ChunkPosition(adj)
              val ambient = if(f.position.getY > adj.getY && f.ambient == LightLevel.FULL_ENCODING) {
                f.ambient
              } else {
                f.ambient - 1
              }
              if(chunk == adjChunk) {
                // Add block to flood in this chunk
                queue.enqueue(AmbientLightFlood(adj, f.position, ambient))
              } else {
                // Add block to flood in other chunk
                val set = buffer.getOrElseUpdate(adjChunk, mutable.Set[AmbientLightFlood]())
                set += AmbientLightFlood(adj, f.position, ambient)
              }
            }
          }
        }
      }
    }

    // Send messages to flood all other chunks
    buffer foreach {
      case (chunk, set) =>
        db ! FloodAmbientLight(chunk, set.toSet)
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
      readBlock(p) { block =>
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
          chunks(index(chunk)) = Some(if(version < ChunkData.Version) {
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
    case f: FloodLight =>
      updateChunk(f.chunk) { () =>
        floodLight(f)
        true //Chunk was updated
      }
    case r: RemoveLight =>
      updateChunk(r.chunk) { () =>
        removeLight(r)
        true //Chunk was updated
      }
    case r: RemoveAmbientLight =>
      updateChunk(r.chunk) { () =>
        removeAmbientLight(r)
        true //Chunk was updated
      }
    case f: FloodAmbientLight =>
      updateChunk(f.chunk) { () =>
        floodAmbientLight(f)
        true //Chunk was updated
      }
    case r: RefreshLight =>
      updateChunk(r.chunk) { () =>
        refreshLight(r)
        true //Chunk was updated
      }
    case r: RefreshAmbientLight =>
      updateChunk(r.chunk) { () =>
        refreshAmbientLight(r)
        true //Chunk was updated
      }
  }

}

object ShardActor {

  // A specific block which should be flooded by ambient light
  case class AmbientLightFlood(position: Position, from: Position, ambient: Int)

  // Message to flood ambient light
  case class FloodAmbientLight(chunk: ChunkPosition, update: Set[AmbientLightFlood])

  // A specific block which should be flooded by light
  case class LightFlood(position: Position, from: Position, light: Int, red: Int, green: Int, blue: Int)

  // Message to flood light
  case class FloodLight(chunk: ChunkPosition, update: Set[LightFlood])

  // Message to refresh light in blocks
  case class RefreshLight(chunk: ChunkPosition, positions: Set[Position])

  // Message to refresh ambient light in blocks
  case class RefreshAmbientLight(chunk: ChunkPosition, positions: Set[Position])


  case class AmbientLightRemoval(position: Position, from: Position, ambient: Int)
  case class RemoveAmbientLight(chunk: ChunkPosition, removal: Set[AmbientLightRemoval])

  // A specific position where light needs to be removed
  case class LightRemoval(position: Position, from: Position, light: Int)
  // Message to remove light
  case class RemoveLight(chunk: ChunkPosition, removal: Set[LightRemoval])

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
