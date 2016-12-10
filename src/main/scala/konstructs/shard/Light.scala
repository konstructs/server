package konstructs.shard

import scala.collection.mutable
import akka.actor.ActorRef
import konstructs.Db
import konstructs.api.{Position, LightLevel, Colour, BlockFactory}

object Light {

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

  // A specific block which ambient light should be removed
  case class AmbientLightRemoval(position: Position, from: Position, ambient: Int)

  // Message to remove ambient light
  case class RemoveAmbientLight(chunk: ChunkPosition, removal: Set[AmbientLightRemoval])

  // A specific position where light needs to be removed
  case class LightRemoval(position: Position, from: Position, light: Int)

  // Message to remove light
  case class RemoveLight(chunk: ChunkPosition, removal: Set[LightRemoval])

  // This function validates lightning of blocks that just changed
  // It handles light sources as well as normal blocks
  def updateLight(positions: Set[(Position, BlockData, BlockData)], chunk: ChunkPosition, db: ActorRef)(
      implicit blockFactory: BlockFactory,
      blockBuffer: Array[Byte]): Int = {
    var bu = 0

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
    for ((position, oldBlock, block) <- positions) {
      // Old block is a light source or was lit
      if (oldBlock.light > 1) {

        // Find all adjacent blocks and add them for light removal
        for (adj <- position.getAdjacent) {
          val adjChunk = ChunkPosition(adj)
          if (adjChunk == chunk) {
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
      if (oldBlock.ambient > 1) {
        // Find all adjacent blocks and add them for light removal
        for (adj <- position.getAdjacent) {
          val adjChunk = ChunkPosition(adj)
          val ambient = if (position.getY > adj.getY && oldBlock.ambient == LightLevel.FULL_ENCODING) {
            oldBlock.ambient
          } else {
            oldBlock.ambient - 1
          }
          if (adjChunk == chunk) {
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
      if (block.light > 1) {
        // Add block to blocks that requires refresh
        refresh += position
      }

      // Iterate through all adjacent blocks
      for (adj <- position.getAdjacent) {
        val adjChunk = ChunkPosition(adj)
        if (adjChunk == chunk) {
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

    bu += removeAmbientLight(RemoveAmbientLight(chunk, removeAmbient.toSet), removeAmbientOthers, refreshAmbient, db)

    // Refresh all lights that requires refresh in this chunk
    bu += refreshAmbientLight(RefreshAmbientLight(chunk, refreshAmbient.toSet), db)

    // Send messages to refresh all other chunks
    refreshAmbientOthers foreach {
      case (chunk, set) =>
        db ! RefreshAmbientLight(chunk, set.toSet)
    }

    // Remove all lights that requires removal in this and other chunks
    // (This function  sends remove messages to other chunks)
    // This also adds blocks that requires refresh due to neighbour removal
    bu += removeLight(RemoveLight(chunk, remove.toSet), removeOthers, refresh, db)

    // Refresh all lights that requires refresh in this chunk
    bu += refreshLight(RefreshLight(chunk, refresh.toSet), db)

    // Send messages to refresh all other chunks
    refreshOthers foreach {
      case (chunk, set) =>
        db ! RefreshLight(chunk, set.toSet)
    }
    return bu
  }

  // Helper method to remove and refresh light
  // This is done when receiving a remove light message
  def removeLight(removal: RemoveLight, db: ActorRef)(implicit blockFactory: BlockFactory,
                                                      blockBuffer: Array[Byte]): Int = {
    val refresh = mutable.Set[Position]()
    var bu = 0
    bu = bu + removeLight(removal, mutable.HashMap[ChunkPosition, mutable.Set[LightRemoval]](), refresh, db)
    bu = bu + refreshLight(RefreshLight(removal.chunk, refresh.toSet), db)
    return bu
  }

  // Removes light and enqueue neighbours that require refresh
  def removeLight(removal: RemoveLight,
                  buffer: mutable.HashMap[ChunkPosition, mutable.Set[LightRemoval]],
                  refresh: mutable.Set[Position],
                  db: ActorRef)(implicit blockFactory: BlockFactory, blockBuffer: Array[Byte]): Int = {
    var bu = 0
    // Queue for BFS search
    val queue = mutable.Queue[LightRemoval]()

    // Add all lights that require removal
    queue ++= removal.removal

    val chunk = removal.chunk

    // BFS flood removal
    while (!queue.isEmpty) {
      val r = queue.dequeue
      val i = ChunkData.index(chunk, r.position)
      val a = BlockData(blockBuffer, i)
      val aType = blockFactory.getBlockType(blockFactory.getBlockTypeId(a.w))

      // If the block is transparent and has light set
      if (aType.isTransparent && a.light != 0) {

        // If the light of the block is equal or smaller to the light to be removed, remove it
        // Else the block needs to be added to refresh list, since it might flood the area where
        // light has been removed with another light
        if (a.light <= r.light) {
          // Remove light from the block
          a.copy(light = 0, red = Colour.WHITE.getRed, green = Colour.WHITE.getGreen, blue = Colour.WHITE.getBlue)
            .write(blockBuffer, i)
          bu = bu + 1
          // If the light to remove is > 1 continue to remove light from all neighbours
          if (r.light > 1) {
            for (adj <- r.position.getAdjacent) {
              if (adj != r.from) {
                val adjChunk = ChunkPosition(adj)
                if (chunk == adjChunk) {
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

    return bu
  }

  // Helper method to remove and refresh light
  // This is done when receiving a remove light message
  def removeAmbientLight(removal: RemoveAmbientLight, db: ActorRef)(implicit blockFactory: BlockFactory,
                                                                    blockBuffer: Array[Byte]): Int = {
    var bu = 0
    val refresh = mutable.Set[Position]()
    bu = bu + removeAmbientLight(removal,
                                 mutable.HashMap[ChunkPosition, mutable.Set[AmbientLightRemoval]](),
                                 refresh,
                                 db)
    bu = bu + refreshAmbientLight(RefreshAmbientLight(removal.chunk, refresh.toSet), db)
    return bu
  }

  // Removes light and enqueue neighbours that require refresh
  def removeAmbientLight(removal: RemoveAmbientLight,
                         buffer: mutable.HashMap[ChunkPosition, mutable.Set[AmbientLightRemoval]],
                         refresh: mutable.Set[Position],
                         db: ActorRef)(implicit blockFactory: BlockFactory, blockBuffer: Array[Byte]): Int = {

    var bu = 0

    // Queue for BFS search
    val queue = mutable.Queue[AmbientLightRemoval]()

    // Add all lights that require removal
    queue ++= removal.removal

    val chunk = removal.chunk

    // BFS flood removal
    while (!queue.isEmpty) {
      val r = queue.dequeue
      val i = ChunkData.index(chunk, r.position)
      val a = BlockData(blockBuffer, i)
      val aTypeId = blockFactory.getBlockTypeId(a.w)
      val aType = blockFactory.getBlockType(aTypeId)

      // If the block is transparent and has light set
      if (aType.isTransparent && a.ambient != 0 && aTypeId.getNamespace != "org/konstructs/space") {

        // If the light of the block is equal or smaller to the light to be removed, remove it
        // Else the block needs to be added to refresh list, since it might flood the area where
        // light has been removed with another light
        if (a.ambient <= r.ambient) {

          bu = bu + 1
          // Remove light from the block
          a.copy(ambient = 0).write(blockBuffer, i)

          // If the light to remove is > 1 continue to remove light from all neighbours
          if (r.ambient > 1) {
            for (adj <- r.position.getAdjacent) {
              if (adj != r.from) {
                val adjChunk = ChunkPosition(adj)
                // TODO: Remove artificial limit at 0
                val ambient = if (r.position.getY > adj.getY && r.ambient == LightLevel.FULL_ENCODING) {
                  r.ambient
                } else {
                  r.ambient - 1
                }
                if (chunk == adjChunk) {
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

    return bu
  }

  // This function looks at the positions given and
  // if the position contains light, tries to propagate
  // it to refresh any updated or newly placed block
  def refreshLight(refresh: RefreshLight, db: ActorRef)(implicit blockFactory: BlockFactory,
                                                        blockBuffer: Array[Byte]): Int = {
    // Blocks where to flood light in this chunk
    val flood = mutable.Set[LightFlood]()

    // Blocks where to flood light in other chunks
    val floodOthers = mutable.HashMap[ChunkPosition, mutable.Set[LightFlood]]()

    val chunk = refresh.chunk

    // Iterate through all blocks that require a refresh
    for (position <- refresh.positions) {
      val i = ChunkData.index(chunk, position)
      val block = BlockData(blockBuffer, i)

      // If block has a light level higher than one
      // it can spread light around
      if (block.light > 1) {
        for (adj <- position.getAdjacent) {
          val adjChunk = ChunkPosition(adj)
          if (adjChunk == chunk) {
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
    return floodLight(FloodLight(chunk, flood.toSet), floodOthers, db)
  }

  def refreshAmbientLight(refreshAmbient: RefreshAmbientLight, db: ActorRef)(implicit blockFactory: BlockFactory,
                                                                             blockBuffer: Array[Byte]): Int = {

    val ambientFlood = mutable.Set[AmbientLightFlood]()
    val ambientFloodOthers = mutable.HashMap[ChunkPosition, mutable.Set[AmbientLightFlood]]()

    val chunk = refreshAmbient.chunk

    // Iterate through all blocks that require a refresh
    for (position <- refreshAmbient.positions) {
      val i = ChunkData.index(chunk, position)
      val block = BlockData(blockBuffer, i)
      val blockTypeId = blockFactory.getBlockTypeId(block.w)
      val blockType = blockFactory.getBlockType(blockTypeId)
      // Block has ambient lightning
      if (blockType.isTransparent && block.ambient > 1) {

        // Find all adjacent blocks and add them for ambient flooding
        for (adj <- position.getAdjacent) {
          val adjChunk = ChunkPosition(adj)
          val ambient = if (position.getY > adj.getY && block.ambient == LightLevel.FULL_ENCODING) {
            block.ambient
          } else {
            block.ambient - 1
          }
          if (adjChunk == chunk) {
            ambientFlood += AmbientLightFlood(adj, position, ambient)
          } else {
            val s = ambientFloodOthers.getOrElseUpdate(adjChunk, mutable.Set[AmbientLightFlood]())
            s += AmbientLightFlood(adj, position, ambient)
          }
        }
      }
    }
    return floodAmbientLight(FloodAmbientLight(chunk, ambientFlood.toSet), ambientFloodOthers, db)
  }

  // Helper method to flood lights received via FloodLight message
  def floodLight(update: FloodLight, db: ActorRef)(implicit blockFactory: BlockFactory,
                                                   blockBuffer: Array[Byte]): Int = {
    return floodLight(update, mutable.HashMap[ChunkPosition, mutable.Set[LightFlood]](), db)
  }

  // This function propagates light by flooding using a BFS
  def floodLight(update: FloodLight, buffer: mutable.HashMap[ChunkPosition, mutable.Set[LightFlood]], db: ActorRef)(
      implicit blockFactory: BlockFactory,
      blockBuffer: Array[Byte]): Int = {

    var bu = 0

    // The BFS queue
    val queue = mutable.Queue[LightFlood]()
    queue ++= update.update

    val chunk = update.chunk

    // The BFS
    while (!queue.isEmpty) {
      val f = queue.dequeue
      val i = ChunkData.index(chunk, f.position)
      val a = BlockData(blockBuffer, i)
      val aType = blockFactory.getBlockType(blockFactory.getBlockTypeId(a.w))

      // If the block has a lower light level than what is to be flooded
      // update it to the new level and continue flooding
      if (aType.isTransparent && a.light < f.light) {
        bu = bu + 1

        a.copy(light = f.light, red = f.red, green = f.green, blue = f.blue).write(blockBuffer, i)

        // If the flood light level is bigger than 1 continue flooding
        if (f.light > 1) {
          for (adj <- f.position.getAdjacent) {
            if (adj != f.from) {
              val adjChunk = ChunkPosition(adj)
              if (chunk == adjChunk) {
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

    return bu
  }

  // Helper method to flood ambient light received via FloodAmbientLight message
  def floodAmbientLight(update: FloodAmbientLight, db: ActorRef)(implicit blockFactory: BlockFactory,
                                                                 blockBuffer: Array[Byte]): Int = {
    return floodAmbientLight(update, mutable.HashMap[ChunkPosition, mutable.Set[AmbientLightFlood]](), db)
  }

  def floodAmbientLight(update: FloodAmbientLight,
                        buffer: mutable.HashMap[ChunkPosition, mutable.Set[AmbientLightFlood]],
                        db: ActorRef)(implicit blockFactory: BlockFactory, blockBuffer: Array[Byte]): Int = {

    var bu = 0

    // The BFS queue
    val queue = mutable.Queue[AmbientLightFlood]()
    queue ++= update.update

    val chunk = update.chunk

    // The BFS
    while (!queue.isEmpty) {
      val f = queue.dequeue
      val i = ChunkData.index(chunk, f.position)
      val a = BlockData(blockBuffer, i)
      val aTypeId = blockFactory.getBlockTypeId(a.w)
      val aType = blockFactory.getBlockType(aTypeId)

      // If the block has a lower light level than what is to be flooded
      // update it to the new level and continue flooding
      if (aType.isTransparent && a.ambient < f.ambient && aTypeId.getNamespace != "org/konstructs/space") {
        bu = bu + 1
        a.copy(ambient = f.ambient).write(blockBuffer, i)

        // If the flood light level is bigger than 1 continue flooding
        if (f.ambient > 1) {
          for (adj <- f.position.getAdjacent) {
            if (adj != f.from) {
              val adjChunk = ChunkPosition(adj)
              val ambient = if (f.position.getY > adj.getY && f.ambient == LightLevel.FULL_ENCODING) {
                f.ambient
              } else {
                f.ambient - 1
              }
              if (chunk == adjChunk) {
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

    return bu
  }

  def refreshChunkAbove(chunk: ChunkPosition): RefreshAmbientLight = {
    val maxChunk = chunk.copy(k = chunk.k + 1)
    val set = (for (x <- 0 until Db.ChunkSize;
                    z <- 0 until Db.ChunkSize) yield {
      maxChunk.position(x, 0, z)
    }) toSet

    RefreshAmbientLight(maxChunk, set)
  }

}
