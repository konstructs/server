package konstructs

import scala.collection.mutable

import akka.actor.{ Actor, Stash, ActorRef, Props }

import konstructs.api._

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

case class ChunkData(blocks: Array[Byte], faces: OpaqueFaces) {
  import ChunkData._

  def unpackTo(blockBuffer: Array[Byte]) {
    val size = compress.inflate(blocks, blockBuffer)
    assert(size == blockBuffer.size)
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
  val ChunkEnd = Db.ChunkSize - 1

  def apply(blocks: Array[Byte], buffer: Array[Byte]): ChunkData = {
    val compressed = compress.deflate(blocks, buffer)
    apply(compressed, OpaqueFaces(blocks))
  }

  def apply(blocks: Array[Byte], faces: Byte) {
    apply(blocks, OpaqueFaces(faces))
  }
  /*
   * Removes all blocks which are fully surrounded by opaque blocks
   * except all blocks on the edges. The blocks on the edges are
   * adjacent to blocks in other chunk so it is not efficient to
   * query for them.
   */
  def removeInvisibleBlocks(in: Array[Byte], out: Array[Byte]) {
    for(
      x <- 0 until Db.ChunkSize;
      y <- 0 until Db.ChunkSize;
      z <- 0 until Db.ChunkSize
    ) {
      val i = index(x,y,z)
      if(x != 0 && x != ChunkEnd && y != 0 && y != ChunkEnd && z != 0 && z != ChunkEnd) {
        val f1 = isOpaque(in(index(x - 1, y, z)))
        val f2 = isOpaque(in(index(x + 1, y, z)))
        val f3 = isOpaque(in(index(x, y - 1, z)))
        val f4 = isOpaque(in(index(x, y + 1, z)))
        val f5 = isOpaque(in(index(x, y, z - 1)))
        val f6 = isOpaque(in(index(x, y, z + 1)))
        if(f1 && f2 && f3 && f4 && f5 && f6) {
          out(i) = 0.toByte
        } else {
          out(i) = in(i)
        }
      } else {
        out(i) = in(i)
      }
    }
  }

  def isOpaque(w: Byte): Boolean = if(w == 0) { false } else { true }

  def index(x: Int, y: Int, z: Int): Int =
    x + y * Db.ChunkSize + z * Db.ChunkSize * Db.ChunkSize

  def index(c: ChunkPosition, p: Position): Int = {
    val x = p.x - c.p * Db.ChunkSize
    val y = p.y - c.k * Db.ChunkSize
    val z = p.z - c.q * Db.ChunkSize
    index(x, y, z)
  }

}

class ShardActor(db: ActorRef, shard: ShardPosition, val binaryStorage: ActorRef, chunkGenerator: ActorRef)
    extends Actor with Stash with utils.Scheduled with BinaryStorage {
  import ShardActor._
  import BinaryStorage._
  import GeneratorActor._
  import DbActor._
  import PlayerActor.ReceiveBlock
  import Db._
  val Blocks = "blocks"
  val Faces = "faces"
  val ns = "chunks"

  private val blockBuffer = new Array[Byte](ChunkSize * ChunkSize * ChunkSize)
  private val compressionBuffer = new Array[Byte](ChunkSize * ChunkSize * ChunkSize)
  private val chunks = new Array[Option[ChunkData]](ShardSize * ShardSize * ShardSize)

  private var dirty: Set[ChunkPosition] = Set()

  private def chunkId(c: ChunkPosition): String =
    s"${c.p}/${c.q}/${c.k}"

  private def chunkFromId(id: String): ChunkPosition = {
    val pqk = id.split('/').map(_.toInt)
    ChunkPosition(pqk(0), pqk(1), pqk(2))
  }

  schedule(5000, StoreChunks)

  def loadChunk(chunk: ChunkPosition): Option[ChunkData] = {
    val i = index(chunk)
    val blocks = chunks(i)
    if(blocks != null) {
      if(!blocks.isDefined)
        stash()
      blocks
    } else {
      loadBinaries(chunkId(chunk), Set(Blocks, Faces))
      chunks(i) = None
      stash()
      None
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
      chunks(index(chunk)) = Some(ChunkData(blockBuffer, compressionBuffer))
    }
  }

  def receive() = {
    case SendBlocks(chunk, _) =>
      val s = sender
      loadChunk(chunk).map { c =>
        s ! BlockList(chunk, c)
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
    case GetBlock(p) =>
      val s = sender
      readChunk(p) { block =>
        s ! BlockPosition(p, block.toInt)
      }
    case DestroyBlock(p) =>
      val s = sender
      updateChunk(p) { old =>
        s ! ReceiveBlock(old)
        db ! BlockUpdate(p, old.toInt, 0)
        0
      }
    case BinariesLoaded(id, data) =>
      val chunk = chunkFromId(id)
      val blocksOption = data.getOrElse(Blocks, None)
      val facesOption = data.getOrElse(Faces, None)
        (blocksOption, facesOption) match {
        case (Some(blocks), Some(faces)) =>
          chunks(index(chunk)) = Some(ChunkData(blocks, OpaqueFaces(faces(0))))
          unstashAll()
        case _ =>
          chunkGenerator ! Generate(chunk)
      }
    case Generated(position, data) =>
      chunks(index(position)) = Some(ChunkData(data, compressionBuffer))
      dirty = dirty + position
      unstashAll()
    case StoreChunks =>
      dirty.map { chunk =>
        chunks(index(chunk)).map { c =>
          storeBinaries(chunkId(chunk), Map(Blocks -> c.blocks, Faces -> Array(c.faces.toByte)))
        }
      }
      dirty = Set()
  }

}

object ShardActor {
  case object StoreChunks

  def index(c: ChunkPosition): Int = {
    val lp = math.abs(c.p % Db.ShardSize)
    val lq = math.abs(c.q % Db.ShardSize)
    val lk = math.abs(c.k % Db.ShardSize)
    lp + lq * Db.ShardSize + lk * Db.ShardSize * Db.ShardSize
  }

  def props(db: ActorRef, shard: ShardPosition, binaryStorage: ActorRef, chunkGenerator: ActorRef) =
    Props(classOf[ShardActor], db, shard, binaryStorage, chunkGenerator)
}
