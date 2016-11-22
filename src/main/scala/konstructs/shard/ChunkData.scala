package konstructs.shard

import akka.util.ByteString

import konstructs.api.{Position, LightLevel}
import konstructs.utils.compress
import konstructs.Db

case class ChunkData(version: Int, data: ByteString) {
  import ChunkData._

  val revision = readRevision(data, 2)

  def unpackTo(blockBuffer: Array[Byte], compressionBuffer: Array[Byte]) {
    data.copyToArray(compressionBuffer)
    val size = compress.inflate(compressionBuffer, blockBuffer, Header, data.size - Header)
  }

  def block(c: ChunkPosition, p: Position, blockBuffer: Array[Byte], compressionBuffer: Array[Byte]): BlockData = {
    unpackTo(blockBuffer, compressionBuffer)
    BlockData(blockBuffer, index(c, p))
  }

}

object ChunkData {
  import BlockData._
  import Db._

  val InitialRevision = 1
  val Size = ChunkSize * ChunkSize * ChunkSize * BlockData.Size
  val RevisionSize = 4
  val Version2Header = 2 + RevisionSize
  val Version1Header = 2
  val Header = Version2Header
  val Version = 3.toByte

  /* Writes revision as a Little Endian 4 byte unsigned integer
   * Long is required since all Java types are signed
   */
  def writeRevision(data: Array[Byte], revision: Long, offset: Int) {
    if (revision > 4294967295L) {
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
  def readRevision(data: ByteString, offset: Int): Long = {
    (data(offset + 0) & 0xFF).toLong +
      ((data(offset + 1) & 0xFF) << 8).toLong +
      ((data(offset + 2) & 0xFF) << 16).toLong +
      ((data(offset + 3) & 0x7F) << 24).toLong
  }

  def apply(revision: Long, blocks: Array[Byte], buffer: Array[Byte]): ChunkData = {
    val size = compress.deflate(blocks, buffer, Header)
    buffer(0) = Version
    buffer(1) = 0.toByte
    writeRevision(buffer, revision, 2)
    apply(Version, ByteString.fromArray(buffer, 0, size))
  }

  def loadOldFormat(version: Int,
                    data: ByteString,
                    blockBuffer: Array[Byte],
                    compressionBuffer: Array[Byte],
                    chunk: ChunkPosition,
                    spaceVacuum: Int): ChunkData = {

    data.copyToArray(compressionBuffer)
    if (version == 1) {
      val size = compress.inflate(compressionBuffer, blockBuffer, Version1Header, data.size - Version1Header)
      convertFromOldFormat1(blockBuffer, size)
    } else {
      val size = compress.inflate(compressionBuffer, blockBuffer, Header, data.size - Header)
      convertFromOldFormat2(blockBuffer, size)
    }
    if (!inOldWorld(chunk)) {
      updateVacuumToSpaceVacuum(blockBuffer, spaceVacuum)
    }
    apply(0, blockBuffer, compressionBuffer)
  }

  private def inOldWorld(chunk: ChunkPosition): Boolean =
    chunk.p >= -48 && chunk.p < 48 &&
      chunk.q >= -48 && chunk.q < 48 &&
      chunk.k >= 0 && chunk.k < 16

  private def updateVacuumToSpaceVacuum(buf: Array[Byte], spaceVacuum: Int) {
    for (i <- 0 until ChunkSize * ChunkSize * ChunkSize) {
      if (BlockData.w(buf, i) == 0) {
        BlockData.writeW(buf, i, spaceVacuum)
        BlockData.writeLight(buf, i, LightLevel.FULL_ENCODING, 0, 0, 0, LightLevel.DARK_ENCODING)
      }
    }
  }

  private def convertFromOldFormat1(buf: Array[Byte], size: Int) {
    val tmp = java.util.Arrays.copyOf(buf, size)
    for (i <- 0 until size) {
      buf(i * BlockData.Size) = tmp(i)
      buf(i * BlockData.Size + 1) = 0.toByte
      buf(i * BlockData.Size + 2) = 0xFF.toByte
      buf(i * BlockData.Size + 3) = 0x07.toByte
      buf(i * BlockData.Size + 4) = 0x00.toByte
      buf(i * BlockData.Size + 5) = 0x00.toByte
      buf(i * BlockData.Size + 6) = 0x00.toByte
    }
  }

  private def convertFromOldFormat2(buf: Array[Byte], size: Int) {
    val tmp = java.util.Arrays.copyOf(buf, size)
    for (i <- 0 until (size / 4)) {
      buf(i * BlockData.Size) = tmp(i * 4)
      buf(i * BlockData.Size + 1) = tmp(i * 4 + 1)
      buf(i * BlockData.Size + 2) = tmp(i * 4 + 2)
      buf(i * BlockData.Size + 3) = tmp(i * 4 + 3)
      buf(i * BlockData.Size + 4) = 0x00.toByte
      buf(i * BlockData.Size + 5) = 0x00.toByte
      buf(i * BlockData.Size + 6) = 0x00.toByte
    }
  }

  def index(x: Int, y: Int, z: Int): Int =
    x + y * ChunkSize + z * ChunkSize * ChunkSize

  def index(c: ChunkPosition, p: Position): Int = {
    val x = p.getX - c.p * ChunkSize
    val y = p.getY - c.k * ChunkSize
    val z = p.getZ - c.q * ChunkSize
    index(x, y, z)
  }

}
