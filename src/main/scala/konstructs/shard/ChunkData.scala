package konstructs.shard

import konstructs.api.Position
import konstructs.utils.compress
import konstructs.Db

case class ChunkData(version: Int, data: Array[Byte]) {
  import ChunkData._

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
    val compressed = compress.deflate(blocks, buffer, Header)
    compressed(0) = Version
    compressed(1) = 0.toByte
    writeRevision(compressed, revision, 2)
    apply(Version, compressed)
  }

  def loadOldFormat(version: Int, data: Array[Byte], blockBuffer: Array[Byte], compressionBuffer: Array[Byte]): ChunkData = {

    if(version == 1) {
      val size = compress.inflate(data, blockBuffer, Version1Header, data.size - Version1Header)
      convertFromOldFormat1(blockBuffer, size)
    } else {
      val size = compress.inflate(data, blockBuffer, Header, data.size - Header)
      convertFromOldFormat2(blockBuffer, size)
    }
    apply(0, blockBuffer, compressionBuffer)
  }

  private def convertFromOldFormat1(buf: Array[Byte], size: Int) {
    val tmp = java.util.Arrays.copyOf(buf, size)
    for(i <- 0 until size) {
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
    for(i <- 0 until (size / 4)) {
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
