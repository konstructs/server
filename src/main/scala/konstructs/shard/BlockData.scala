package konstructs.shard

import java.util.UUID

import konstructs.api.{ Position, Block,
                        BlockTypeId, Direction, Rotation,
                        Orientation, LightLevel, Colour,
                        Health }
import konstructs.Db

case class BlockData(w: Int, health: Int, direction: Int, rotation: Int, ambient: Int,
  red: Int, green: Int, blue: Int, light: Int) {
  def write(data: Array[Byte], i: Int) {
    BlockData.write(data, i, w, health, direction, rotation, ambient, red, green, blue, light)
  }

  def block(id: UUID, blockTypeId: BlockTypeId) =
    new Block(id, blockTypeId, Health.get(health), Orientation.get(direction, rotation))
}

object BlockData {

  val Size = 7

  def w(data: Array[Byte], i: Int): Int =
    (data(i * Size) & 0xFF) + ((data(i * Size + 1) & 0xFF) << 8)

  def hp(data: Array[Byte], i: Int): Int =
    (data(i * Size + 2) & 0xFF) + ((data(i * Size + 3) & 0x07) << 8)

  def direction(data: Array[Byte], i: Int): Int =
    ((data(i * Size + 3) & 0xE0) >> 5)

  def rotation(data: Array[Byte], i: Int): Int =
    ((data(i * Size + 3) & 0x18) >> 3)

  def ambientLight(data: Array[Byte], i: Int): Int =
    ((data(i * Size + 4) & 0x0F))

  def red(data: Array[Byte], i: Int): Int =
    ((data(i * Size + 4) & 0xF0) >> 4)

  def green(data: Array[Byte], i: Int): Int =
    ((data(i * Size + 5) & 0x0F))

  def blue(data: Array[Byte], i: Int): Int =
    ((data(i * Size + 5) & 0xF0) >> 4)

  def light(data: Array[Byte], i: Int): Int =
    ((data(i * Size + 6) & 0x0F))

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
    data(i * Size) = (w & 0xFF).toByte
    data(i * Size + 1) = ((w >> 8) & 0xFF).toByte
  }

  def writeHealthAndOrientation(data: Array[Byte], i: Int, health: Int, direction: Int, rotation: Int) {
    data(i * Size + 2) = (health & 0xFF).toByte
    val b = (((direction << 5) & 0xE0) + ((rotation << 3) & 0x18) + ((health >> 8) & 0x07)).toByte
    data(i * Size + 3) = b
  }

  def writeLight(data: Array[Byte], i: Int, ambient: Int, red: Int, green: Int, blue: Int, light: Int) {
    val b4 = ((ambient & 0x0F) + ((red << 4) & 0xF0)).toByte
    data(i * Size + 4) = b4
    val b5 = ((green & 0x0F) + ((blue << 4) & 0xF0)).toByte
    data(i * Size + 5) = b5
    data(i * Size + 6) = (light & 0x0F).toByte
  }

}
