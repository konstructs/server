package konstructs

import scala.util.Try
import com.sksamuel.scrimage.Image

trait HeightMap extends PartialFunction[Position, Int]

trait LocalHeightMap {
  def get(local: Position): Int
  def sizeX: Int
  def sizeZ: Int
}

case class FlatHeightMap(height: Int) extends HeightMap {
  def apply(pos: Position) = height
  def isDefinedAt(pos: Position) = true
}

case object EmptyHeightMap extends HeightMap {
  def apply(pos: Position) = ???
  def isDefinedAt(pos: Position) = false
}

case class ImageHeightMap(img: Image, range: Int = 128) extends LocalHeightMap {
  private val scale: Double = (256*256*256) / range
  def get(local: Position) = {
    val v: Double = (img.pixel(local.x, local.z) & 0x00FFFFFF)
    (v / scale).toInt
  }
  def sizeX = img.width
  def sizeZ = img.height
}

case class GlobalHeightMap(placement: Position, map: LocalHeightMap) extends HeightMap {
  def apply(position: Position) = {
    val x = position.x - placement.x
    val z = position.z - placement.z
    map.get(Position(x, 0, z))
  }
  def isDefinedAt(position: Position) =
    (position.x >= placement.x &&
      position.z >= placement.z &&
      position.x < placement.x + map.sizeX &&
      position.z < placement.z + map.sizeZ)
}

class DiamondSquareHeightMap(roughness: Float, baseSize: Int, placement: Position, global: PartialFunction[Position, Int]) extends HeightMap {
  import DiamondSquareHeightMap._
  private val size = findNearestPowerOfTwo(baseSize) + 1
  private val offset = size / 4 + 1
  private val localPlacement = placement.copy(x = placement.x - offset,
                                              z = placement.z - offset)
  private val max = size - 1
  private val array = Array.fill[Float](size * size)(Float.NaN)
  private val random = new scala.util.Random()

  private val local = new PartialFunction[Position, Float] {
    def apply(local: Position) = {
      array(local.x + size * local.z)
    }
    def isDefinedAt(local: Position) =
      array.isDefinedAt(local.x + size * local.z)
  }

  val g = new PartialFunction[Position, Float] {
    def apply(local: Position) = {
      val noise = (1.0f - (random.nextFloat() * 2.0f))
      global(Position(localPlacement.x + local.x, 0, localPlacement.z + local.z)).toFloat + noise
    }
    def isDefinedAt(local: Position) =
      global.isDefinedAt(Position(localPlacement.x + local.x, 0, localPlacement.z + local.z))
  }

  val result = GlobalHeightMap(placement, new LocalHeightMap {

    def get(local: Position): Int =
      math.round(array((local.x + offset) + size * (local.z + offset)))

    def sizeX = baseSize
    def sizeZ = baseSize
  })

  private val map = g orElse local

  {
    val dist = offset / 2
    setPoint(0, 0, diamond(0, 0, dist))
    setPoint(0, max, diamond(0, max, dist))
    setPoint(max, 0, diamond(max, 0, dist))
    setPoint(max, max, diamond(max, max, dist))
    divide(max)
  }

  private def getPoint(x: Int, z: Int): Option[Float] = {
    val pos = Position(x, 0, z)
    if(map.isDefinedAt(pos)) {
      val point = map(pos)
      if(point.isNaN) None
      else Some(point)
    }
    else None
  }

  private def setPoint(x: Int, z: Int, value: Float) {
    array(x + size * z) = value
  }

  def square(x: Int, y: Int, size: Int) =
    average(
      getPoint(x - size, y - size), // upper left
      getPoint(x + size, y - size), // upper right
      getPoint(x + size, y + size), // lower right
      getPoint(x - size, y + size) // lower left
    )

  def diamond(x: Int, y: Int, size: Int) =
    average(
      getPoint(x, y - size), // top
      getPoint(x + size, y), // right
      getPoint(x, y + size), // bottom
      getPoint(x - size, y) // left
    )

  def average(values: Option[Float]*): Float = {
    val v = values.flatten
    if(v.size > 0) v.sum / v.size
    else 0.0f
  }

  def divide(size: Int) {
    val half = size / 2
    val scale = roughness * half

    if (half < 1) return

    for (y <- half until max by size; x <- half until max by size) {
      val v = square(x, y, half) + random.nextFloat() * scale * 2 - scale
      setPoint(x, y, v)
    }
    for (y <- 0 to max by half; x <- ((y + half) % size) to max by size) {
      val v = diamond(x, y, half) + random.nextFloat() * scale * 2 - scale
      setPoint(x, y, v)
    }
    divide(size / 2)
  }

  def apply(pos: Position): Int = result(pos)
  def isDefinedAt(pos: Position) = result.isDefinedAt(pos)

}

object DiamondSquareHeightMap {
  def findNearestPowerOfTwo(number: Int): Int = {
    if(number < 0) throw new IllegalArgumentException("Number must be positive")
    var n = number
    var i = 0
    while(n != 0) {
      i += 1
      n = n >> 1
    }
    1 << i
  }
}
