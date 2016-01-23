package konstructs

import konstructs.api.Position

case class Matrix(a: Int, b: Int, c: Int, d: Int, e: Int, f: Int, g: Int, h: Int, i: Int) {
  def *(m: Matrix) =
    Matrix(a*m.a + b*m.d + c*m.g, a*m.b + b*m.e + c*m.h, a*m.c + b*m.f + c*m.i,
           d*m.a + e*m.d + f*m.g, d*m.b + e*m.e + f*m.h, d*m.c + e*m.f + f*m.i,
           g*m.a + h*m.d + i*m.g, g*m.b + h*m.e + i*m.h, g*m.c + h*m.f + i*m.i)

  def *(p: Position) = Position(a*p.x + b*p.y + c*p.z,
                                d*p.x + e*p.y + f*p.z,
                                g*p.x + h*p.y + i*p.z)
  def adg = Position(a, d, g)
  def beh = Position(b, e, h)
  def cfi = Position(c, f, i)
  override def toString = s"Matrix(\n\t$a,\t$b,\t$c\n\t$d,\t$e,\t$f\n\t$g,\t$h,\t$i\n)"
}

case class Box(start: Position, end: Position) {
  if(start.x > end.x || start.y > end.y || start.z > end.z)
    throw new IllegalArgumentException("Start must be smaller than end in all dimensions")

  private val xSize = end.x - start.x
  private val ySize = end.y - start.y
  private val zSize = end.z - start.z

  def contains(p: Position): Boolean =
    p.x >= start.x && p.x < end.x && p.y >= start.y && p.y < end.y && p.z >= start.z && p.z < end.z

  def contains(chunk: ChunkPosition): Boolean =
    contains(Position(chunk, 0, 0, 0))

  def translate(chunk: ChunkPosition): ChunkPosition = {
    ChunkPosition(Position(chunk, 0, 0, 0) - start)
  }

  def chunked: Set[Box] = {
    val startChunk = ChunkPosition(start)
    val endChunk = ChunkPosition(end)

    val xrange = startChunk.p to endChunk.p
    val yrange = startChunk.k to endChunk.k
    val zrange = startChunk.q to endChunk.q

    (for (xi <- xrange; yi <- yrange; zi <- zrange) yield {
      val xs = if(xi == startChunk.p) start.x else xi * Db.ChunkSize
      val xe = if(xi == endChunk.p) end.x else xi * Db.ChunkSize + Db.ChunkSize
      val ys = if(yi == startChunk.k) start.y else yi * Db.ChunkSize
      val ye = if(yi == endChunk.k) end.y else yi * Db.ChunkSize + Db.ChunkSize
      val zs = if(zi == startChunk.q) start.z else zi * Db.ChunkSize
      val ze = if(zi == endChunk.q) end.z else zi * Db.ChunkSize + Db.ChunkSize
      Box(Position(xs, ys, zs), Position(xe, ye, ze))
    }).filter { e =>
      e.start != e.end // Remove all empty queries
    } toSet
  }

  def index(x: Int, y: Int, z: Int): Int =
    x * ySize * zSize + y * zSize + z

  def index(pos: Position): Int =
    index(pos.x - start.x, pos.y - start.y, pos.z - start.z)

  def blocks = xSize * ySize * zSize
}

case class ChunkPosition(p: Int, q: Int, k: Int) {
  def translate(pd: Int, qd: Int, kd: Int) =
    copy(p = p + pd, q = q + qd, k = k + kd)
  def distance(c: ChunkPosition): Double = {
    val dp = p - c.p
    val dq = q - c.q
    val dk = k - c.k
    math.pow(dp*dp + dq*dq + dk*dk, 1d/2d)
  }
}

object ChunkPosition {
  def apply(pos: Position): ChunkPosition = {
    // For negative values we need to "round down", i.e. -0.01 should be -1 and not 0
    val p = (if(pos.x < 0) (pos.x - Db.ChunkSize + 1) else pos.x) / Db.ChunkSize
    val q = (if(pos.z < 0) (pos.z - Db.ChunkSize + 1) else pos.z) / Db.ChunkSize
    val k = (if(pos.y < 0) (pos.y - Db.ChunkSize + 1) else pos.y) / Db.ChunkSize
    ChunkPosition(p, q, k)
  }
}
