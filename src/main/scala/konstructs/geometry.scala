package konstructs

import konstructs.api.{ Position, Box }

case class Matrix(a: Int, b: Int, c: Int, d: Int, e: Int, f: Int, g: Int, h: Int, i: Int) {
  def *(m: Matrix) =
    Matrix(a*m.a + b*m.d + c*m.g, a*m.b + b*m.e + c*m.h, a*m.c + b*m.f + c*m.i,
           d*m.a + e*m.d + f*m.g, d*m.b + e*m.e + f*m.h, d*m.c + e*m.f + f*m.i,
           g*m.a + h*m.d + i*m.g, g*m.b + h*m.e + i*m.h, g*m.c + h*m.f + i*m.i)

  def *(p: Position) = new Position(a*p.getX + b*p.getY + c*p.getZ,
                                    d*p.getX + e*p.getY + f*p.getZ,
                                    g*p.getX + h*p.getY + i*p.getZ)
  def adg = new Position(a, d, g)
  def beh = new Position(b, e, h)
  def cfi = new Position(c, f, i)
  override def toString = s"Matrix(\n\t$a,\t$b,\t$c\n\t$d,\t$e,\t$f\n\t$g,\t$h,\t$i\n)"
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

  def position(x: Int, y: Int, z: Int): Position =
    new Position(
      p * Db.ChunkSize + x,
      k * Db.ChunkSize + y,
      q * Db.ChunkSize + z
    )

}

object ChunkPosition {
  def apply(pos: Position): ChunkPosition = {
    // For negative values we need to "round down", i.e. -0.01 should be -1 and not 0
    val p = (if(pos.getX < 0) (pos.getX - Db.ChunkSize + 1) else pos.getX) / Db.ChunkSize
    val q = (if(pos.getZ < 0) (pos.getZ - Db.ChunkSize + 1) else pos.getZ) / Db.ChunkSize
    val k = (if(pos.getY < 0) (pos.getY - Db.ChunkSize + 1) else pos.getY) / Db.ChunkSize
    ChunkPosition(p, q, k)
  }
}

object BoxChunking {

  def contains(box: Box, chunk: ChunkPosition): Boolean =
    box.contains(chunk.position(0, 0, 0))

  def translate(box: Box, chunk: ChunkPosition): ChunkPosition = {
    ChunkPosition(chunk.position(0, 0, 0).subtract(box.getFrom))
  }

  def chunked(box: Box): Set[Box] = {
    val start = box.getFrom
    val end = box.getUntil
    val startChunk = ChunkPosition(start)
    val endChunk = ChunkPosition(end)

    val xrange = startChunk.p to endChunk.p
    val yrange = startChunk.k to endChunk.k
    val zrange = startChunk.q to endChunk.q

    (for (xi <- xrange; yi <- yrange; zi <- zrange) yield {
      val xs = if(xi == startChunk.p) start.getX else xi * Db.ChunkSize
      val xe = if(xi == endChunk.p) end.getX else xi * Db.ChunkSize + Db.ChunkSize
      val ys = if(yi == startChunk.k) start.getY else yi * Db.ChunkSize
      val ye = if(yi == endChunk.k) end.getY else yi * Db.ChunkSize + Db.ChunkSize
      val zs = if(zi == startChunk.q) start.getZ else zi * Db.ChunkSize
      val ze = if(zi == endChunk.q) end.getZ else zi * Db.ChunkSize + Db.ChunkSize
      new Box(new Position(xs, ys, zs), new Position(xe, ye, ze))
    }).filter { e =>
      e.getFrom != e.getUntil // Remove all empty queries
    } toSet
  }

}
