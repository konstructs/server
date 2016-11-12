package konstructs.shard

import konstructs.api.{Position, Box}

import konstructs.Db

case class ChunkPosition(p: Int, q: Int, k: Int) {
  def translate(pd: Int, qd: Int, kd: Int) =
    copy(p = p + pd, q = q + qd, k = k + kd)
  def distance(c: ChunkPosition): Double = {
    val dp = p - c.p
    val dq = q - c.q
    val dk = k - c.k
    math.pow(dp * dp + dq * dq + dk * dk, 1d / 2d)
  }

  def position(x: Int, y: Int, z: Int): Position =
    new Position(
      p * Db.ChunkSize + x,
      k * Db.ChunkSize + y,
      q * Db.ChunkSize + z
    )

  def contains(pos: Position): Boolean = {
    val pp = (if (pos.getX < 0) (pos.getX - Db.ChunkSize + 1) else pos.getX) / Db.ChunkSize
    val pq = (if (pos.getZ < 0) (pos.getZ - Db.ChunkSize + 1) else pos.getZ) / Db.ChunkSize
    val pk = (if (pos.getY < 0) (pos.getY - Db.ChunkSize + 1) else pos.getY) / Db.ChunkSize
    return p == pp && q == pq && k == pk
  }
}

object ChunkPosition {
  def apply(pos: Position): ChunkPosition = {
    // For negative values we need to "round down", i.e. -0.01 should be -1 and not 0
    val p = (if (pos.getX < 0) (pos.getX - Db.ChunkSize + 1) else pos.getX) / Db.ChunkSize
    val q = (if (pos.getZ < 0) (pos.getZ - Db.ChunkSize + 1) else pos.getZ) / Db.ChunkSize
    val k = (if (pos.getY < 0) (pos.getY - Db.ChunkSize + 1) else pos.getY) / Db.ChunkSize
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
      val xs = if (xi == startChunk.p) start.getX else xi * Db.ChunkSize
      val xe = if (xi == endChunk.p) end.getX else xi * Db.ChunkSize + Db.ChunkSize
      val ys = if (yi == startChunk.k) start.getY else yi * Db.ChunkSize
      val ye = if (yi == endChunk.k) end.getY else yi * Db.ChunkSize + Db.ChunkSize
      val zs = if (zi == startChunk.q) start.getZ else zi * Db.ChunkSize
      val ze = if (zi == endChunk.q) end.getZ else zi * Db.ChunkSize + Db.ChunkSize
      new Box(new Position(xs, ys, zs), new Position(xe, ye, ze))
    }).filter { e =>
      e.getFrom != e.getUntil // Remove all empty queries
    } toSet
  }

}
