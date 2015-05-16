package konstructs

import akka.actor.{ Actor, ActorRef, Props }

object Db {
  val ChunkSize = 32
  val ShardSize = 8
}

case class LocalPosition(x: Int, y: Int, z: Int) {
  def global(c: ChunkPosition) =
    Position(
      c.p * Db.ChunkSize + x,
      c.k * Db.ChunkSize + y,
      c.q * Db.ChunkSize + z
    )
  val index = x + y * Db.ChunkSize + z * Db.ChunkSize * Db.ChunkSize
}

object LocalPosition {
  def apply(p: Position): LocalPosition = {
    val c = ChunkPosition(p)
    LocalPosition(p.x - c.p * Db.ChunkSize, p.y - c.k * Db.ChunkSize, p.z - c.q * Db.ChunkSize)
  }
}

case class ChunkPosition(p: Int, q: Int, k: Int) {
  def shard = {
    // For negative values we need to "round down", i.e. -0.01 should be -1 and not 0
    val m = (if(p < 0) (p - Db.ShardSize + 1) else p) / Db.ShardSize
    val n = (if(q < 0) (q - Db.ShardSize + 1) else q) / Db.ShardSize
    val o = (if(k < 0) (k - Db.ShardSize + 1) else k) / Db.ShardSize
    ShardPosition(m, n, o)
  }
  def translate(pd: Int, qd: Int, kd: Int) =
    copy(p = p + pd, q = q + qd, k = k + kd)
  def index = {
    val lp = math.abs(p % Db.ShardSize)
    val lq = math.abs(q % Db.ShardSize)
    val lk = math.abs(k % Db.ShardSize)
    lp + lq * Db.ShardSize + lk * Db.ShardSize * Db.ShardSize
  }
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

case class ShardPosition(m: Int, n: Int, o: Int)

class DbActor(universe: ActorRef, generator: ActorRef) extends Actor {
  import DbActor._
  import Db.ChunkSize

  val chunkStore = context.actorOf(StorageActor.props(new java.io.File("db/")))

  def shardActorId(r: ShardPosition) = s"shard-${r.m}-${r.n}-${r.o}"

  def getShardActor(shard: ShardPosition): ActorRef = {
    val rid = shardActorId(shard)
    context.child(rid) match {
      case Some(a) => a
      case None =>
        context.actorOf(ShardActor.props(shard, chunkStore, generator), rid)
    }
  }

  def sendBlocks(to: ActorRef, chunk: ChunkPosition, version: Option[Int]) {
    getShardActor(chunk.shard) ! SendBlocks(to, chunk, version)
  }

  def receive = {
    case SendBlocks(to, chunk, version) =>
      sendBlocks(to, chunk, version)
    case b: PutBlock =>
      getShardActor(ChunkPosition(b.pos).shard) ! b
    case b: DestroyBlock =>
      getShardActor(ChunkPosition(b.pos).shard) ! b
    case b: ShardActor.BlockUpdate =>
      universe ! b
  }
}

object DbActor {
  case class SendBlocks(to: ActorRef, chunk: ChunkPosition, version: Option[Int])
  case class BlockList(chunk: ChunkPosition, blocks: Array[Byte])
  case class PutBlock(from: ActorRef, pos: Position, w: Int)
  case class DestroyBlock(from: ActorRef, pos: Position)

  def props(universe: ActorRef, generator: ActorRef) = Props(classOf[DbActor], universe, generator)
}
