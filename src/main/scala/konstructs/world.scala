package konstructs

import akka.actor.{ Actor, ActorRef, Props }

import scala.collection.mutable

object World {
  val ChunkSize = 32
  val ShardSize = 8
}

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

case class LocalPosition(x: Int, y: Int, z: Int) {
  def global(c: ChunkPosition) =
    Position(
      c.p * World.ChunkSize + x,
      c.k * World.ChunkSize + y,
      c.q * World.ChunkSize + z
    )
  val index = x + y * World.ChunkSize + z * World.ChunkSize * World.ChunkSize
}

case class Position(x: Int, y: Int, z: Int) {
  def chunk = {
    // For negative values we need to "round down", i.e. -0.01 should be -1 and not 0
    val p = (if(x < 0) (x - World.ChunkSize + 1) else x) / World.ChunkSize
    val q = (if(z < 0) (z - World.ChunkSize + 1) else z) / World.ChunkSize
    val k = (if(y < 0) (y - World.ChunkSize + 1) else y) / World.ChunkSize
    ChunkPosition(p, q, k)
  }
  def local = {
    val c = chunk
    LocalPosition(x - c.p * World.ChunkSize, y - c.k * World.ChunkSize, z - c.q * World.ChunkSize)
  }

  def +(p: Position) = Position(x + p.x, y + p.y, z + p.z)
}

object Position {
  def apply(pos: protocol.Position): Position =
    apply(math.round(pos.x), math.round(pos.y), math.round(pos.z))
}

case class ChunkPosition(p: Int, q: Int, k: Int) {
  def shard = {
    // For negative values we need to "round down", i.e. -0.01 should be -1 and not 0
    val m = (if(p < 0) (p - World.ShardSize + 1) else p) / World.ShardSize
    val n = (if(q < 0) (q - World.ShardSize + 1) else q) / World.ShardSize
    val o = (if(k < 0) (k - World.ShardSize + 1) else k) / World.ShardSize
    ShardPosition(m, n, o)
  }
  def translate(pd: Int, qd: Int, kd: Int) =
    copy(p = p + pd, q = q + qd, k = k + kd)
  def index = {
    val lp = math.abs(p % World.ShardSize)
    val lq = math.abs(q % World.ShardSize)
    val lk = math.abs(k % World.ShardSize)
    lp + lq * World.ShardSize + lk * World.ShardSize * World.ShardSize
  }
  def distance(c: ChunkPosition): Double = {
    val dp = p - c.p
    val dq = q - c.q
    val dk = k - c.k
    math.pow(dp*dp + dq*dq + dk*dk, 1d/2d)
  }
}

case class ShardPosition(m: Int, n: Int, o: Int)

class WorldActor extends Actor {
  import WorldActor._
  import World.ChunkSize

  private var nextPid = 0

  val chunkStore = context.actorOf(StorageActor.props(new java.io.File("world/")))
  val chunkGenerator = context.actorOf(GeneratorActor.props())

  val jsonStore = context.actorOf(JsonStorageActor.props(new java.io.File("meta/")))

  def playerActorId(pid: Int) = s"player-$pid"
  def shardActorId(r: ShardPosition) = s"shard-${r.m}-${r.n}-${r.o}"

  def allPlayers(except: Option[Int] = None) = {
    val players = context.children.filter(_.path.name.startsWith("player-"))
    except match {
      case Some(pid) =>
        players.filter(_.path.name != playerActorId(pid))
      case None => players
    }
  }

  def player(nick: String, password: String) {
    val player = context.actorOf(PlayerActor.props(nextPid, nick, password, sender, self, jsonStore, protocol.Position(0,0,0,0,0)), playerActorId(nextPid))
    allPlayers(except = Some(nextPid)).foreach(_ ! PlayerActor.SendInfo(player))
    allPlayers(except = Some(nextPid)).foreach(player ! PlayerActor.SendInfo(_))
    nextPid = nextPid + 1
  }

  def getShardActor(shard: ShardPosition): ActorRef = {
    val rid = shardActorId(shard)
    context.child(rid) match {
      case Some(a) => a
      case None =>
        context.actorOf(ShardActor.props(shard, chunkStore, chunkGenerator), rid)
    }
  }

  def sendBlocks(to: ActorRef, chunk: ChunkPosition, version: Option[Int]) {
    getShardActor(chunk.shard) ! SendBlocks(to, chunk, version)
  }

  def receive = {
    case CreatePlayer(nick, password) =>
      player(nick, password)
    case SendBlocks(to, chunk, version) =>
      sendBlocks(to, chunk, version)
    case b: PutBlock =>
      getShardActor(b.pos.chunk.shard) ! b
    case b: DestroyBlock =>
      getShardActor(b.pos.chunk.shard) ! b
    case b: ShardActor.BlockUpdate =>
      val chunk = b.pos.chunk
      allPlayers() foreach { p =>
        p ! protocol.SendBlock(chunk.p, chunk.q, b.pos.x, b.pos.y, b.pos.z, b.newW)
      }
    case m: PlayerActor.PlayerMovement =>
      allPlayers(except = Some(m.pid)).foreach(_ ! m)
    case l: PlayerActor.PlayerLogout =>
      allPlayers(except = Some(l.pid)).foreach(_ ! l)
 }
}

object WorldActor {
  case class CreatePlayer(nick: String, password: String)
  case class SendBlocks(to: ActorRef, chunk: ChunkPosition, version: Option[Int])
  case class BlockList(chunk: ChunkPosition, blocks: Array[Byte])
  case class PutBlock(from: ActorRef, pos: Position, w: Int)
  case class DestroyBlock(from: ActorRef, pos: Position)
  def props() = Props(classOf[WorldActor])
}
