package craft

import akka.actor.{ Actor, ActorRef, Props }

import scala.collection.mutable

object World {
  val ChunkSize = 32
  val RegionSize = 8
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
}

case class ChunkPosition(p: Int, q: Int, k: Int) {
  def region = {
    // For negative values we need to "round down", i.e. -0.01 should be -1 and not 0
    val m = (if(p < 0) (p - World.RegionSize + 1) else p) / World.RegionSize
    val n = (if(q < 0) (q - World.RegionSize + 1) else q) / World.RegionSize
    val o = (if(k < 0) (k - World.RegionSize + 1) else k) / World.RegionSize
    RegionPosition(m, n, o)
  }
  def index = {
    val lp = math.abs(p % World.RegionSize)
    val lq = math.abs(q % World.RegionSize)
    val lk = math.abs(k % World.RegionSize)
    lp + lq * World.RegionSize + lk * World.RegionSize * World.RegionSize
  }
}

case class RegionPosition(m: Int, n: Int, o: Int)
case class Player(pid: Int, actor: ActorRef, nick: String)

class WorldActor extends Actor {
  import WorldActor._
  import World.ChunkSize

  private var nextPid = 0

  val chunkStore = context.actorOf(StorageActor.props(new java.io.File("world/")))
  val chunkGenerator = context.actorOf(GeneratorActor.props())

  def playerActorId(pid: Int) = s"player-$pid"
  def regionActorId(r: RegionPosition) = s"region-${r.m}-${r.n}-${r.o}"

  def allPlayers(except: Option[Int] = None) = {
    val players = context.children.filter(_.path.name.startsWith("player-"))
    except match {
      case Some(pid) =>
        players.filter(_.path.name != playerActorId(pid))
      case None => players
    }
  }

  def player(nick: String) {
    val player = context.actorOf(PlayerActor.props(nextPid, nick, sender, self, protocol.Position(0,0,0,0,0)), playerActorId(nextPid))
    val p = Player(nextPid, player, nick)
    allPlayers(except = Some(nextPid)).foreach(_ ! PlayerActor.SendInfo(player))
    allPlayers(except = Some(nextPid)).foreach(player ! PlayerActor.SendInfo(_))
    nextPid = nextPid + 1
    sender ! p
  }

  def getRegionActor(region: RegionPosition): ActorRef = {
    val rid = regionActorId(region)
    context.child(rid) match {
      case Some(a) => a
      case None =>
        context.actorOf(RegionActor.props(region, chunkStore, chunkGenerator), rid)
    }
  }

  def sendBlocks(to: ActorRef, chunk: ChunkPosition, version: Option[Int]) {
    getRegionActor(chunk.region) ! SendBlocks(to, chunk, version)
  }

  def receive = {
    case CreatePlayer(nick: String) =>
      player(nick)
    case SendBlocks(to, chunk, version) =>
      sendBlocks(to, chunk, version)
    case b: PutBlock =>
      getRegionActor(b.pos.chunk.region) ! b
    case b: DestroyBlock =>
      getRegionActor(b.pos.chunk.region) ! b
    case b: RegionActor.BlockUpdate =>
      val chunk = b.pos.chunk
      allPlayers() foreach { p =>
        p ! protocol.SendBlock(chunk.p, chunk.q, b.pos.x, b.pos.y, b.pos.z, b.newW)
      }
    case m: PlayerActor.PlayerMovement =>
      allPlayers(except = Some(m.pid)).foreach(_ ! m)

 }
}

object WorldActor {
  case class CreatePlayer(nick: String)
  case class SendBlocks(to: ActorRef, chunk: ChunkPosition, version: Option[Int])
  case class BlockList(chunk: ChunkPosition, blocks: Array[Byte])
  case class PutBlock(from: ActorRef, pos: Position, w: Int)
  case class DestroyBlock(from: ActorRef, pos: Position)
  def props() = Props(classOf[WorldActor])
}
