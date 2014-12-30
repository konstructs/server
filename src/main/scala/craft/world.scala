package craft

import akka.actor.{ Actor, ActorRef, Props }

import scala.collection.mutable

object World {
  val ChunkSize = 32
}

case class LocalPosition(x: Int, y: Int, z: Int) {
  def global(c: Chunk) =
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
    Chunk(p, q, k)
  }
  def local = {
    val c = chunk
    LocalPosition(x - c.p * World.ChunkSize, y - c.k * World.ChunkSize, z - c.q * World.ChunkSize)
  }
}

case class Chunk(p: Int, q: Int, k: Int)
case class Player(pid: Int, actor: ActorRef)

class WorldActor extends Actor {
  import WorldActor._
  import World.ChunkSize

  private var nextPid = 0

  def playerActorId(pid: Int) = s"player-$nextPid"
  def chunkActorId(chunk: Chunk) = s"chunk-${chunk.p}-${chunk.q}-${chunk.k}"

  def player() {
    val player = context.actorOf(PlayerActor.props(sender, self, protocol.Position(0,0,0,0,0)), playerActorId(nextPid))
    val p = Player(nextPid, player)
    nextPid = nextPid + 1
    sender ! p
  }

  def getChunkActor(chunk: Chunk): ActorRef = {
    val cid = chunkActorId(chunk)
    context.child(cid) match {
      case Some(a) => a
      case None    => context.actorOf(ChunkActor.props(chunk), cid)
    }
  }

  def sendBlocks(to: ActorRef, chunk: Chunk, version: Option[Int]) {
    getChunkActor(chunk) ! ChunkActor.SendBlocks(to)
  }

  def receive = {
    case CreatePlayer =>
      player()
    case SendBlocks(to, chunk, version) =>
      sendBlocks(to, chunk, version)
    case b: UpdateBlock =>
      getChunkActor(b.pos.chunk) ! b
    case b: ChunkActor.BlockUpdate =>
      val chunk = b.pos.chunk
      context.children.filter(_.path.name.startsWith("player-")) foreach { p =>
        p ! protocol.SendBlock(chunk.p, chunk.q, b.pos.x, b.pos.y, b.pos.z, b.newW)
      }
 }
}

object WorldActor {
  case object CreatePlayer
  case class SendBlocks(to: ActorRef, chunk: Chunk, version: Option[Int])
  case class BlockList(chunk: Chunk, blocks: Array[Byte])
  case class UpdateBlock(from: ActorRef, pos: Position, w: Int)
  def props() = Props(classOf[WorldActor])
}
