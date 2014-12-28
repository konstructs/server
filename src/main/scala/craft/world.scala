package craft

import akka.actor.{ Actor, ActorRef, Props }

import scala.collection.mutable

case class Player(pid: Int, actor: ActorRef)

class WorldActor extends Actor {
  import WorldActor._

  private var nextPid = 0

  def playerActorId(pid: Int) = s"player-$nextPid"
  def chunkActorId(chunk: Chunk) = s"chunk_${chunk.p}_${chunk.q}_${chunk.k}"

  def player() {
    val player = context.actorOf(PlayerActor.props(sender, self, protocol.Position(0,0,0,0,0)), playerActorId(nextPid))
    val p = Player(nextPid, player)
    nextPid = nextPid + 1
    sender ! p
  }

  def sendBlocks(to: ActorRef, chunk: Chunk, version: Option[Int]) {
    val cid = chunkActorId(chunk)
    val actor = context.child(cid) match {
      case Some(a) => a
      case None    => context.actorOf(ChunkActor.props(chunk), cid)
    }
    actor ! ChunkActor.SendBlocks(to)
  }

  def receive = {
    case CreatePlayer =>
      player()
    case SendBlocks(to, chunk, version) =>
      sendBlocks(to, chunk, version)
  }
}

object WorldActor {
  val ChunkSize = 32
  val YChunks = 65536 / 32

  case object CreatePlayer
  case class Chunk(p: Int, q: Int, k: Int)
  case class SendBlocks(to: ActorRef, chunk: Chunk, version: Option[Int])
  case class BlockList(blocks: Seq[protocol.SendBlock])
  def props() = Props(classOf[WorldActor])
}
