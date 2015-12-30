package konstructs

import akka.actor.{ Actor, ActorRef, Props }

import konstructs.api.{ Position }

object Db {
  val ChunkSize = 32
  val ShardSize = 8
  val Header = 2
  val Version = 1.toByte
}

case class ShardPosition(m: Int, n: Int, o: Int)

object ShardPosition {
  def apply(c: ChunkPosition): ShardPosition = {
    // For negative values we need to "round down", i.e. -0.01 should be -1 and not 0
    val m = (if(c.p < 0) (c.p - Db.ShardSize + 1) else c.p) / Db.ShardSize
    val n = (if(c.q < 0) (c.q - Db.ShardSize + 1) else c.q) / Db.ShardSize
    val o = (if(c.k < 0) (c.k - Db.ShardSize + 1) else c.k) / Db.ShardSize
    ShardPosition(m, n, o)
  }

  def apply(p: Position): ShardPosition =
    ShardPosition(ChunkPosition(p))

}

class DbActor(universe: ActorRef, generator: ActorRef, binaryStorage: ActorRef)
    extends Actor {
  import DbActor._
  import Db.ChunkSize

  def shardActorId(r: ShardPosition) = s"shard-${r.m}-${r.n}-${r.o}"

  def getShardActor(pos: Position): ActorRef =
    getShardActor(ShardPosition(pos))

  def getShardActor(chunk: ChunkPosition): ActorRef =
    getShardActor(ShardPosition(chunk))

  def getShardActor(shard: ShardPosition): ActorRef = {
    val rid = shardActorId(shard)
    context.child(rid) match {
      case Some(a) => a
      case None =>
        context.actorOf(ShardActor.props(self, shard, binaryStorage, generator), rid)
    }
  }

  def receive = {
    case p: PutBlock =>
      getShardActor(p.pos) forward p
    case r: ReplaceBlock =>
      getShardActor(r.pos) forward r
    case r: RemoveBlock =>
      getShardActor(r.pos) forward r
    case v: ViewBlock =>
      getShardActor(v.pos) forward v
    case s: SendBlocks =>
      getShardActor(s.chunk) forward s
    case b: BlockList =>
      universe ! b
  }
}

object DbActor {
  case class SendBlocks(chunk: ChunkPosition)
  case class BlockList(chunk: ChunkPosition, data: ChunkData)

  case class PutBlock(pos: Position, w: Int, initiator: ActorRef)
  case class UnableToPut(pos: Position, w: Int, initiator: ActorRef)
  case class ReplaceBlock(pos: Position, w: Int, initiator: ActorRef)
  case class RemoveBlock(pos: Position, initiator: ActorRef)
  case class BlockRemoved(pos: Position, w: Int, initiator: ActorRef)
  case class ViewBlock(pos: Position, initiator: ActorRef)
  case class BlockViewed(pos: Position, w: Int, intitator: ActorRef)

  def props(universe: ActorRef, generator: ActorRef, binaryStorage: ActorRef) =
    Props(classOf[DbActor], universe, generator, binaryStorage)
}
