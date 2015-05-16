package konstructs

import akka.actor.{ Actor, ActorRef, Props }

object Db {
  val ChunkSize = 32
  val ShardSize = 8
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
    getShardActor(ShardPosition(chunk)) ! SendBlocks(to, chunk, version)
  }

  def receive = {
    case SendBlocks(to, chunk, version) =>
      sendBlocks(to, chunk, version)
    case b: PutBlock =>
      getShardActor(ShardPosition(b.pos)) ! b
    case b: DestroyBlock =>
      getShardActor(ShardPosition(b.pos)) ! b
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
