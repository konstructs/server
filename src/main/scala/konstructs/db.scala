package konstructs

import akka.actor.{ Actor, ActorRef, Props }

import konstructs.api._

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

class DbActor(universe: ActorRef, generator: ActorRef, binaryStorage: ActorRef,
  jsonStorage: ActorRef)
    extends Actor {
  import DbActor._
  import Db.ChunkSize

  val blockMeta =
    context.actorOf(BlockMetaActor.props("block-meta", jsonStorage), "block-meta")

  def shardActorId(r: ShardPosition) = s"shard-${r.m}-${r.n}-${r.o}"

  def getShardActor(shard: ShardPosition): ActorRef = {
    val rid = shardActorId(shard)
    context.child(rid) match {
      case Some(a) => a
      case None =>
        context.actorOf(ShardActor.props(self, blockMeta, shard, binaryStorage, generator), rid)
    }
  }

  def sendBlocks(chunk: ChunkPosition, version: Option[Int]) {
    getShardActor(ShardPosition(chunk)) forward SendBlocks(chunk, version)
  }

  def receive = {
    case SendBlocks(chunk, version) =>
      sendBlocks(chunk, version)
    case b: PutBlock =>
      getShardActor(ShardPosition(b.pos)) forward b
    case b: DestroyBlock =>
      getShardActor(ShardPosition(b.pos)) forward b
    case g: GetBlock =>
      getShardActor(ShardPosition(g.pos)) forward g
    case b: BlockDataUpdate =>
      universe ! b
  }
}

object DbActor {
  case class SendBlocks(chunk: ChunkPosition, version: Option[Int])
  case class BlockList(chunk: ChunkPosition, data: ChunkData)

  def props(universe: ActorRef, generator: ActorRef, binaryStorage: ActorRef,
    jsonStorage: ActorRef) =
    Props(classOf[DbActor], universe, generator, binaryStorage, jsonStorage)
}
