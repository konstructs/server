package konstructs

import scala.collection.JavaConverters._
import scala.collection.mutable
import akka.actor.{ Actor, ActorRef, Props }

import konstructs.api.{ Position, BlockFactory, BoxQuery, BoxData, BlockTypeId,
                        BoxQueryRawResult, BoxQueryResult, ReplaceBlocks }

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
  blockFactory: BlockFactory)
    extends Actor {
  import DbActor._

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
        context.actorOf(ShardActor.props(self, shard, binaryStorage, generator, blockFactory), rid)
    }
  }

  def receive = {
    case p: DbActor.PutBlock =>
      getShardActor(p.pos) forward p
    case r: DbActor.RemoveBlock =>
      getShardActor(r.pos) forward r
    case v: DbActor.ViewBlock =>
      getShardActor(v.pos) forward v
    case s: SendBlocks =>
      getShardActor(s.chunk) forward s
    case q: BoxQuery =>
      val chunkBoxes = q.box.chunked
      val resultActor = context.actorOf(BoxQueryResultActor.props(sender, q, chunkBoxes, blockFactory))
      chunkBoxes.foreach { box =>
        getShardActor(box.start).tell(BoxQuery(box), resultActor)
      }
    case ReplaceBlocks(filter, blocks) =>
      for((chunk, blocks) <- splitList[BlockTypeId](blocks)) {
        getShardActor(chunk) forward ShardActor.ReplaceBlocks(chunk, filter, blocks)
      }
    case b: BlockList =>
      universe ! b
  }
}

object DbActor {
  case class SendBlocks(chunk: ChunkPosition)
  case class BlockList(chunk: ChunkPosition, data: ChunkData)

  case class PutBlock(pos: Position, w: Int, initiator: ActorRef)
  case class UnableToPut(pos: Position, w: Int, initiator: ActorRef)
  case class RemoveBlock(pos: Position, initiator: ActorRef)
  case class BlockRemoved(pos: Position, w: Int, initiator: ActorRef)
  case class ViewBlock(pos: Position, initiator: ActorRef)
  case class BlockViewed(pos: Position, w: Int, intitator: ActorRef)

  def splitList[T](placed: java.util.Map[Position, T]):
      Map[ChunkPosition, Map[Position, T]] = {
    val shards = mutable.HashMap[ChunkPosition, mutable.Map[Position, T]]()

    for((position, i) <- placed.asScala) {
      val pos = ChunkPosition(position)
      val map: mutable.Map[Position, T] =
        shards.getOrElse(pos, mutable.HashMap[Position, T]())
      map += position -> i
      shards += pos -> map
    }
    (shards.map { case (k, v) =>
      k -> v.toMap
    }).toMap
  }

  def props(universe: ActorRef, generator: ActorRef, binaryStorage: ActorRef,
    blockFactory: BlockFactory) =
    Props(classOf[DbActor], universe, generator, binaryStorage, blockFactory)
}

class BoxQueryResultActor(initiator: ActorRef, blockFactory: BlockFactory,
  query: BoxQuery, boxes: Set[Box])
    extends Actor {
  var receivedBoxes: Set[BoxData[Int]] = Set()

  def receive = {
    case r: BoxQueryRawResult =>
      receivedBoxes += r.result
      if(receivedBoxes.map(_.box) == boxes) {
        val data = new Array[BlockTypeId](query.box.blocks)
        for(subData <- receivedBoxes) {
          for((position, typeId)  <- subData.toPlaced.asScala) {
            data(query.box.index(position)) = blockFactory.wMapping(typeId)
          }
        }
        initiator ! BoxQueryResult(BoxData(query.box, java.util.Arrays.asList(data:_*)))
        context.stop(self)
      }
  }

}

object BoxQueryResultActor {
  def props(initiator: ActorRef, query: BoxQuery, boxes: Set[Box],
    blockFactory: BlockFactory) =
    Props(classOf[BoxQueryResultActor], initiator, blockFactory, query, boxes)
}
