package konstructs

import scala.collection.JavaConverters._
import scala.collection.mutable
import akka.actor.{ Actor, ActorRef, Props }

import konstructs.api.{ Position, BlockFactory, Box, BlockTypeId }
import konstructs.api.messages.{ BoxQuery, BoxQueryResult, ViewBlock,
                                 ReplaceBlocks, ReplaceBlock }

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
  jsonStorage: ActorRef, blockFactory: BlockFactory)
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
        context.actorOf(ShardActor.props(self, shard, binaryStorage, jsonStorage, generator, blockFactory), rid)
    }
  }

  def receive = {
    case r: ReplaceBlock =>
      getShardActor(r.getPosition) forward r
    case v: ViewBlock =>
      getShardActor(v.getPosition) forward v
    case s: SendBlocks =>
      getShardActor(s.chunk) forward s
    case q: BoxQuery =>
      val chunkBoxes = BoxChunking.chunked(q.getBox)
      val resultActor = context.actorOf(BoxQueryResultActor.props(sender, q, chunkBoxes, blockFactory))
      chunkBoxes.foreach { box =>
        getShardActor(box.getFrom).tell(new BoxQuery(box), resultActor)
      }
    case r: ReplaceBlocks =>
      for((chunk, blocks) <- splitList[BlockTypeId](r.getBlocks)) {
        getShardActor(chunk) forward ShardActor.ReplaceBlocks(chunk, r.getFilter, blocks)
      }
    case b: BlockList =>
      universe ! b
  }
}

object DbActor {
  case class SendBlocks(chunk: ChunkPosition)
  case class BlockList(chunk: ChunkPosition, data: ChunkData)

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
    jsonStorage: ActorRef, blockFactory: BlockFactory) =
    Props(classOf[DbActor], universe, generator, binaryStorage, jsonStorage, blockFactory)
}

class BoxQueryResultActor(initiator: ActorRef, blockFactory: BlockFactory,
  query: BoxQuery, boxes: Set[Box])
    extends Actor {
  var receivedBoxes: Set[BoxQueryResult] = Set()

  def receive = {
    case r: BoxQueryResult =>
      receivedBoxes += r
      if(receivedBoxes.map(_.getBox) == boxes) {
        val data = new Array[BlockTypeId](query.getBox.getNumberOfBlocks)
        for(subData <- receivedBoxes) {
          for((position, typeId)  <- subData.getAsMap.asScala) {
            data(query.getBox.arrayIndex(position)) = typeId
          }
        }
        initiator ! new BoxQueryResult(query.getBox, data)
        context.stop(self)
      }
  }

}

object BoxQueryResultActor {
  def props(initiator: ActorRef, query: BoxQuery, boxes: Set[Box],
    blockFactory: BlockFactory) =
    Props(classOf[BoxQueryResultActor], initiator, blockFactory, query, boxes)
}
