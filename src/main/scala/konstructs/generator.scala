package konstructs

import akka.actor.{ Actor, ActorRef, Props }

import konstructs.api._

import konstructs.shard.{ ChunkPosition, BoxChunking,
                          BlockData }

class GeneratorActor(jsonStorage: ActorRef, binaryStorage: ActorRef, factory: BlockFactory) extends Actor {
  import GeneratorActor._

  val worlds = Seq[WorldEntry](
    WorldEntry(
      new Box(new Position(-1536, 0, -1536), new Position(1536, 512, 1536)),
      context.actorOf(FlatWorldActor.props("Terra", new Position(3072, 1024, 3072), factory, jsonStorage, binaryStorage))
    )
  )

  val Vacuum = factory.getW(BlockTypeId.VACUUM)

  val Pristine = Health.PRISTINE.getHealth()

  val EmptyChunk = {
    val data = new Array[Byte](Db.ChunkSize * Db.ChunkSize * Db.ChunkSize * BlockData.Size)
    for(i <- 0 until Db.ChunkSize * Db.ChunkSize * Db.ChunkSize) {
      BlockData.write(data, i, Vacuum, Pristine, Direction.UP_ENCODING, Rotation.IDENTITY_ENCODING,
        LightLevel.FULL_ENCODING, 0, 0, 0, LightLevel.DARK_ENCODING)
    }
    data
  }

  def receive = {
    case Generate(chunk) =>
      worlds.filter { b => BoxChunking.contains(b.box, chunk) }.headOption match {
        case Some(entry) =>
          entry.actor forward World.Generate(chunk, BoxChunking.translate(entry.box, chunk))
        case None =>
          sender ! Generated(chunk, EmptyChunk)
      }
  }

}

object GeneratorActor {
  case class Generate(position: ChunkPosition)
  case class Generated(position: ChunkPosition, blocks: Array[Byte])

  case class WorldEntry(box: Box, actor: ActorRef)
  def props(jsonStorage: ActorRef, binaryStorage: ActorRef, factory: BlockFactory) =
    Props(classOf[GeneratorActor], jsonStorage, binaryStorage, factory)
}
