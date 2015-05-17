package konstructs

import akka.actor.{ Actor, ActorRef, Props }

class GeneratorActor(jsonStorage: ActorRef) extends Actor {
  import GeneratorActor._

  val worlds = Seq[WorldEntry](
    WorldEntry(
      Box(Position(-1024, 0, -1024), Position(2046, 1024, 2046)),
      context.actorOf(FlatWorldActor.props("Terra", Position(2046 + 1024, 1024, 2046 + 1024), jsonStorage))
    )
  )

  val EmptyChunk = {
    val blocks = new Array[Byte](Db.ChunkSize * Db.ChunkSize * Db.ChunkSize)
    val compressionBuffer = new Array[Byte](Db.ChunkSize * Db.ChunkSize * Db.ChunkSize)
    Chunk(compress.deflate(blocks, compressionBuffer))
  }

  def receive = {
    case Generate(chunk) =>
      worlds.filter(_.box.contains(chunk)).headOption match {
        case Some(entry) =>
          entry.actor forward World.Generate(chunk, entry.box.translate(chunk))
        case None =>
          sender ! Generated(chunk, EmptyChunk)
      }
  }

}

object GeneratorActor {
  case class Generate(position: ChunkPosition)
  case class Generated(position: ChunkPosition, chunk: Chunk)

  case class WorldEntry(box: Box, actor: ActorRef)
  def props(jsonStorage: ActorRef) = Props(classOf[GeneratorActor], jsonStorage)
}
