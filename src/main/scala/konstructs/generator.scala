package konstructs

import akka.actor.{ Actor, ActorRef, Props }

class GeneratorActor extends Actor {
  import GeneratorActor._

  val worlds = Seq[WorldEntry](
    WorldEntry(
      Box(Position(-1024, 0, -1024), Position(1024, 1024, 1024)),
      context.actorOf(FlatWorldActor.props(Position(2096, 1024, 2096)))
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
  def props() = Props(classOf[GeneratorActor])
}
