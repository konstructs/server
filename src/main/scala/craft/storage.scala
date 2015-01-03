package craft

import java.io.File
import akka.actor.{ Actor, Props }

import org.apache.commons.io.FileUtils

class StorageActor(directory: File) extends Actor {
  import StorageActor._

  private def chunkFile(c: ChunkPosition) = new File(directory, s"${c.p}/${c.q}/${c.k}.chunk")

  def receive = {
    case Load(chunk) =>
      val f = chunkFile(chunk)
      val blocks = if(f.exists) {
        Some(FileUtils.readFileToByteArray(f))
      } else {
        None
      }
      sender ! Loaded(chunk, blocks)
    case Store(chunk, blocks) =>
      val f = chunkFile(chunk)
      FileUtils.writeByteArrayToFile(f, blocks)
  }

}

object StorageActor {
  case class Load(chunk: ChunkPosition)
  case class Loaded(chunk: ChunkPosition, blocks: Option[Array[Byte]])
  case class Store(chunk: ChunkPosition, blocks: Array[Byte])

  def props(directory: File) =
    Props(classOf[StorageActor], directory)
}
