package konstructs

import java.io.File
import scala.util.Try
import akka.actor.{ Actor, ActorRef, Props }
import spray.json._

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

class JsonStorageActor(directory: File) extends Actor {
  import JsonStorage.{ StoreJson, LoadJson, JsonLoaded }

  private def file(id: String, ns: String) = new File(directory, s"$ns/$id.json")

  def receive = {
    case StoreJson(id, ns, data) =>
      FileUtils.writeByteArrayToFile(file(id, ns), data.compactPrint.getBytes)
    case LoadJson(id, ns) =>
      val f = file(id, ns)
      val data = if(f.exists) {
        val content = FileUtils.readFileToByteArray(f)
        Try((new String(content)).parseJson).toOption
      } else {
        None
      }
      sender ! JsonLoaded(id, ns, data)
  }
}

object JsonStorageActor {
  def props(directory: File) = Props(classOf[JsonStorageActor], directory)
}

object JsonStorage {
  case class StoreJson(id: String, ns: String, data: JsValue)
  case class LoadJson(id: String, ns: String)
  case class JsonLoaded(id: String, ns: String, data: Option[JsValue])
}

trait JsonStorage {
  import JsonStorage.{ StoreJson, LoadJson}

  def ns: String
  def jsonStorage: ActorRef

  def load(id: String)(implicit sender: ActorRef) = jsonStorage ! LoadJson(id, ns)
  def store(id: String, data: JsValue)(implicit sender: ActorRef) = jsonStorage ! StoreJson(id, ns, data)
}
