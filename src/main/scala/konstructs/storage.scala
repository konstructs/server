package konstructs

import java.io.File
import scala.util.Try
import akka.actor.{ Actor, ActorRef, Props }
import org.apache.commons.io.FileUtils
import konstructs.plugin.{ PluginConstructor, Config }
import konstructs.api.GsonDefault
import com.google.gson.{ Gson, JsonParser, JsonElement }

class BinaryStorageActor(name: String, directory: File) extends Actor {
  import konstructs.api.{ StoreBinary, LoadBinary, BinaryLoaded }
  import Storage._

  val Suffix = "binary"

  def receive = {
    case StoreBinary(id, ns, data) =>
      write(directory, id, ns, Suffix, data)
    case LoadBinary(id, ns) =>
      sender ! BinaryLoaded(id, load(directory, id, ns, Suffix))
  }
}

trait BinaryStorage {
  import konstructs.api.{ StoreBinary, LoadBinary }

  def ns: String
  def binaryStorage: ActorRef

  def loadBinary(id: String)(implicit sender: ActorRef) = binaryStorage ! LoadBinary(id, ns)
  def storeBinary(id: String, data: Array[Byte])(implicit sender: ActorRef) = binaryStorage ! StoreBinary(id, ns, data)
}

object BinaryStorageActor {
  @PluginConstructor
  def props(name: String, universe: ActorRef,
    @Config(key = "directory") directory: File) = Props(classOf[BinaryStorageActor], name, directory)
}

class JsonStorageActor(name: String, directory: File) extends Actor {
  import konstructs.api.{ StoreGson, LoadGson, GsonLoaded }
  import Storage._

  private val Suffix = "json"
  private val gson = new Gson()
  private val parser = new JsonParser()

  def receive = {
    case StoreGson(id, ns, data) =>
      write(directory, id, ns, Suffix, gson.toJson(data).getBytes())
    case LoadGson(id, ns) =>
      val data = load(directory, id, ns, Suffix).flatMap { d =>
        Try(parser.parse(new String(d))).toOption
      }
      sender ! GsonLoaded(id, data.orNull)
  }
}

object JsonStorageActor {
  @PluginConstructor
  def props(name: String, universe: ActorRef,
    @Config(key = "directory") directory: File) = Props(classOf[JsonStorageActor], name, directory)
}

trait JsonStorage {
  import konstructs.api.{ StoreGson, LoadGson }

  val gson = GsonDefault.getDefaultGson

  def ns: String
  def jsonStorage: ActorRef

  def loadGson(id: String)(implicit sender: ActorRef) = jsonStorage ! LoadGson(id, ns)
  def storeGson(id: String, data: JsonElement)(implicit sender: ActorRef) = jsonStorage ! StoreGson(id, ns, data)

}

object Storage {
  def file(directory: File, id: String, ns: String, suffix: String) = new File(directory, s"$ns/$id.$suffix")
  def write(directory: File, id: String, ns: String, suffix: String, data: Array[Byte]) =
    FileUtils.writeByteArrayToFile(file(directory, id, ns, suffix), data)
  def load(directory: File, id: String, ns: String, suffix: String): Option[Array[Byte]] = {
    val f = file(directory, id, ns, suffix)
    if(f.exists) {
      Some(FileUtils.readFileToByteArray(f))
    } else {
      None
    }
  }
}
