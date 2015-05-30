package konstructs

import java.io.File
import scala.util.Try
import akka.actor.{ Actor, ActorRef, Props }
import spray.json._

import org.apache.commons.io.FileUtils

class BinaryStorageActor(directory: File) extends Actor {
  import BinaryStorage._
  import Storage._

  val Suffix = ".binary"

  def receive = {
    case StoreBinary(id, ns, data) =>
      write(directory, id, ns, Suffix, data)
    case LoadBinary(id, ns) =>
      sender ! BinaryLoaded(id, load(directory, id, ns, Suffix))
  }
}

object BinaryStorage {
  case class StoreBinary(id: String, ns: String, data: Array[Byte])
  case class LoadBinary(id: String, ns: String)
  case class BinaryLoaded(id: String, data: Option[Array[Byte]])
}

trait BinaryStorage {
  import BinaryStorage.{ StoreBinary, LoadBinary }

  def ns: String
  def binaryStorage: ActorRef

  def loadBinary(id: String)(implicit sender: ActorRef) = binaryStorage ! LoadBinary(id, ns)
  def storeBinary(id: String, data: Array[Byte])(implicit sender: ActorRef) = binaryStorage ! StoreBinary(id, ns, data)
}

object BinaryStorageActor {
  def props(directory: File) = Props(classOf[BinaryStorageActor], directory)
}

class JsonStorageActor(directory: File) extends Actor {
  import JsonStorage.{ StoreJson, LoadJson, JsonLoaded }
  import Storage._
  private val Suffix = ".json"

  def receive = {
    case StoreJson(id, ns, data) =>
      write(directory, id, ns, Suffix, data.compactPrint.getBytes)
    case LoadJson(id, ns) =>
      val data = load(directory, id, ns, Suffix).flatMap { d =>
        Try((new String(d)).parseJson).toOption
      }
      sender ! JsonLoaded(id, data)
  }
}

object JsonStorageActor {
  def props(directory: File) = Props(classOf[JsonStorageActor], directory)
}

object JsonStorage {
  case class StoreJson(id: String, ns: String, data: JsValue)
  case class LoadJson(id: String, ns: String)
  case class JsonLoaded(id: String, data: Option[JsValue])
}

trait JsonStorage {
  import JsonStorage.{ StoreJson, LoadJson}

  def ns: String
  def jsonStorage: ActorRef

  def loadJson(id: String)(implicit sender: ActorRef) = jsonStorage ! LoadJson(id, ns)
  def storeJson(id: String, data: JsValue)(implicit sender: ActorRef) = jsonStorage ! StoreJson(id, ns, data)
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
