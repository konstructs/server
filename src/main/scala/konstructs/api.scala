package konstructs.api
import java.util.UUID
import akka.actor.ActorRef
import com.google.gson.JsonElement
import spray.json._
import konstructs.ChunkPosition
import konstructs.protocol
import konstructs.Db

/* Data structures */
case class Block(id: Option[UUID], w: Int)
trait Filter[T] {
  def chain: Seq[ActorRef]
  def next(chain: Seq[ActorRef]): Filter[T]
  def next(chain: Seq[ActorRef], message: T): Filter[T]

  /** Let next plugin in chain handle the unchanged message
    */
  def continue(implicit sender: ActorRef) {
    chain.head ! next(chain = chain.tail)
  }

  /** Let next plugin in chain handle an updated message
    */
  def continueWith(newMessage: T)(implicit sender: ActorRef) {
    chain.head ! next(chain = chain.tail, message = newMessage)
  }

  /** Skip all other plugins in chain, but let the server process the
    * unchanged message
    */
  def skip(implicit sender: ActorRef) {
    chain.last ! next(chain = Seq())
  }

  /** Skip all other plugins in chain, but let the server process an
    * updated message
    */
  def skipWith(newMessage: T)(implicit sender: ActorRef) {
    chain.last ! next(chain = Seq(), message = newMessage)
  }

  /** Drop the message
    * (this is a no-op since the message is dropped if continue or
    * skip is not called)
    */
  def drop() {}
}

case class Position(x: Int, y: Int, z: Int) {
  def +(p: Position) = Position(x + p.x, y + p.y, z + p.z)
  def -(p: Position) = Position(x - p.x, y - p.y, z - p.z)
}

object Position {
  def apply(pos: protocol.Position): Position =
    apply(math.round(pos.x), math.round(pos.y), math.round(pos.z))
  def apply(chunk: ChunkPosition, x: Int, y: Int, z: Int): Position =
    Position(
      chunk.p * Db.ChunkSize + x,
      chunk.k * Db.ChunkSize + y,
      chunk.q * Db.ChunkSize + z
    )
}


/* Messages for chat */
case class Say(text: String)
case class SayFilter(chain: Seq[ActorRef], message: Say) extends Filter[Say] {
  def next(chain: Seq[ActorRef]) = copy(chain = chain)
  def next(chain: Seq[ActorRef], message: Say) = copy(chain = chain, message = message)
}
case class Said(text: String)

/* Messages for world interaction */
case class PutBlock(pos: Position, block: Block)
case class DestroyBlock(pos: Position)
case class ReceiveBlock(pos: Position, block: Block)
case class GetBlock(pos: Position)
case class GetBlockResponse(pos: Position, block: Block)
case class BlockDataUpdate(pos: Position, oldW: Int, newW: Int)

/* Manage block IDs */
case class GetOrCreateBlockId(pos: Position)
case class GetOrCreateBlockIdResponse(pos: Position, id: UUID)

/* Messages for binary storage */
case class StoreBinary(id: String, ns: String, data: Array[Byte])
case class LoadBinary(id: String, ns: String)
case class BinaryLoaded(id: String, data: Option[Array[Byte]])

/* Messages for JSON storage */
case class StoreJson(id: String, ns: String, data: JsValue)
case class LoadJson(id: String, ns: String)
case class JsonLoaded(id: String, data: Option[JsValue])
case class StoreGson(id: String, ns: String, data: JsonElement)
case class LoadGson(id: String, ns: String)
case class GsonLoaded(id: String, data: Option[JsonElement])

object KonstructsJsonProtocol extends DefaultJsonProtocol {
  implicit object UuidJsonFormat extends JsonFormat[UUID] {
    def write(x: UUID) = JsString(x.toString)
    def read(value: JsValue) = value match {
      case JsString(x) => UUID.fromString(x)
      case x => deserializationError("Expected UUID as JsString, but got " + x)
    }
  }
  implicit val blockFormat = jsonFormat2(Block)
}
