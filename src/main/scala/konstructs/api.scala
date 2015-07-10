package konstructs

import akka.actor.ActorRef
import com.google.gson.JsonElement
import spray.json.JsValue

package api {
  /* Messages for chat */
  case class Say(text: String)
  case class SayFilter(chain: Seq[ActorRef], message: Say) extends Filter[Say] {
    def next(chain: Seq[ActorRef]) = copy(chain = chain)
    def next(chain: Seq[ActorRef], message: Say) = copy(chain = chain, message = message)
  }
  case class Said(text: String)

  /* Messages for world interaction */
  case class PutBlock(pos: Position, w: Int)
  case class DestroyBlock(pos: Position)
  case class GetBlock(pos: Position)
  case class BlockPosition(pos: Position, w: Int)
  case class BlockUpdate(pos: Position, oldW: Int, newW: Int)

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
}
