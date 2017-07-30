package konstructs.api

import java.util.UUID
import akka.actor.ActorRef
import akka.util.ByteString
import com.google.gson.JsonElement

/* Manage blocks */
case object GetTextures
case class Textures(textures: Array[Byte])

/* Manage konstructing */
case class MatchPattern(pattern: Pattern)
case class PatternMatched(result: Stack)
case class KonstructPattern(pattern: Pattern)
case class PatternKonstructed(pattern: PatternTemplate, result: Stack, number: Int)
case object PatternNotKonstructed

/* Messages for binary storage */
case class StoreBinary(id: String, ns: String, data: ByteString)
case class LoadBinary(id: String, ns: String)
case class BinaryLoaded(id: String, data: Option[ByteString])

/* Messages for JSON storage */
case class StoreGson(id: String, ns: String, data: JsonElement)
case class LoadGson(id: String, ns: String)
case class GsonLoaded(id: String, data: JsonElement)
