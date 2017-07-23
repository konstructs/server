package konstructs.api

import java.util.UUID
import akka.actor.ActorRef
import akka.util.ByteString
import com.google.gson.JsonElement

/* Manage blocks */
case object GetTextures
case class Textures(textures: Array[Byte])

case class CreateInventory(blockId: UUID, size: Int)
case class GetInventory(blockId: UUID)
case class GetInventoryResponse(blockId: UUID, inventory: Option[Inventory])
case class PutStack(blockId: UUID, slot: Int, stack: Stack)
case class RemoveStack(blockId: UUID, slot: Int, amount: StackAmount)
case class GetStack(blockId: UUID, slot: Int)
case class GetStackResponse(blockId: UUID, slot: Int, stack: Stack)
case class DeleteInventory(blockId: UUID)
case class ReceiveStack(stack: Stack)

/* Manage konstructing */
case class MatchPattern(pattern: Pattern)
case class PatternMatched(result: Stack)
case class KonstructPattern(pattern: Pattern)
case class PatternKonstructed(pattern: PatternTemplate, result: Stack, number: Int)
case object PatternNotKonstructed

/* Manage player */
case class ConnectView(manager: ActorRef, view: View)
case class UpdateView(view: View)
case class PutViewStack(stack: Stack, to: Int)
case class RemoveViewStack(from: Int, amount: StackAmount)
case object CloseInventory

/* Messages for binary storage */
case class StoreBinary(id: String, ns: String, data: ByteString)
case class LoadBinary(id: String, ns: String)
case class BinaryLoaded(id: String, data: Option[ByteString])

/* Messages for JSON storage */
case class StoreGson(id: String, ns: String, data: JsonElement)
case class LoadGson(id: String, ns: String)
case class GsonLoaded(id: String, data: JsonElement)
