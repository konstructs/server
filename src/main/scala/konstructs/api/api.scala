package konstructs.api

import java.util.UUID
import scala.collection.JavaConverters._
import scala.collection.mutable
import akka.actor.ActorRef
import com.google.gson.JsonElement
import konstructs.protocol

/* Data structures */

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

/* Messages */

/* Messages for chat */
case class Say(player: String, text: String)
case class SayFilter(chain: Seq[ActorRef], message: Say) extends Filter[Say] {
  def next(chain: Seq[ActorRef]) = copy(chain = chain)
  def next(chain: Seq[ActorRef], message: Say) = copy(chain = chain, message = message)
}
case class Said(text: String)

/* Messages for world interaction */
case class InteractPrimary(sender: ActorRef, player: String, pos: Position, block: Block)
case class InteractPrimaryFilter(chain: Seq[ActorRef], message: InteractPrimary) extends Filter[InteractPrimary] {
  def next(chain: Seq[ActorRef]) = copy(chain = chain)
  def next(chain: Seq[ActorRef], message: InteractPrimary) = copy(chain = chain, message = message)
}

case class InteractSecondary(sender: ActorRef, player: String, pos: Position, block: Block)
case class InteractSecondaryFilter(chain: Seq[ActorRef], message: InteractSecondary) extends Filter[InteractSecondary] {
  def next(chain: Seq[ActorRef]) = copy(chain = chain)
  def next(chain: Seq[ActorRef], message: InteractSecondary) = copy(chain = chain, message = message)
}

case class InteractTertiary(sender: ActorRef, player: String, pos: Position, block: Block)
case class InteractTertiaryFilter(chain: Seq[ActorRef], message: InteractTertiary) extends Filter[InteractTertiary] {
  def next(chain: Seq[ActorRef]) = copy(chain = chain)
  def next(chain: Seq[ActorRef], message: InteractTertiary) = copy(chain = chain, message = message)
}

/* Manage blocks */
case object GetBlockFactory
case object GetTextures
case class Textures(textures: Array[Byte])

/* Manage inventories */
case class CreateInventory(blockId: UUID, size: Int)
case class GetInventory(blockId: UUID)
case class GetInventoryResponse(blockId: UUID, inventory: Option[Inventory])
case class PutStack(blockId: UUID, slot: Int, stack: Stack)
case class RemoveStack(blockId: UUID, slot: Int)
case class GetStack(blockId: UUID, slot: Int)
case class GetStackResponse(blockId: UUID, slot: Int, stack: Stack)
case class DeleteInventory(blockId: UUID)
case class ReceiveStack(stack: Stack)

/* Manage konstructing */
case class MatchPattern(pattern: Pattern)
case class PatternMatched(result: Stack)
case class KonstructPattern(pattern: Pattern)
case class PatternKonstructed(pattern: Pattern, result: Stack)
case class PatternKonstructedFilter(chain: Seq[ActorRef], message: PatternKonstructed, sender: ActorRef) extends Filter[PatternKonstructed] {
  def next(chain: Seq[ActorRef]) = copy(chain = chain, sender = sender)
  def next(chain: Seq[ActorRef], message: PatternKonstructed) = copy(chain = chain, message = message, sender = sender)
}

/* Manage player */
case class ConnectView(manager: ActorRef, view: View)
case class UpdateView(view: View)
case class PutViewStack(stack: Stack, to: Int)
case class RemoveViewStack(from: Int)
case object CloseInventory

/* Messages for binary storage */
case class StoreBinary(id: String, ns: String, data: Array[Byte])
case class LoadBinary(id: String, ns: String)
case class BinaryLoaded(id: String, data: Option[Array[Byte]])

/* Messages for JSON storage */
case class StoreGson(id: String, ns: String, data: JsonElement)
case class LoadGson(id: String, ns: String)
case class GsonLoaded(id: String, data: JsonElement)
