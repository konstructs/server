package konstructs.api

import java.util.UUID
import scala.collection.JavaConverters._
import akka.actor.ActorRef
import com.google.gson.JsonElement
import spray.json._
import konstructs.ChunkPosition
import konstructs.protocol
import konstructs.Db

/* Data structures */
case class Block(id: Option[UUID], w: Int) {
  def withId = copy(id = Some(UUID.randomUUID))
}

object Block {
  def createWithId(w: Int): Block = {
    apply(Some(UUID.randomUUID), w)
  }
}

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

case class Stack(blocks: java.util.List[Block]) {
  def w = blocks.asScala.headOption.map(_.w).getOrElse(0)
  def size = blocks.size
  def isEmpty = blocks.isEmpty
  def isFull = blocks.size == Stack.MaxSize
  def head = blocks.asScala.head
  def headOption = blocks.asScala.headOption
  def tail = Stack(blocks.asScala.tail.asJava)
  def accepts(block: Block): Boolean = isEmpty || (block.w == w && !isFull)
  def accept(block: Block): Option[Stack] = if(accepts(block)) {
    val newBlocks = blocks.asScala :+ block
    Some(Stack(newBlocks.asJava))
  } else {
    None
  }
}

object Stack {
  def fromSeq(blocks: Seq[Block]): Stack =
    apply(blocks.toList.asJava)
  def fromBlock(block: Block): Stack =
    apply(List(block).asJava)
  val Empty = Stack(List.empty[Block].asJava)
  val MaxSize = 64
}

case class Inventory(stacks: Array[Stack]) {
  def isEmpty = !stacks.exists(!_.isEmpty)

  def withoutSlot(slot: Int) = {
    val newStacks = stacks.clone()
    newStacks(slot) = Stack.Empty
    Inventory(newStacks)
  }

  def withSlot(slot: Int, stack: Stack) = {
    val newStacks = stacks.clone()
    newStacks(slot) = stack
    Inventory(newStacks)
  }

  def stackOption(slot: Int): Option[Stack] = {
    val stack = stacks(slot)
    if(stack.isEmpty) {
      None
    } else {
      Some(stack)
    }
  }

  def blockHeadOption(slot: Int): Option[Block] = {
    stackOption(slot) flatMap { stack =>
      stack.headOption
    }
  }

  def stackTail(slot: Int): Inventory = {
    val stack = stacks(slot)
    withSlot(slot, stack.tail)
  }

  def accept(block: Block): Option[Inventory] = {
    stacks.zipWithIndex.find(_._1.accepts(block)) flatMap {
      case (stack, slot) =>
        stack.accept(block).map(withSlot(slot, _))
    }
  }

  def moveSlot(from: Int, to: Int): Inventory = {
    val fromStack = stacks(from)
    val toStack = stacks(to)
    withSlot(from, toStack).withSlot(to, fromStack)
  }
}

object Inventory {
  def createEmpty(dimension: Int): Inventory =
    apply(Array.fill(dimension)(Stack.Empty))
}

case class InventoryView(rowOffset: Int, columnOffset: Int, rows: Int, columns: Int) {
  import View._
  def translate(pos: Int): Int = {
    val r = pos / Columns
    val c = pos % Columns
    val row = r - rowOffset
    val column = c - columnOffset
    row * columns + column
  }

  def contains(pos: Int): Boolean = {
    val row = pos / Columns
    val col = pos % Columns
    row >= rowOffset && row < rowOffset + rows && col >= columnOffset && col < columnOffset + columns
  }
}

case class View(items: Map[Int, Option[Stack]]) {
  import View._

  def add(inventoryView: InventoryView, inventory: Inventory): View = {
    View(items ++ (for(
      row <- 0 until inventoryView.rows;
      column <- 0 until inventoryView.columns
    ) yield {
      val r = row + inventoryView.rowOffset
      val c = column + inventoryView.columnOffset
      ((r * Columns + c) -> Some(inventory.stacks(row * inventoryView.columns + column)))
    }))
  }

}

object View {
  val Columns = 16
  val Empty = View((for(i <- 0 until 256) yield {
    i -> None
  }).toMap)
}

/* Messages for chat */
case class Say(player: String, text: String)
case class SayFilter(chain: Seq[ActorRef], message: Say) extends Filter[Say] {
  def next(chain: Seq[ActorRef]) = copy(chain = chain)
  def next(chain: Seq[ActorRef], message: Say) = copy(chain = chain, message = message)
}
case class Said(text: String)

/* Messages for world interaction */
case class InteractPrimary(sender: ActorRef, player: String, pos: Position, block: Option[Block])
case class InteractPrimaryFilter(chain: Seq[ActorRef], message: InteractPrimary) extends Filter[InteractPrimary] {
  def next(chain: Seq[ActorRef]) = copy(chain = chain)
  def next(chain: Seq[ActorRef], message: InteractPrimary) = copy(chain = chain, message = message)
}

case class InteractSecondary(sender: ActorRef, player: String, pos: Position, block: Option[Block])
case class InteractSecondaryFilter(chain: Seq[ActorRef], message: InteractSecondary) extends Filter[InteractSecondary] {
  def next(chain: Seq[ActorRef]) = copy(chain = chain)
  def next(chain: Seq[ActorRef], message: InteractSecondary) = copy(chain = chain, message = message)
}
case class PutBlock(pos: Position, block: Block)
case class DestroyBlock(pos: Position)
case class ReceiveStack(stack: Stack)
case class GetBlock(pos: Position)
case class GetBlockResponse(pos: Position, block: Block)
case class BlockDataUpdate(pos: Position, oldW: Int, newW: Int)

/* Manage block IDs */
case class GetOrCreateBlockId(pos: Position)
case class GetOrCreateBlockIdResponse(pos: Position, id: UUID)

/* Manage inventories */
case class CreateInventory(blockId: UUID, size: Int)
case class GetInventory(blockId: UUID)
case class GetInventoryResponse(blockId: UUID, inventory: Option[Inventory])
case class PutStack(blockId: UUID, slot: Int, stack: Stack)
case class RemoveStack(blockId: UUID, slot: Int)
case class GetStack(blockId: UUID, slot: Int)
case class GetStackResponse(blockId: UUID, slot: Int, stack: Option[Stack])
case class DeleteInventory(blockId: UUID)
case class MoveStack(fromBlockId: UUID, from: Int, toBlockId: UUID, to: Int)

/* Manage player */
case class ConnectView(manager: ActorRef, view: View)
case class UpdateView(view: View)
case class MoveViewStack(from: Int, to: Int)
case class PutViewStack(stack: Stack, to: Int)
case class RemoveViewStack(from: Int)
case object CloseInventory

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

  implicit def javaListFormat[T :JsonFormat] = new RootJsonFormat[java.util.List[T]] {
    def write(list: java.util.List[T]) = JsArray(list.asScala.map(_.toJson).toVector)
    def read(value: JsValue): java.util.List[T] = value match {
      case JsArray(elements) => elements.map(_.convertTo[T])(collection.breakOut).asJava
      case x => deserializationError("Expected List as JsArray, but got " + x)
    }
  }
  implicit val blockFormat = jsonFormat2(Block.apply)
  implicit val stackFormat = jsonFormat1(Stack.apply)
  implicit val inventoryFormat = jsonFormat1(Inventory.apply)
}
