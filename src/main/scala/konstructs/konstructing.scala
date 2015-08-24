package konstructs

import java.util.UUID
import scala.collection.JavaConverters._

import com.typesafe.config.{ Config => TypesafeConfig }
import akka.actor.{ Actor, Props, ActorRef, Stash }
import konstructs.api._
import konstructs.plugin.{ PluginConstructor, Config, ListConfig }

class KonstructingActor(konstructs: Set[Konstruct], konstructedFilters: Seq[ActorRef])
    extends Actor {

  def bestMatch(pattern: Pattern): Option[Konstruct] = {
    konstructs.filter { k =>
      pattern.contains(k.pattern)
    }.toSeq.sortWith(_.pattern.complexity > _.pattern.complexity).headOption
  }

  def receive = {
    case MatchPattern(pattern: Pattern) =>
      bestMatch(pattern).map { k =>
        sender ! PatternMatched(k.result)
      }
    case KonstructPattern(pattern: Pattern) =>
      bestMatch(pattern) match {
        case Some(k) =>
          val filters = konstructedFilters :+ self
          filters.head.forward(
            PatternKonstructedFilter(filters.tail, PatternKonstructed(k.pattern, k.result), sender)
          )
        case None =>
          sender ! PatternKonstructed(pattern, Stack.Empty)
        }
    case PatternKonstructedFilter(_, message, sender) =>
      sender ! message
  }
}

object KonstructingActor {

  import konstructs.plugin.Plugin.nullAsEmpty

  def parseStack(config: TypesafeConfig): Stack = {
    val blockId = config.getString("id")
    if(blockId == 0) {
      Stack.Empty
    } else {
      val amount = if(config.hasPath("amount")) {
        config.getInt("amount")
      } else {
        1
      }
      Stack(((0 until amount).map { i => Block(None, BlockType(BlockTypeId.fromString(blockId))) }).toList.asJava )
    }
  }

  def parsePattern(config: TypesafeConfig): Pattern = {
    if(config.hasPath("stack")) {
      Pattern(List(parseStack(config.getConfig("stack"))).asJava, 1, 1)
    } else {
      val rows = config.getInt("rows")
      val columns = config.getInt("columns")
      Pattern(config.getConfigList("stacks").asScala.map(parseStack).asJava, rows, columns)
    }
  }

  def parseKonstructs(config: TypesafeConfig): Set[Konstruct] = {
    val konstructs = config.root.entrySet.asScala.map { e =>
      config.getConfig(e.getKey)
    }
    (for(konstruct <- konstructs) yield {
      val pattern = parsePattern(konstruct.getConfig("match"))
      val result = parseStack(konstruct.getConfig("result"))
      Konstruct(pattern, result)
    }) toSet
  }

  @PluginConstructor
  def props(
    name: String, notUsed: ActorRef,
    @Config(key = "konstructs") konstructs: TypesafeConfig,
    @ListConfig(key = "konstructed-filters", elementType = classOf[ActorRef], optional = true) konstructedFilters: Seq[ActorRef]
  ): Props =
    Props(classOf[KonstructingActor], parseKonstructs(konstructs),
      nullAsEmpty(konstructedFilters))
}


class KonstructingViewActor(player: ActorRef, universe: ActorRef, inventoryId: UUID,
                                inventoryView: InventoryView, konstructingView: InventoryView,
                                resultView: InventoryView)
    extends Actor with Stash {

  universe ! GetInventory(inventoryId)

  var konstructing = Inventory.createEmpty(konstructingView.rows * konstructingView.columns)
  var result = Inventory.createEmpty(resultView.rows * resultView.columns)

  private def view(inventory: Inventory) =
    View.Empty
      .add(inventoryView, inventory)
      .add(konstructingView, konstructing)
      .add(resultView, result)

  private def updateKonstructing(k: Inventory) {
    konstructing = k
    result = Inventory.createEmpty(1)
    konstructing.pattern(konstructingView).map { p =>
      universe ! MatchPattern(p)
    }
  }

  def receive = {
    case GetInventoryResponse(_, Some(inventory)) =>
      context.become(ready(inventory))
      player ! ConnectView(self, view(inventory))
    case _ =>
      context.stop(self)
  }


  def awaitInventory: Receive = {
    case GetInventoryResponse(_, Some(inventory)) =>
      context.become(ready(inventory))
      player ! UpdateView(view(inventory))
      unstashAll()
    case CloseInventory =>
      context.stop(self)
    case _ =>
      stash()
  }

  def awaitKonstruction(inventory: Inventory): Receive = {
    case PatternKonstructed(pattern, stack) =>
      context.become(ready(inventory))
      updateKonstructing(konstructing.removePattern(pattern))
      player ! ReceiveStack(stack)
      player ! UpdateView(view(inventory))
      unstashAll()
    case CloseInventory =>
      context.stop(self)
    case _ =>
      stash()
  }

  def ready(inventory: Inventory): Receive = {
    case r: ReceiveStack =>
      player ! r
    case PatternMatched(stack) =>
      result = result.withSlot(0, stack)
      player ! UpdateView(view(inventory))
    case PutViewStack(stack, to) =>
      if(inventoryView.contains(to)) {
        context.become(awaitInventory)
        universe.forward(PutStack(inventoryId, inventoryView.translate(to), stack))
        universe ! GetInventory(inventoryId)
      } else if(konstructingView.contains(to)) {
        val index = konstructingView.translate(to)
        val oldStack = konstructing.stacks.get(index)
        oldStack.acceptStack(stack) match {
          case Some((newStack, left)) =>
            if(left != Stack.Empty) {
              sender ! ReceiveStack(left)
            }
            updateKonstructing(konstructing.withSlot(index, newStack))
          case None =>
            updateKonstructing(konstructing.withSlot(index, stack))
            sender ! ReceiveStack(oldStack)
        }
        player ! UpdateView(view(inventory))
      } else {
        sender ! ReceiveStack(stack)
      }
    case RemoveViewStack(from) =>
      if(inventoryView.contains(from)) {
        context.become(awaitInventory)
        universe.forward(RemoveStack(inventoryId, inventoryView.translate(from)))
        universe ! GetInventory(inventoryId)
      } else if(konstructingView.contains(from)) {
        val stack = konstructing.stacks.get(konstructingView.translate(from))
        updateKonstructing(konstructing.withoutSlot(konstructingView.translate(from)))
        sender ! ReceiveStack(stack)
        player ! UpdateView(view(inventory))
      } else if(resultView.contains(from)) {
        konstructing.pattern(konstructingView) match {
          case Some(pattern) =>
            if(!result.isEmpty) {
              context.become(awaitKonstruction(inventory))
              universe ! KonstructPattern(pattern)
            } else {
              sender ! ReceiveStack(Stack.Empty)
            }
          case None =>
            sender ! ReceiveStack(Stack.Empty)
        }
      }
    case CloseInventory =>
      context.stop(self)
  }

}

object KonstructingViewActor {
  def props(player: ActorRef, universe: ActorRef, inventoryId: UUID,
    inventoryView: InventoryView, konstructingView: InventoryView,
    resultView: InventoryView): Props =
    Props(classOf[KonstructingViewActor], player, universe, inventoryId,
      inventoryView, konstructingView, resultView)
}
