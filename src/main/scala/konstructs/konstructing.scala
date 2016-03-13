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
      pattern.contains(k.getPattern)
    }.toSeq.sortWith(_.getPattern.getComplexity > _.getPattern.getComplexity).headOption
  }

  def receive = {
    case MatchPattern(pattern: Pattern) =>
      bestMatch(pattern).map { k =>
        sender ! PatternMatched(k.getResult)
      }
    case KonstructPattern(pattern: Pattern) =>
      bestMatch(pattern) match {
        case Some(k) =>
          val filters = konstructedFilters :+ self
          filters.head.forward(
            PatternKonstructedFilter(
              filters.tail,
              PatternKonstructed(k.getPattern, k.getResult),
              sender)
          )
        case None =>
          sender ! PatternKonstructed(pattern, null)
        }
    case PatternKonstructedFilter(_, message, sender) =>
      sender ! message
  }
}

object KonstructingActor {

  import konstructs.plugin.Plugin.nullAsEmpty

  def parseStack(config: TypesafeConfig): Stack = {
    val blockId = config.getString("id")
    val amount = if(config.hasPath("amount")) {
      config.getInt("amount")
    } else {
      1
    }
    new Stack(
      BlockTypeId.fromString(blockId),
      ((0 until amount).map { i => Block.create(blockId) }).toArray )
  }

  def parsePattern(config: TypesafeConfig): Pattern = {
    if(config.hasPath("stack")) {
      new Pattern(Array(parseStack(config.getConfig("stack"))), 1, 1)
    } else {
      val rows = config.getInt("rows")
      val columns = config.getInt("columns")
      new Pattern(config.getConfigList("stacks").asScala.map(parseStack).toArray, rows, columns)
    }
  }

  def parseKonstructs(config: TypesafeConfig): Set[Konstruct] = {
    val konstructs = config.root.entrySet.asScala.map { e =>
      config.getConfig(e.getKey)
    }
    (for(konstruct <- konstructs) yield {
      val pattern = parsePattern(konstruct.getConfig("match"))
      val result = parseStack(konstruct.getConfig("result"))
      new Konstruct(pattern, result)
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

  var konstructing = Inventory.createEmpty(konstructingView.getRows * konstructingView.getColumns)
  var result = Inventory.createEmpty(resultView.getRows * resultView.getColumns)

  private def view(inventory: Inventory) =
    View.EMPTY
      .add(inventoryView, inventory)
      .add(konstructingView, konstructing)
      .add(resultView, result)

  private def updateKonstructing(k: Inventory) {
    konstructing = k
    result = Inventory.createEmpty(1)
    val p = konstructing.getPattern(konstructingView)
    if(p != null)
      universe ! MatchPattern(p)
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
        val oldStack = konstructing.getStack(index)
        if(oldStack != null) {
          if(oldStack.acceptsPartOf(stack)) {
            val r = oldStack.acceptPartOf(stack)
            sender ! ReceiveStack(r.getGiving)
            updateKonstructing(konstructing.withSlot(index, r.getAccepting()))
          } else {
            updateKonstructing(konstructing.withSlot(index, stack))
            sender ! ReceiveStack(oldStack)
          }
        } else {
          updateKonstructing(konstructing.withSlot(index, stack))
          sender ! ReceiveStack(null)
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
        val stack = konstructing.getStack(konstructingView.translate(from))
        updateKonstructing(konstructing.withoutSlot(konstructingView.translate(from)))
        sender ! ReceiveStack(stack)
        player ! UpdateView(view(inventory))
      } else if(resultView.contains(from)) {
        val pattern = konstructing.getPattern(konstructingView)
        if(pattern != null) {
          if(!result.isEmpty) {
            context.become(awaitKonstruction(inventory))
            universe ! KonstructPattern(pattern)
          } else {
            sender ! ReceiveStack(null)
          }
        } else {
          sender ! ReceiveStack(null)
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
