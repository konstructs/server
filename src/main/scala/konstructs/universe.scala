package konstructs

import akka.actor.{ Actor, ActorRef, Props, Stash }
import konstructs.plugin.{ PluginConstructor, Config, ListConfig, PluginRef }
import konstructs.api._
import konstructs.api.messages._
import collection.JavaConversions._

class UniverseActor(
                     name: String,
                     jsonStorage: ActorRef,
                     binaryStorage: ActorRef,
                     inventoryManager: ActorRef,
                     konstructing: ActorRef,
                     blockManager: ActorRef,
                     chatFilters: Seq[ActorRef],
                     blockUpdateEvents: Seq[ActorRef],
                     primaryInteractionFilters: Seq[ActorRef],
                     secondaryInteractionFilters: Seq[ActorRef],
                     tertiaryInteractionFilters: Seq[ActorRef],
                     listeners: Map[EventTypeId, Seq[ActorRef]]
                   ) extends Actor with Stash {

  import UniverseActor._

  var generator: ActorRef = null
  var db: ActorRef = null

  private var nextPid = 0

  def playerActorId(pid: Int) = s"player-$pid"

  def allPlayers(except: Option[Int] = None) = {
    val players = context.children.filter(_.path.name.startsWith("player-"))
    except match {
      case Some(pid) =>
        players.filter(_.path.name != playerActorId(pid))
      case None => players
    }
  }

  def player(nick: String, password: String) {
    val player = context.actorOf(
      PlayerActor.props(nextPid, nick, password, sender, db, self, jsonStorage, protocol.Position(0,512f,0,0,0)),
      playerActorId(nextPid)
    )
    allPlayers(except = Some(nextPid)).foreach(_ ! PlayerActor.SendInfo(player))
    allPlayers(except = Some(nextPid)).foreach(player ! PlayerActor.SendInfo(_))
    nextPid = nextPid + 1
  }

  blockManager ! GetBlockFactory.MESSAGE

  def receive = {
    case factory: BlockFactory =>
      generator = context.actorOf(GeneratorActor.props(jsonStorage, binaryStorage, factory))
      db = context.actorOf(DbActor.props(self, generator, binaryStorage, jsonStorage, blockUpdateEvents, factory,
        tertiaryInteractionFilters))
      context.become(ready)
      unstashAll()
    case _ =>
      stash()
  }

  def ready: Receive = {
    case s: SendEvent =>
      listeners getOrElse(s.getId, Seq()) foreach (_ forward s.getMessage)
    case CreatePlayer(nick, password) =>
      player(nick, password)
    case m: PlayerActor.PlayerMovement =>
      allPlayers(except = Some(m.pid)).foreach(_ ! m)
    case l: PlayerActor.PlayerLogout =>
      allPlayers(except = Some(l.pid)).foreach(_ ! l)
    case c: DbActor.ChunkUpdate =>
      allPlayers().foreach(_ ! c)
    case s: Say =>
      val filters = chatFilters :+ self
      filters.head.forward(new SayFilter(filters.tail.toArray, s))
    case s: SayFilter =>
      allPlayers().foreach(_.forward(new Said(s.getMessage.getText)))
    case s: Said =>
      allPlayers().foreach(_.forward(s))
    case i: InteractPrimary =>
      val filters = primaryInteractionFilters :+ self
      filters.head.forward(new InteractPrimaryFilter(filters.tail.toArray, i))
    case i: InteractPrimaryFilter =>
      val message = i.getMessage
      if(message.getPosition != null) {
        db.tell(DbActor.InteractPrimaryUpdate(message.getPosition, message.getBlock), message.getSender)
      } else {
        message.getSender ! new InteractResult(message.getPosition, message.getBlock, null)
      }
    case i: InteractSecondary =>
      val filters = secondaryInteractionFilters :+ self
      filters.head.forward(new InteractSecondaryFilter(filters.tail.toArray, i))
    case i: InteractSecondaryFilter =>
      val message = i.getMessage
      if(message.getPosition != null && message.getBlock != null) {
        db.tell(DbActor.InteractSecondaryUpdate(message.getPosition, message.getOrientation, message.getBlock), message.getSender)
      } else {
        message.getSender ! new InteractResult(message.getPosition, message.getBlock, null)
      }
    case i: InteractTertiary =>
      if(i.getPosition != null) {
        db ! DbActor.InteractTertiaryUpdate(tertiaryInteractionFilters, i)
      } else {
        val filters = tertiaryInteractionFilters :+ self
        filters.head ! new InteractTertiaryFilter(filters.tail.toArray, i)
      }
    case i: InteractTertiaryFilter if !i.getMessage.isWorldPhase =>
      val message = i.getMessage
      message.getSender ! new InteractResult(message.getPosition, message.getBlock, message.getBlockAtPosition)
    case p: ReplaceBlock =>
      db.forward(p)
    case v: ViewBlock =>
      db.forward(v)
    case r: ReplaceBlocks =>
      db forward r
    case c: CreateInventory =>
      inventoryManager.forward(c)
    case g: GetInventory =>
      inventoryManager.forward(g)
    case p: PutStack =>
      inventoryManager.forward(p)
    case r: RemoveStack =>
      inventoryManager.forward(r)
    case g: GetStack =>
      inventoryManager.forward(g)
    case d: DeleteInventory =>
      inventoryManager.forward(d)
    case m: MatchPattern =>
      konstructing.forward(m)
    case k: KonstructPattern =>
      konstructing.forward(k)
    case q: BoxQuery =>
      db forward q
    case q: BoxShapeQuery =>
      db forward q
    case d: DamageBlockWithBlock =>
      db forward d
    case GetBlockFactory.MESSAGE =>
      blockManager.forward(GetBlockFactory.MESSAGE)
    case GetTextures =>
      blockManager.forward(GetTextures)
  }
}

object UniverseActor {
  case class CreatePlayer(nick: String, password: String)

  import konstructs.plugin.Plugin.nullAsEmpty

  def parseEventId(plugin: PluginRef)(id: String): (EventTypeId, PluginRef) =
    (EventTypeId.fromString(id), plugin)

  def parseEventIds(plugin: PluginRef): Seq[(EventTypeId, PluginRef)] =
    plugin.getConfig.root.keySet map parseEventId(plugin) toSeq

  def tag(entry: (EventTypeId, PluginRef)): EventTypeId = entry._1

  def getActorRef(plugins: Seq[(EventTypeId, konstructs.plugin.PluginRef)]): Seq[ActorRef] =
    plugins map {
      case (_, plugin) => plugin.getPlugin
    }

  def parseListeners(listeners: Seq[PluginRef]): Map[EventTypeId, Seq[ActorRef]] = {
    val tagged: Seq[(EventTypeId, PluginRef)] = listeners map parseEventIds flatten
    val grouped = tagged groupBy tag
    grouped mapValues getActorRef
  }

  @PluginConstructor
  def props(
             name: String,
             notUsed: ActorRef,

             @Config(key = "binary-storage") binaryStorage: ActorRef,
             @Config(key = "json-storage") jsonStorage: ActorRef,
             @Config(key = "inventory-manager") inventoryManager: ActorRef,
             @Config(key = "konstructing") konstructing: ActorRef,
             @Config(key = "block-manager") blockManager: ActorRef,

             @ListConfig(
               key = "chat-filters",
               elementType = classOf[ActorRef],
               optional = true
             ) chatFilters: Seq[ActorRef],


             @ListConfig(
               key = "block-update-events",
               elementType = classOf[ActorRef],
               optional = true
             ) blockUpdateEvents: Seq[ActorRef],

             @ListConfig(
               key = "primary-interaction-listeners",
               elementType = classOf[ActorRef],
               optional = true
             ) primaryListeners: Seq[ActorRef],

             @ListConfig(
               key = "secondary-interaction-listeners",
               elementType = classOf[ActorRef],
               optional = true
             ) secondaryListeners: Seq[ActorRef],

             @ListConfig(
               key = "tertiary-interaction-listeners",
               elementType = classOf[ActorRef],
               optional = true
             ) tertiaryListeners: Seq[ActorRef],

             @ListConfig(
               key = "listeners",
               elementType = classOf[PluginRef],
               optional = true
             ) listeners: Seq[PluginRef]

           ): Props = Props(
                             classOf[UniverseActor],
                             name,
                             jsonStorage,
                             binaryStorage,
                             inventoryManager,
                             konstructing,
                             blockManager,
                             nullAsEmpty(chatFilters),
                             nullAsEmpty(blockUpdateEvents),
                             nullAsEmpty(primaryListeners),
                             nullAsEmpty(secondaryListeners),
                             nullAsEmpty(tertiaryListeners),
                             parseListeners(nullAsEmpty(listeners))
                           )
}
