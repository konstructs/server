package konstructs

import akka.actor.{ Actor, ActorRef, Props }
import konstructs.plugin.{ PluginConstructor, Config, ListConfig }
import konstructs.api._

class UniverseActor(name: String, jsonStorage: ActorRef, binaryStorage: ActorRef, chatFilters: Seq[ActorRef], blockListeners: Seq[ActorRef], primaryInteractionFilters: Seq[ActorRef],
  secondaryInteractionFilters: Seq[ActorRef]) extends Actor {
  import UniverseActor._

  val generator = context.actorOf(GeneratorActor.props(jsonStorage, binaryStorage))
  val db = context.actorOf(DbActor.props(self, generator, binaryStorage, jsonStorage))

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
    val player = context.actorOf(PlayerActor.props(nextPid, nick, password, sender, db, self, jsonStorage, protocol.Position(0,32f,0,0,0)), playerActorId(nextPid))
    allPlayers(except = Some(nextPid)).foreach(_ ! PlayerActor.SendInfo(player))
    allPlayers(except = Some(nextPid)).foreach(player ! PlayerActor.SendInfo(_))
    nextPid = nextPid + 1
  }

  def receive = {
    case CreatePlayer(nick, password) =>
      player(nick, password)
    case m: PlayerActor.PlayerMovement =>
      allPlayers(except = Some(m.pid)).foreach(_ ! m)
    case l: PlayerActor.PlayerLogout =>
      allPlayers(except = Some(l.pid)).foreach(_ ! l)
    case b: BlockDataUpdate =>
      allPlayers() ++ blockListeners foreach(_ ! b)
    case s: Say =>
      val filters = chatFilters :+ self
      filters.head.forward(SayFilter(filters.tail, s))
    case s: SayFilter =>
      allPlayers().foreach(_.forward(Said(s.message.text)))
    case s: Said =>
      allPlayers().foreach(_.forward(s))
    case i: InteractPrimary =>
      val filters = primaryInteractionFilters :+ self
      filters.head.forward(InteractPrimaryFilter(filters.tail, i))
    case i: InteractPrimaryFilter =>
      db.tell(DestroyBlock(i.message.pos), i.message.sender)
    case i: InteractSecondary =>
      val filters = secondaryInteractionFilters :+ self
      filters.head.forward(InteractSecondaryFilter(filters.tail, i))
    case i: InteractSecondaryFilter =>
      i.message.block.map { block =>
        db.tell(PutBlock(i.message.pos, block), i.message.sender)
      }
    case p: PutBlock =>
      db.forward(p)
    case d: DestroyBlock =>
      db.forward(d)
    case g: GetBlock =>
      db.forward(g)
  }
}

object UniverseActor {
  case class CreatePlayer(nick: String, password: String)

  @PluginConstructor
  def props(name: String, notUsed: ActorRef,
    @Config(key = "binary-storage") binaryStorage: ActorRef,
    @Config(key = "json-storage") jsonStorage: ActorRef,
    @ListConfig(key = "chat-filters", elementType = classOf[ActorRef]) chatFilters: Seq[ActorRef],
    @ListConfig(key = "block-listeners", elementType = classOf[ActorRef]) blockListeners: Seq[ActorRef]
  ): Props
  = Props(classOf[UniverseActor], name, jsonStorage, binaryStorage, chatFilters, blockListeners, Seq(), Seq())

  @PluginConstructor
  def propsWithOnlyBlocks(name: String, notUsed: ActorRef,
    @Config(key = "binary-storage") binaryStorage: ActorRef,
    @Config(key = "json-storage") jsonStorage: ActorRef,
    @ListConfig(key = "block-listeners", elementType = classOf[ActorRef]) blockListeners: Seq[ActorRef]
  ): Props
  = Props(classOf[UniverseActor], name, jsonStorage, binaryStorage, Seq(), blockListeners, Seq(), Seq())

  @PluginConstructor
  def props(name: String, notUsed: ActorRef,
    @Config(key = "binary-storage") binaryStorage: ActorRef,
    @Config(key = "json-storage") jsonStorage: ActorRef,
    @ListConfig(key = "chat-filters", elementType = classOf[ActorRef]) chatFilters: Seq[ActorRef]): Props
  = Props(classOf[UniverseActor], name, jsonStorage, binaryStorage, chatFilters, Seq(), Seq(), Seq())

  @PluginConstructor
  def props(name: String, notUsed: ActorRef,
    @Config(key = "binary-storage") binaryStorage: ActorRef,
    @Config(key = "json-storage") jsonStorage: ActorRef): Props
  = Props(classOf[UniverseActor], name, jsonStorage, binaryStorage, Seq(), Seq(), Seq(), Seq())

  @PluginConstructor
  def props(name: String, notUsed: ActorRef,
    @Config(key = "binary-storage") binaryStorage: ActorRef,
    @Config(key = "json-storage") jsonStorage: ActorRef,
    @ListConfig(key = "chat-filters", elementType = classOf[ActorRef]) chatFilters: Seq[ActorRef],
    @ListConfig(key = "block-listeners", elementType = classOf[ActorRef]) blockListeners: Seq[ActorRef],
    @ListConfig(key = "primary-interaction-listeners", elementType = classOf[ActorRef]) primaryListeners: Seq[ActorRef],
    @ListConfig(key = "secondary-interaction-listeners", elementType = classOf[ActorRef]) secondaryListeners: Seq[ActorRef]
  ): Props
  = Props(classOf[UniverseActor], name, jsonStorage, binaryStorage, chatFilters, blockListeners, primaryListeners, secondaryListeners)
}
