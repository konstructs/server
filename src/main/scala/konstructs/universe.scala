package konstructs

import akka.actor.{ Actor, ActorRef, Props }

class UniverseActor extends Actor {
  import UniverseActor._

  val db = context.actorOf(DbActor.props(self))
  val jsonStore = context.actorOf(JsonStorageActor.props(new java.io.File("meta/")))

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
    val player = context.actorOf(PlayerActor.props(nextPid, nick, password, sender, db, jsonStore, protocol.Position(0,0,0,0,0)), playerActorId(nextPid))
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
    case b: ShardActor.BlockUpdate =>
      val chunk = b.pos.chunk
      allPlayers() foreach { p =>
        p ! protocol.SendBlock(chunk.p, chunk.q, b.pos.x, b.pos.y, b.pos.z, b.newW)
      }
  }
}

object UniverseActor {
  case class CreatePlayer(nick: String, password: String)

  def props() = Props(classOf[UniverseActor])
}
