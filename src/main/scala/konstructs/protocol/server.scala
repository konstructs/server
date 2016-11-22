package konstructs.protocol

import akka.actor.{Actor, ActorRef, Props, ActorLogging, Stash}
import akka.io._
import java.net.InetSocketAddress
import konstructs.plugin.PluginConstructor
import konstructs.api.{BlockFactory, GetTextures, Textures}
import konstructs.api.messages.GetBlockFactory

class Server(name: String, universe: ActorRef) extends Actor with ActorLogging with Stash {
  import Tcp._
  import context.system

  IO(Tcp) ! Bind(self, new InetSocketAddress("0.0.0.0", 4080))

  universe ! GetTextures

  def receive = {
    case Textures(textures) =>
      context.become(getFactory(textures))
      universe ! GetBlockFactory.MESSAGE
      unstashAll()
    case _ =>
      stash()
  }

  def getFactory(textures: Array[Byte]): Receive = {
    case b: BlockFactory =>
      context.become(waiting(b, textures))
      unstashAll()
    case _ =>
      stash()
  }

  def waiting(factory: BlockFactory, textures: Array[Byte]): Receive = {
    case _: Bound =>
      context.become(bound(sender, factory, textures))
    case CommandFailed(_: Bind) => context stop self
  }

  def bound(listener: ActorRef, factory: BlockFactory, textures: Array[Byte]): Receive = {
    case Connected(remote, _) =>
      val connection = sender
      val handler = context.actorOf(ClientActor.props(universe, factory, textures))
      println(s"$remote connected!")
      connection ! Tcp.Register(handler)
  }
}

object Server {
  @PluginConstructor
  def props(name: String, universe: ActorRef) =
    Props(classOf[Server], name, universe)
}
