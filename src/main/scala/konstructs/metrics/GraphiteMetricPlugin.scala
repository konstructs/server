package konstructs.metric

import java.net.InetSocketAddress
import scala.concurrent.duration.{Duration, FiniteDuration}

import akka.actor.{Actor, ActorRef, Props, Stash}
import akka.io.{IO, Tcp}
import akka.util.ByteString
import java.util.concurrent.TimeUnit

import konstructs.plugin.{Config, PluginConstructor}
import konstructs.api.messages.SetMetric

class GraphiteMetricPlugin(name: String, remote: InetSocketAddress, namespacePrefix: String, reconnectDelay: Int)
    extends Actor
    with Stash {
  import GraphiteMetricPlugin.AttemptConnect
  import Tcp._
  import context.system
  val duration = FiniteDuration(reconnectDelay, TimeUnit.MILLISECONDS)

  def connect(delay: FiniteDuration) {
    context.system.scheduler.scheduleOnce(delay, self, AttemptConnect)(context.dispatcher)
  }

  connect(Duration.Zero)

  def receive = {
    case AttemptConnect =>
      println(s"$name: Attempting to connect $remote")
      IO(Tcp) ! Connect(remote)
    case CommandFailed(_: Connect) =>
      println(s"$name: Failed to connect $remote")
      connect(duration)
    case c @ Connected(remote, local) =>
      unstashAll()
      println(s"$name: Connected to $remote")
      val connection = sender
      connection ! Register(self)
      context become {
        case s: SetMetric =>
          val unixTime = System.currentTimeMillis() / 1000L
          val msg = s"${namespacePrefix}.${s.getId.toMetricString} ${s.getValue} $unixTime\n"
          connection ! Write(ByteString(msg, "ascii"))
        case CommandFailed(w: Write) =>
          // O/S buffer was full
          println(s"$name: Failed to write")
        case _: ConnectionClosed =>
          println(s"$name: Connection closed")
          connect(duration)
          context.unbecome()
      }
    case _ =>
      stash()
  }

}

object GraphiteMetricPlugin {
  case object AttemptConnect

  @PluginConstructor
  def props(
      name: String,
      universe: ActorRef,
      @Config(key = "host") host: String,
      @Config(key = "port") port: Int,
      @Config(key = "namespace-prefix") namespacePrefix: String,
      @Config(key = "reconnect-delay") reconnectDelay: Int
  ) = {
    Props(classOf[GraphiteMetricPlugin],
          name,
          new InetSocketAddress(host, port),
          namespacePrefix: String,
          reconnectDelay)
  }
}
