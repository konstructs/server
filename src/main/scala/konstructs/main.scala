package konstructs

import akka.actor.ActorSystem
import com.typesafe.config.ConfigFactory
import konstructs.plugin.PluginLoaderActor

object Main extends App {
  val conf =
    ConfigFactory.parseFile(new java.io.File("konstructs.conf")).withFallback(ConfigFactory.load())
  implicit val system = ActorSystem("main", conf)
  val loader = system.actorOf(PluginLoaderActor.props(conf.getConfig("konstructs")), "plugin-loader")
  loader ! PluginLoaderActor.Start
}
