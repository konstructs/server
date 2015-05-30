package konstructs.plugin

import java.lang.reflect.{ Method, Type, Modifier }
import java.io.File
import scala.concurrent.Future
import akka.util.Timeout
import akka.actor.{ Props, ActorSystem, ActorRef, Actor, ActorSelection }
import com.typesafe.config.{ Config => TypesafeConfig }
case class PluginConfigParameterMeta(name: String, configType: Class[_])

case class PluginConfigMeta(method: Method, parameters: Seq[PluginConfigParameterMeta])

object PluginConfigMeta {
  def apply(m: Method): PluginConfigMeta = {
    val annotations = m
      .getParameterAnnotations
      .flatMap(_.filter(_.isInstanceOf[Config]))
      .map(_.asInstanceOf[Config])
    val parameters = m.getParameterTypes.tail.zip(annotations).map {
      case (t, c) =>
        PluginConfigParameterMeta(c.key, t)
    }
    apply(m, parameters)
  }
}

case class PluginMeta(configs: Seq[PluginConfigMeta])

object PluginMeta {

  private def allParametersAreAnnotated(m: Method): Boolean =
    m.getParameterAnnotations.filter(_.exists(_.isInstanceOf[Config])).size == m.getParameterTypes.size - 1

  def apply(className: String): PluginMeta = {
    val clazz = Class.forName(className)
    val configs = clazz
      .getMethods
      .filter(_.getReturnType == classOf[Props])
      .filter { m => Modifier.isStatic(m.getModifiers) }
      .filter(_.getParameterTypes()(0) == classOf[String])
      .filter(_.getAnnotations.exists(_.isInstanceOf[PluginConstructor]))
      .filter(allParametersAreAnnotated)
      .map(PluginConfigMeta.apply)
    apply(configs)
  }

}

case class ConfiguredPlugin(name: String, method: Method, args: Seq[Either[Object, String]]) {

  val dependencyEdges =
    args.collect {
      case Right(dep) => (name, dep)
    }
}

class PluginLoaderActor(config: TypesafeConfig) extends Actor {
  import scala.collection.JavaConverters._
  import PluginLoaderActor._
  import context.dispatcher
  implicit val selectionTimeout = Timeout(1, java.util.concurrent.TimeUnit.SECONDS)

  val StringType = classOf[String]
  val FileType = classOf[File]
  val ActorRefType = classOf[ActorRef]

  def configurePlugin(name: String, config: TypesafeConfig, c: PluginConfigMeta):
      ConfiguredPlugin = {
    val args: Seq[Either[Object, String]] = c.parameters.map { p =>
      p.configType match {
        case StringType => Left(config.getString(p.name))
        case FileType => Left(new File(config.getString(p.name)))
        case ActorRefType => Right(config.getString(p.name))
      }
    }
    ConfiguredPlugin(name, c.method, Left[Object, String](name) +: args)
  }


  def configurePlugin(name: String, config: TypesafeConfig, meta: PluginMeta):
      ConfiguredPlugin = {
    for(c <- meta.configs) {
      try {
        return configurePlugin(name, config, c)
      }
    }
    println(s"Valid configurations: ${meta.configs}")
    throw new Exception("No valid plugin constructor found")
  }

  def invokePlugins(plugins: List[ConfiguredPlugin]) {
    plugins match {
      case head :: tail =>
        val args = Future.sequence(head.args.map {
          case Right(dep) => ActorSelection(self, dep).resolveOne
          case Left(obj) => Future.successful(obj)
        })
        args.onFailure {
          case e => println(s"Failed to start plugin ${head.name} due to $e")
        }
        for(a <- args) {
          val props = head.method.invoke(null, a: _*).asInstanceOf[Props]
          context.actorOf(props, head.name)
          println(s"Started plugin ${head.name}")
          invokePlugins(tail)
        }
      case _ => Nil
    }
  }

  def receive = {
    case Start =>
      val objs = config.root().entrySet.asScala.filter(_.getValue.valueType == com.typesafe.config.ConfigValueType.OBJECT)
      val plugins = for(e <- objs) yield {
        val name = e.getKey
        val plugin = config.getConfig(name)
        val clazz = plugin.getString("class")
        val meta = PluginMeta(clazz)

        val pluginConf = configurePlugin(name, plugin, meta)
        println(s"Validated configuration for $name")
        pluginConf
      }

      val pluginMap = plugins.map { p =>  (p.name, p) }.toMap

      val pluginEdges = plugins.flatMap(_.dependencyEdges)

      val sortedPlugins = tsort(pluginEdges).map(pluginMap).toSeq.reverse

      invokePlugins(sortedPlugins.toList)
  }
}

object PluginLoaderActor {
  case object Start
  import scala.annotation.tailrec

  def tsort[A](edges: Traversable[(A, A)]): Iterable[A] = {
    @tailrec
    def tsort(toPreds: Map[A, Set[A]], done: Iterable[A]): Iterable[A] = {
        val (noPreds, hasPreds) = toPreds.partition { _._2.isEmpty }
        if (noPreds.isEmpty) {
            if (hasPreds.isEmpty) done else sys.error(hasPreds.toString)
        } else {
            val found = noPreds.map { _._1 }
            tsort(hasPreds.mapValues { _ -- found }, done ++ found)
        }
    }

    val toPred = edges.foldLeft(Map[A, Set[A]]()) { (acc, e) =>
        acc + (e._1 -> acc.getOrElse(e._1, Set())) + (e._2 -> (acc.getOrElse(e._2, Set()) + e._1))
    }
    tsort(toPred, Seq())
  }

  def props(config: TypesafeConfig) = Props(classOf[PluginLoaderActor], config)
}
