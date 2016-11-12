package konstructs.plugin

import java.lang.reflect.{Method, Modifier}
import java.io.File
import scala.concurrent.Future
import scala.language.existentials
import scala.util.Try

import akka.util.Timeout
import akka.actor.{Props, ActorRef, Actor, ActorSelection, Stash}
import com.typesafe.config.{Config => TypesafeConfig, ConfigException, ConfigValueType}

import konstructs.api.messages.GlobalConfig

object Plugin {
  val StaticParameters = 2
  def nullAsEmpty[T](seq: Seq[T]): Seq[T] =
    if (seq == null) {
      Seq.empty[T]
    } else {
      seq
    }
  def nullAsEmpty[T](list: java.util.List[T]): java.util.List[T] =
    if (list == null) {
      java.util.Collections.emptyList[T]
    } else {
      list
    }
}

case class PluginConfigParameterMeta(name: String,
                                     configType: Class[_],
                                     optional: Boolean,
                                     listType: Option[Class[_]] = None)

case class PluginConfigMeta(method: Method, parameters: Seq[PluginConfigParameterMeta], staticParameters: Int)

object PluginConfigMeta {
  def apply(m: Method, staticParameters: Int): PluginConfigMeta = {
    val annotations = m.getParameterAnnotations.flatMap(_.filter { a =>
      a.isInstanceOf[Config] || a.isInstanceOf[ListConfig]
    })
    val parameters = m.getParameterTypes.drop(staticParameters).zip(annotations).map {
      case (t, c: Config) =>
        PluginConfigParameterMeta(c.key, t, c.optional)
      case (t, c: ListConfig) =>
        PluginConfigParameterMeta(c.key, c.elementType, c.optional, Some(t))
    }
    apply(m, parameters, staticParameters)
  }
}

case class PluginMeta(configs: Seq[PluginConfigMeta])

object PluginMeta {
  private def validationError(m: Method) {
    println(s"Error validating: $m")
  }

  private def validateReturnType(m: Method): Boolean =
    if (m.getReturnType == classOf[Props]) {
      return true
    } else {
      validationError(m)
      println(s"The return type must be: ${classOf[Props]}")
      return false
    }

  private def validateStatic(m: Method): Boolean = {
    if (Modifier.isStatic(m.getModifiers)) {
      return true
    } else {
      validationError(m)
      println("The method must be static")
      return false
    }
  }

  private def validateFirstArgString(m: Method): Boolean = {
    if (m.getParameterTypes()(0) == classOf[String]) {
      return true
    } else {
      validationError(m)
      println(s"The first argument must be: ${classOf[String]}")
      return false
    }
  }

  private def validateSecondArgActorRef(m: Method): Boolean = {
    if (m.getParameterTypes()(1) == classOf[ActorRef]) {
      return true
    } else {
      validationError(m)
      println(s"The second argument must be: ${classOf[ActorRef]}")
      return false
    }
  }

  private def thirdArgIsConfig(m: Method): Boolean = {
    val paramTypes = m.getParameterTypes()
    if (paramTypes.size > 2 && paramTypes(2) == classOf[TypesafeConfig] && !m.getParameterAnnotations()(2).exists {
          a =>
            a.isInstanceOf[Config] || a.isInstanceOf[ListConfig]
        }) {
      return true
    } else {
      return false
    }
  }

  private def allParametersAreAnnotated(m: Method, staticParameters: Int): Boolean =
    if (m.getParameterAnnotations
          .filter(_.exists { a =>
            a.isInstanceOf[Config] || a.isInstanceOf[ListConfig]
          })
          .size == m.getParameterTypes.size - staticParameters) {
      return true
    } else {
      validationError(m)
      println(
        s"Only the first three arguments, name, universe and optionally config, are left without an @Config annotation.")
      println("All other arguments must be annotated.")
      return false
    }

  def apply(className: String): PluginMeta = {
    val clazz = Class.forName(className)

    val filteredMethods = clazz.getMethods
      .filter(_.getAnnotations.exists(_.isInstanceOf[PluginConstructor]))
      .filter(validateReturnType)
      .filter(validateStatic)
      .filter(validateFirstArgString)
      .filter(validateSecondArgActorRef)

    val methods = for (m <- filteredMethods) yield {
      val staticParameters = if (thirdArgIsConfig(m)) {
        Plugin.StaticParameters + 1
      } else {
        Plugin.StaticParameters
      }

      if (allParametersAreAnnotated(m, staticParameters))
        Some(PluginConfigMeta(m, staticParameters))
      else
        None
    }
    apply(methods.flatten)
  }

}

case class Dependency(name: String, config: Option[TypesafeConfig])

case class Dependencies(dependencies: Seq[Dependency], t: Class[_], isPluginRef: Boolean)

object Dependencies {
  def apply(dep: String, config: Option[TypesafeConfig], t: Class[_], isPluginRef: Boolean): Dependencies =
    apply(Seq(Dependency(dep, config)), t, isPluginRef)
}

case class ConfiguredPlugin(name: String, method: Method, args: Seq[Either[Object, Dependencies]]) {

  val dependencyEdges =
    args.collect {
      case Right(deps) =>
        deps.dependencies.map { d =>
          (name, d.name)
        }
    } flatten

}

class PluginLoaderActor(rootConfig: TypesafeConfig) extends Actor {
  import scala.collection.JavaConverters._
  import PluginLoaderActor._
  import context.dispatcher
  import UniverseProxyActor.SetUniverse

  val config = rootConfig.getConfig("konstructs")
  val globalConfig = new GlobalConfig(rootConfig.getDouble("globals.simulation-speed").toFloat)

  implicit val selectionTimeout = Timeout(1, java.util.concurrent.TimeUnit.SECONDS)

  val StringType = classOf[String]
  val IntegerType = classOf[Int]
  val FileType = classOf[File]
  val ActorRefType = classOf[ActorRef]
  val PluginRefType = classOf[PluginRef]
  val SeqType = classOf[Seq[_]]
  val ListType = classOf[java.util.List[_]]
  val ConfigType = classOf[TypesafeConfig]
  val universeProxy = context.actorOf(UniverseProxyActor.props(), "universe-proxy")

  private def actorName(name: String) = name.replace('/', '-')

  private def listType(t: Class[_], seq: Seq[_ <: AnyRef]): Object = t match {
    case SeqType => seq
    case ListType => seq.toList.asJava
    case ActorRefType => seq.head
    case PluginRefType => seq.head
  }

  private def getOptional(optional: Boolean)(f: => Object): Object = {
    if (optional) {
      try {
        f
      } catch {
        case _: ConfigException.Missing => null
      }
    } else {
      f
    }
  }

  private def toConfig(s: String, config: TypesafeConfig): TypesafeConfig =
    config.getConfig(s)

  private def toOptionalConfig(s: String, config: TypesafeConfig): Option[TypesafeConfig] =
    Try {
      config.getConfig(s)
    } toOption

  private def asDependency(key: String, config: TypesafeConfig): Dependency =
    Dependency(keyAsString(key), toOptionalConfig(key, config))

  private def keepString(s: String, config: TypesafeConfig): String = config.getString(s)

  private def keyAsString(s: String): String = s.replace("\"", "")

  private def toFile(s: String, config: TypesafeConfig): File = new File(config.getString(s))

  private def configToSeq[T](get: (String, TypesafeConfig) => T)(config: TypesafeConfig): Seq[T] =
    config.root.entrySet.asScala.filter { e =>
      val v = e.getValue
      v.valueType != ConfigValueType.NULL
    }.map { e =>
      val k = e.getKey
      get(k, config)
    }.toSeq

  def configurePlugin(name: String, config: TypesafeConfig, c: PluginConfigMeta): ConfiguredPlugin = {
    val args: Seq[Either[Object, Dependencies]] = c.parameters.map { p =>
      val opt =
        getOptional(p.optional) _
      p.configType match {
        case StringType =>
          if (p.listType.isDefined) {
            Left(opt(listType(p.listType.get, configToSeq(keepString)(config.getConfig(p.name)))))
          } else {
            Left(opt(config.getString(p.name)))
          }
        case IntegerType =>
          if (p.listType.isDefined) {
            Left(opt(listType(p.listType.get, config.getIntList(p.name).asScala.toSeq)))
          } else {
            Left(opt(new Integer(config.getInt(p.name))))
          }
        case FileType =>
          if (p.listType.isDefined) {
            Left(opt(listType(p.listType.get, configToSeq(toFile)(config.getConfig(p.name)))))
          } else {
            Left(opt(toFile(p.name, config)))
          }
        case ActorRefType =>
          try {
            if (p.listType.isDefined) {
              Right(Dependencies(configToSeq(asDependency)(config.getConfig(p.name)), p.listType.get, false))
            } else {
              Right(Dependencies(config.getString(p.name), toOptionalConfig(p.name, config), ActorRefType, false))
            }
          } catch {
            case e: ConfigException.Missing =>
              if (p.optional) {
                Left(null)
              } else {
                throw e
              }
          }
        case PluginRefType =>
          try {
            if (p.listType.isDefined) {
              Right(Dependencies(configToSeq(asDependency)(config.getConfig(p.name)), p.listType.get, true))
            } else {
              Right(Dependencies(config.getString(p.name), toOptionalConfig(p.name, config), PluginRefType, true))
            }
          } catch {
            case e: ConfigException.Missing =>
              if (p.optional) {
                Left(null)
              } else {
                throw e
              }
          }
        case ConfigType =>
          if (p.listType.isDefined) {
            Left(opt(listType(p.listType.get, configToSeq(toConfig)(config.getConfig(p.name)))))
          } else {
            Left(opt(toConfig(p.name, config)))
          }
      }
    }
    val staticArgs = if (c.staticParameters == 2) {
      Seq(Left[Object, Dependencies](name), Left[Object, Dependencies](universeProxy))
    } else {
      Seq(Left[Object, Dependencies](name),
          Left[Object, Dependencies](universeProxy),
          Left[Object, Dependencies](config))
    }
    ConfiguredPlugin(name, c.method, staticArgs ++ args)
  }

  def configurePlugin(name: String, config: TypesafeConfig, meta: PluginMeta): ConfiguredPlugin = {
    for (c <- meta.configs.sortBy(_.parameters.size).reverse) {
      try {
        return configurePlugin(name, config, c)
      } catch {
        case e: ConfigException.Missing =>
      }
    }
    if (!meta.configs.isEmpty)
      println(s"Valid configurations: ${meta.configs}")
    else
      println(s"No valid configurations exists")
    throw new Exception(s"No valid plugin constructor found for $name")
  }

  def invokePlugins(plugins: List[ConfiguredPlugin]) {
    plugins match {
      case head :: tail =>
        val args = Future.sequence(head.args.map {
          case Right(d) =>
            Future
              .sequence(d.dependencies.map { dep =>
                val selection = ActorSelection(self, actorName(dep.name)).resolveOne
                if (d.isPluginRef) {
                  selection.map { ref =>
                    new PluginRef(ref, dep.config.getOrElse(null))
                  }
                } else {
                  selection
                }
              })
              .map { as =>
                listType(d.t, as)
              }
          case Left(obj) => Future.successful(obj)
        })
        args.onFailure {
          case e => println(s"Failed to start plugin ${head.name} due to $e")
        }
        for (a <- args) {
          val props = head.method.invoke(null, a: _*).asInstanceOf[Props]
          val actor = context.actorOf(props, actorName(head.name))
          println(s"Started plugin ${head.name}")
          if (head.name == "universe") {
            println("Universe started, updating proxy")
            universeProxy ! SetUniverse(actor)
          }
          actor.tell(globalConfig, universeProxy)
          invokePlugins(tail)
        }
      case _ => Nil
    }
  }

  def receive = {
    case Start =>
      val objs =
        config.root().entrySet.asScala.filter(_.getValue.valueType == com.typesafe.config.ConfigValueType.OBJECT)
      val plugins = (for (e <- objs) yield {
        val name = e.getKey
        val plugin = config.getConfig(name)
        if (plugin.hasPath("class")) {
          val clazz = plugin.getString("class")
          val meta = PluginMeta(clazz)

          val pluginConf = configurePlugin(name, plugin, meta)
          println(s"Validated configuration for $name")
          Some(pluginConf)
        } else {
          println(s"$name has no class set, ignoring")
          None
        }
      }) flatten

      val pluginMap = plugins.map { p =>
        (p.name, p)
      }.toMap

      val pluginEdges = plugins.flatMap(_.dependencyEdges)

      println(s"Plugin dependencies: $pluginEdges")

      println("Resolving dependency order ...")
      val sortedPlugins = tsort(pluginEdges).map(pluginMap).toSeq.reverse
      val allPlugins = sortedPlugins ++ (plugins.toSet &~ sortedPlugins.toSet).toSeq
      println(s"Loading plugins in dependency order: ${allPlugins.map(_.name)}")
      invokePlugins(allPlugins.toList)
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

class UniverseProxyActor extends Actor with Stash {
  import UniverseProxyActor.SetUniverse

  def receive = {
    case SetUniverse(universe) =>
      unstashAll()
      context.become(ready(universe))
    case _ =>
      stash()
  }

  def ready(universe: ActorRef): Receive = {
    case o =>
      universe.forward(o)
  }

}

object UniverseProxyActor {
  case class SetUniverse(universe: ActorRef)
  def props() = Props(classOf[UniverseProxyActor])
}
