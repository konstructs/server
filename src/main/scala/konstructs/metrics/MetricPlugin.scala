package konstructs.metric

import scala.collection.mutable

import akka.actor.{Actor, ActorRef, Props}

import konstructs.plugin.{PluginConstructor, Config, ListConfig}
import konstructs.api.MetricId
import konstructs.api.messages.{IncreaseMetric, SetMetric}
import konstructs.utils.Scheduled

class MetricPlugin(universe: ActorRef, config: MetricConfig) extends Actor with Scheduled {
  import MetricPlugin.SendMetrics

  val metrics = mutable.Map[MetricId, Long]()

  schedule(config.interval, SendMetrics)

  def receive = {
    case u: IncreaseMetric =>
      val id = u.getId
      val v = metrics.getOrElse(id, 0.toLong) + u.getIncrease
      metrics += id -> v
    case u: SetMetric =>
      val id = u.getId
      val v = u.getValue
      metrics += id -> v
    case SendMetrics =>
      for ((k, v) <- metrics; l <- config.listeners) {
        l ! new SetMetric(k, v.toInt)
      }
  }
}

object MetricPlugin {
  import konstructs.plugin.Plugin.nullAsEmpty
  case object SendMetrics

  @PluginConstructor
  def props(
      name: String,
      universe: ActorRef,
      @Config(key = "interval") interval: Int,
      @ListConfig(
        key = "listeners",
        elementType = classOf[ActorRef],
        optional = true
      ) listeners: Seq[ActorRef]
  ) =
    Props(
      classOf[MetricPlugin],
      universe,
      MetricConfig(interval, nullAsEmpty(listeners))
    )
}
