package konstructs.metric

import scala.collection.mutable
import akka.actor.{Actor, ActorRef, Props}
import konstructs.plugin.PluginConstructor
import konstructs.api.messages.SetMetric
import konstructs.api.MetricId

class PrintMetricPlugin extends Actor {

  val oldMetrics = mutable.Map[MetricId, Long]()

  def receive = {
    case s: SetMetric =>
      val old = oldMetrics.getOrElse(s.getId, 0.toLong)
      oldMetrics += s.getId -> s.getValue
      val v = s.getValue - old
      println(s"${s.getId.toMetricString}: $v")
  }

}

object PrintMetricPlugin {
  @PluginConstructor
  def props(name: String, universe: ActorRef) =
    Props(classOf[PrintMetricPlugin])
}
