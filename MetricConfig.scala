package konstructs.metric

import akka.actor.ActorRef

case class MetricConfig(interval: Int, listeners: Seq[ActorRef])
