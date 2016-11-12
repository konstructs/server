package konstructs.utils

import java.util.concurrent.TimeUnit
import scala.util.Random
import scala.concurrent.duration.Duration
import akka.actor.Actor

trait Scheduled { actor: Actor =>
  def schedule(millis: Int, msg: Any) {
    val init = Duration(new Random().nextInt(millis), TimeUnit.MILLISECONDS)
    val freq = Duration(millis, TimeUnit.MILLISECONDS)
    context.system.scheduler.schedule(init, freq, self, msg)(context.dispatcher)
  }
}
