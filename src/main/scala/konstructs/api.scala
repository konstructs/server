package konstructs

import akka.actor.ActorRef

package object api {
  case class Say(message: String)
  case class SayFilter(chain: Seq[ActorRef], message: Say) extends Filter[Say] {
    def next(chain: Seq[ActorRef]) = copy(chain = chain)
    def next(chain: Seq[ActorRef], message: Say) = copy(chain = chain, message = message)
  }
  case class Said(message: String)
}
