package konstructs

import akka.actor.ActorRef

package api {
  /* Messages for chat */
  case class Say(text: String)
  case class SayFilter(chain: Seq[ActorRef], message: Say) extends Filter[Say] {
    def next(chain: Seq[ActorRef]) = copy(chain = chain)
    def next(chain: Seq[ActorRef], message: Say) = copy(chain = chain, message = message)
  }
  case class Said(text: String)

  /* Messages for world interaction */
  case class PutBlock(pos: Position, w: Int)
  case class DestroyBlock(pos: Position)
  case class BlockUpdate(pos: Position, oldW: Int, newW: Int)
}
