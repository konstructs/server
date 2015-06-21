package konstructs

import akka.actor.ActorRef

trait Filter[T] {
  def chain: Seq[ActorRef]
  def next(chain: Seq[ActorRef]): Filter[T]
  def next(chain: Seq[ActorRef], message: T): Filter[T]

  def continue(implicit sender: ActorRef) {
    chain.head ! next(chain = chain.tail)
  }

  def continueWith(newMessage: T)(implicit sender: ActorRef) {
    chain.head ! next(chain = chain.tail, message = newMessage)
  }

  def skip(implicit sender: ActorRef) {
    chain.last ! next(chain = Seq())
  }

  def skipWith(newMessage: T)(implicit sender: ActorRef) {
    chain.last ! next(chain = Seq(), message = newMessage)
  }
}
