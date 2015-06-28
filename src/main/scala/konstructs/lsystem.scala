package konstructs

import scala.util.{ Sorting, Random }
import scala.math.Ordering
import scala.collection.JavaConverters._

trait ProductionRule {
  def predecessor: String
  def successor: String
}

case class ProbabilisticProductionRule(predecessor: String, successors: Seq[(Int, String)]) extends ProductionRule {
  assert(successors.map(_._1).sum == 100, "Rules need to sum up to 100")

  private val rand = new Random

  private def select(p: Int, current: Int, successors: Seq[(Int, String)]): String = {
    val h = successors.head
    if(p < h._1 + current) {
      h._2
    } else {
      select(p, current + h._1, successors.tail)
    }
  }

  def successor: String = {
    val p = rand.nextInt(100)
    select(p, 0, successors)
  }
}

case class DeterministicProductionRule(predecessor: String, successor: String) extends ProductionRule

case class LSystem(rules: Seq[ProductionRule]) {
  val orderedRules = {
    val arr = rules.toArray
    Sorting.quickSort(arr)(Ordering.by[ProductionRule, Int](_.predecessor.length))
    arr.reverse.toSeq
  }

  def longestMatch(production: String): Option[ProductionRule] =
    orderedRules filter { r =>
      production.startsWith(r.predecessor)
    } headOption

  def iterate(production: String): String = {
    if(production != "") {
      longestMatch(production) match {
        case Some(rule) => rule.successor + iterate(production.drop(rule.predecessor.size))
        case None => production.head + iterate(production.tail)
      }
    } else {
      ""
    }
  }

  def iterate(init: String, iterations: Int): String = {
    if(iterations > 0) {
      iterate(iterate(init), iterations - 1)
    } else {
      init
    }
  }
}

object LSystem {
  def fromList(rules: java.util.List[ProductionRule]): LSystem =
    apply(rules.asScala.toSeq)
}
