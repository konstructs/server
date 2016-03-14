package konstructs
import scala.collection.JavaConverters._
import org.scalatest.{ Matchers, WordSpec }

import konstructs.api._

class KonstructingSpec extends WordSpec with Matchers {

  val One = new BlockTypeId("test", "1")
  val Two = new BlockTypeId("test", "2")
  val Three = new BlockTypeId("test", "3")

  def s(t: BlockTypeId) = Stack.createFromBlock(new Block(null, t))
  def b(t: BlockTypeId) = new Block(null, t)

  "A Inventory" should {
    "produce a 1x1 pattern for a 1x1 inventory" in {
      val view = new InventoryView(0,0,1,1)
      val i = new Inventory(Array(s(One)))
      i.getPattern(view) shouldEqual new Pattern(Array(s(One)), 1, 1)
    }
    "produce a 1x1 pattern for a 2x2 inventory if only one stack" in {
      val view = new InventoryView(0,0,2,2)
      val i = new Inventory(Array(s(One), null, null, null))
      i.getPattern(view) shouldEqual new Pattern(Array(s(One)), 1, 1)
    }
    "produce a 1x1 pattern for a 2x2 inventory if only one stack on another position" in {
      val view = new InventoryView(0,0,2,2)
      val i = new Inventory(Array(null, null, s(One), null))
      i.getPattern(view) shouldEqual new Pattern(Array(s(One)), 1, 1)
    }
    "produce a 1x2 pattern for a 2x2 inventory" in {
      val view = new InventoryView(0,0,2,2)
      val i = new Inventory(Array(s(One), s(Two), null, null))
      i.getPattern(view) shouldEqual new Pattern(Array(s(One), s(Two)), 1, 2)
    }
    "produce a 1x2 pattern for a 2x2 inventory on second row" in {
      val view = new InventoryView(0,0,2,2)
      val i = new Inventory(Array( null, null, s(One), s(Two)))
      i.getPattern(view) shouldEqual new Pattern(Array(s(One), s(Two)), 1, 2)
    }
    "produce a 2x1 pattern for a 2x2 inventory" in {
      val view = new InventoryView(0,0,2,2)
      val i = new Inventory(Array(s(One), null, s(Two), null))
      i.getPattern(view) shouldEqual new Pattern(Array(s(One), s(Two)), 2, 1)
    }
    "produce a 2x1 pattern for a 2x2 inventory on second column" in {
      val view = new InventoryView(0,0,2,2)
      val i = new Inventory(Array( null, s(One), null, s(Two)))
      i.getPattern(view) shouldEqual new Pattern(Array(s(One), s(Two)), 2, 1)
    }
    "produce no pattern if empty" in {
      val view = new InventoryView(0,0,1,1)
      val i = new Inventory(Array[Stack]())
      i.getPattern(view) shouldEqual null
    }
  }
  "A Pattern" should {
    "contain a 1x1 pattern in a 1x1 pattern" in {
      new Pattern(Array(s(One)), 1, 1).contains(new Pattern(Array(s(One)), 1, 1))
    }
    "contain a 1x1 pattern in a 1x1 pattern with more blocks" in {
      new Pattern(Array(new Stack(One, Array(b(One), b(One), b(One)))), 1, 1).contains(new Pattern(Array(s(One)), 1, 1))
    }
    "not contain a 1x1 pattern in an empty" in {
      new Pattern(Array(null), 1, 1).contains(new Pattern(Array(s(One)), 1, 1)) shouldEqual false
    }
    "contain a 1x2 pattern in a 1x2 pattern" in {
      new Pattern(Array(s(One), s(Two)), 1, 2).contains(new Pattern(Array(s(One), s(Two)), 1, 2)) shouldEqual true
    }
    "contain a 1x2 pattern in a 1x2 pattern with more blocks" in {
      new Pattern(Array(new Stack(One, Array(b(One), b(One), b(One))), s(Two)), 1, 2).contains(new Pattern(Array(s(One), s(Two)), 1, 2)) shouldEqual true
    }
    "not contain a 1x2 pattern in an invalid 1x2 pattern" in {
      new Pattern(Array(s(Three), s(Two)), 1, 2).contains(new Pattern(Array(s(One), s(Two)), 1, 2)) shouldEqual false
    }
  }

}
