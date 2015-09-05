package konstructs
import scala.collection.JavaConverters._
import org.scalatest.{ Matchers, WordSpec }

import konstructs.api._

class KonstructingSpec extends WordSpec with Matchers {

  "A Inventory" should {
    "produce a 1x1 pattern for a 1x1 inventory" in {
      val view = InventoryView(0,0,1,1)
      val i = Inventory(List(Stack.fromBlock(Block(None, 1))).asJava)
      i.pattern(view) shouldEqual Some(Pattern(List(Stack.fromBlock(Block(None, 1))).asJava, 1, 1))
    }
    "produce a 1x1 pattern for a 2x2 inventory if only one stack" in {
      val view = InventoryView(0,0,2,2)
      val i = Inventory(List(Stack.fromBlock(Block(None, 1)), Stack.Empty, Stack.Empty, Stack.Empty).asJava)
      i.pattern(view) shouldEqual Some(Pattern(List(Stack.fromBlock(Block(None, 1))).asJava, 1, 1))
    }
    "produce a 1x1 pattern for a 2x2 inventory if only one stack on another position" in {
      val view = InventoryView(0,0,2,2)
      val i = Inventory(List(Stack.Empty, Stack.Empty, Stack.fromBlock(Block(None, 1)), Stack.Empty).asJava)
      i.pattern(view) shouldEqual Some(Pattern(List(Stack.fromBlock(Block(None, 1))).asJava, 1, 1))
    }
    "produce a 1x2 pattern for a 2x2 inventory" in {
      val view = InventoryView(0,0,2,2)
      val i = Inventory(List(Stack.fromBlock(Block(None, 1)), Stack.fromBlock(Block(None, 2)), Stack.Empty, Stack.Empty).asJava)
      i.pattern(view) shouldEqual Some(Pattern(List(Stack.fromBlock(Block(None, 1)), Stack.fromBlock(Block(None, 2))).asJava, 1, 2))
    }
    "produce a 1x2 pattern for a 2x2 inventory on second row" in {
      val view = InventoryView(0,0,2,2)
      val i = Inventory(List( Stack.Empty, Stack.Empty, Stack.fromBlock(Block(None, 1)), Stack.fromBlock(Block(None, 2))).asJava)
      i.pattern(view) shouldEqual Some(Pattern(List(Stack.fromBlock(Block(None, 1)), Stack.fromBlock(Block(None, 2))).asJava, 1, 2))
    }
    "produce a 2x1 pattern for a 2x2 inventory" in {
      val view = InventoryView(0,0,2,2)
      val i = Inventory(List(Stack.fromBlock(Block(None, 1)), Stack.Empty, Stack.fromBlock(Block(None, 2)), Stack.Empty).asJava)
      i.pattern(view) shouldEqual Some(Pattern(List(Stack.fromBlock(Block(None, 1)), Stack.fromBlock(Block(None, 2))).asJava, 2, 1))
    }
    "produce a 2x1 pattern for a 2x2 inventory on second column" in {
      val view = InventoryView(0,0,2,2)
      val i = Inventory(List( Stack.Empty, Stack.fromBlock(Block(None, 1)), Stack.Empty, Stack.fromBlock(Block(None, 2))).asJava)
      i.pattern(view) shouldEqual Some(Pattern(List(Stack.fromBlock(Block(None, 1)), Stack.fromBlock(Block(None, 2))).asJava, 2, 1))
    }
    "produce no pattern if empty" in {
      val view = InventoryView(0,0,1,1)
      val i = Inventory(List[Stack]().asJava)
      i.pattern(view) shouldEqual None
    }
  }
  "A Pattern" should {
    "contain a 1x1 pattern in a 1x1 pattern" in {
      Pattern(List(Stack.fromBlock(Block(None, 1))).asJava, 1, 1).contains(Pattern(List(Stack.fromBlock(Block(None, 1))).asJava, 1, 1))
    }
    "contain a 1x1 pattern in a 1x1 pattern with more blocks" in {
      Pattern(List(Stack(List(Block(None, 1), Block(None, 1), Block(None, 1)).asJava)).asJava, 1, 1).contains(Pattern(List(Stack.fromBlock(Block(None, 1))).asJava, 1, 1))
    }
    "not contain a 1x1 pattern in an empty" in {
      Pattern(List(Stack.Empty).asJava, 1, 1).contains(Pattern(List(Stack.fromBlock(Block(None, 1))).asJava, 1, 1)) shouldEqual false
    }
    "contain a 1x2 pattern in a 1x2 pattern" in {
      Pattern(List(Stack.fromBlock(Block(None, 1)), Stack.fromBlock(Block(None, 2))).asJava, 1, 2).contains(Pattern(List(Stack.fromBlock(Block(None, 1)), Stack.fromBlock(Block(None, 2))).asJava, 1, 2)) shouldEqual true
    }
    "contain a 1x2 pattern in a 1x2 pattern with more blocks" in {
      Pattern(List(Stack(List(Block(None, 1), Block(None, 1), Block(None, 1)).asJava), Stack.fromBlock(Block(None, 2))).asJava, 1, 2).contains(Pattern(List(Stack.fromBlock(Block(None, 1)), Stack.fromBlock(Block(None, 2))).asJava, 1, 2)) shouldEqual true
    }
    "not contain a 1x2 pattern in an invalid 1x2 pattern" in {
      Pattern(List(Stack.fromBlock(Block(None, 3)), Stack.fromBlock(Block(None, 2))).asJava, 1, 2).contains(Pattern(List(Stack.fromBlock(Block(None, 1)), Stack.fromBlock(Block(None, 2))).asJava, 1, 2)) shouldEqual false
    }
  }

}
