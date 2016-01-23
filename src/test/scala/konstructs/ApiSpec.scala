package konstructs.api

import scala.collection.JavaConverters._

import org.scalatest.{ Matchers, WordSpec }

class ApiSpec extends WordSpec with Matchers {

  val SomeType = BlockType(List(0,0,0,0,0,0).asJava, BlockType.ShapeBlock, false, false)

  "A BlockFilter" should {

    "Match a namespace" in {
      val filter = BlockFilterFactory
        .withNamespace("org/test")
      filter.matches(BlockTypeId("org/test", "test"), SomeType) shouldEqual true
    }

    "Match a name" in {
      val filter = BlockFilterFactory
        .withName("test")
      filter.matches(BlockTypeId("org/test", "test"), SomeType) shouldEqual true
    }

    "Match a name and namespace" in {
      val filter = BlockFilterFactory
        .withNamespace("org/test")
        .withName("test")
      filter.matches(BlockTypeId("org/test", "test"), SomeType) shouldEqual true
    }

    "Not match an invalid namespace" in {
      val filter = BlockFilterFactory
        .withNamespace("invalid")
      filter.matches(BlockTypeId("org/test", "test"), SomeType) shouldEqual false
    }

    "Not match an invalid name" in {
      val filter = BlockFilterFactory
        .withName("invalid")
      filter.matches(BlockTypeId("org/test", "test"), SomeType) shouldEqual false
    }

    "Not match an invalid name even though namespace is correct" in {
      val filter = BlockFilterFactory
        .withNamespace("org/test")
        .withName("invalid")
      filter.matches(BlockTypeId("org/test", "test"), SomeType) shouldEqual false
    }

    "Match one name or another" in {
      val filter = BlockFilterFactory
        .withName("test")
        .or(BlockFilterFactory
          .withName("invalid"))
      filter.matches(BlockTypeId("org/test", "test"), SomeType) shouldEqual true
    }


    "Match a complex filter" in {
      val filter = BlockFilterFactory
        .withNamespace("org/konstructs/forest")
        .or(BlockFilterFactory.vacuum)
        .or(BlockFilterFactory
            .withNamespace("org/konstructs")
            .withName("wood"))
        .or(BlockFilterFactory
            .withNamespace("org/konstructs")
            .withName("leaves"));
      filter.matches(BlockTypeId("org/konstructs", "vacuum"), SomeType) shouldEqual true
      filter.matches(BlockTypeId("org/konstructs", "leaves"), SomeType) shouldEqual true
      filter.matches(BlockTypeId("org/konstructs", "wood"), SomeType) shouldEqual true
      filter.matches(BlockTypeId("org/konstructs/forest", "sapling"), SomeType) shouldEqual true
    }


  }

}
