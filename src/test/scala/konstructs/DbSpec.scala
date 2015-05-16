package konstructs

import org.scalatest.{ Matchers, WordSpec }

class DbSpec extends WordSpec with Matchers {

  "A Position" should {
    "return chunk 0, 0 for 0, 0, 0" in {
      Position(0, 0, 0).chunk shouldEqual ChunkPosition(0, 0, 0)
    }

    "return chunk 0, 1 for 0, 0, 32" in {
      Position(0, 0, 32).chunk shouldEqual ChunkPosition(0, 1, 0)
    }

    "return chunk 1, 0 for 32, 0, 0" in {
      Position(32, 0, 0).chunk shouldEqual ChunkPosition(1, 0, 0)
    }

    "return chunk 1, 1 for 32, 0, 32" in {
      Position(32, 0, 32).chunk shouldEqual ChunkPosition(1, 1, 0)
    }

    "return chunk 0, 0 for 31, 0, 31" in {
      Position(31, 0, 31).chunk shouldEqual ChunkPosition(0, 0, 0)
    }

    "return chunk 0, -1 for 0, 0, -1" in {
      Position(0, 0, -1).chunk shouldEqual ChunkPosition(0, -1, 0)
    }

    "return chunk -1, -1 for -1, 0, -1" in {
      Position(-1, 0, -1).chunk shouldEqual ChunkPosition(-1, -1, 0)
    }

    "return chunk -1, -1 for -32, 0, -32" in {
      Position(-32, 0, -32).chunk shouldEqual ChunkPosition(-1, -1, 0)
    }

    "return chunk -2, -2 for -33, 0, -33" in {
      Position(-33, 0, -33).chunk shouldEqual ChunkPosition(-2, -2, 0)
    }

    "return chunk -1, 0 for -1, 0, 0" in {
      Position(-1, 0, 0).chunk shouldEqual ChunkPosition(-1, 0, 0)
    }

    "return local position 0, 0, 0 for 0, 0, 0" in {
      Position(0, 0, 0).local shouldEqual LocalPosition(0, 0, 0)
    }

    "return local position 5, 0, 5 for 5, 0, 5" in {
      Position(5, 0, 5).local shouldEqual LocalPosition(5, 0, 5)
    }

    "return local position 5, 0, 5 for 37, 0, 37" in {
      Position(5, 0, 5).local shouldEqual LocalPosition(5, 0, 5)
    }

    "return local position 0, 0, 0 for 32, 0, 32" in {
      Position(32, 0, 32).local shouldEqual LocalPosition(0, 0, 0)
    }

    "return local position 0, 0, 0 for -32, 0, -32" in {
      Position(-32, 0, -32).local shouldEqual LocalPosition(0, 0, 0)
    }

    "return local position 31, 0, 31 for -1, 0, -1" in {
      Position(-32, 0, -32).local shouldEqual LocalPosition(0, 0, 0)
    }

    "return local position 31, 0, 31 for -33, 0, -33" in {
      Position(-33, 0, -33).local shouldEqual LocalPosition(31, 0, 31)
    }

    "return local position 31, 0, 0 for -1, 0, -32" in {
      Position(-1, 0, -32).local shouldEqual LocalPosition(31, 0, 0)
    }

  }

  "A LocalPosition" should {
    "Return global 0, 0, 0 for local 0, 0, 0 in chunk 0, 0" in {
      val c = ChunkPosition(0, 0, 0)
      LocalPosition(0, 0, 0).global(c) shouldEqual Position(0, 0, 0)
    }

    "Return global 0, 0, -32 for local 0, 0, 0 in chunk 0, -1" in {
      val c = ChunkPosition(0, -1, 0)
      LocalPosition(0, 0, 0).global(c) shouldEqual Position(0, 0, -32)
    }

    "Return global -32, 0, 0 for local 0, 0, 0 in chunk -1, 0" in {
      val c = ChunkPosition(-1, 0, 0)
      LocalPosition(0, 0, 0).global(c) shouldEqual Position(-32, 0, 0)
    }

    "Return global -32, 0, -32 for local 0, 0, 0 in chunk -1, -1" in {
      val c = ChunkPosition(-1, -1, 0)
      LocalPosition(0, 0, 0).global(c) shouldEqual Position(-32, 0, -32)
    }

    "Return global 32, 0, -32 for local 0, 0, 0 in chunk 1, -1" in {
      val c = ChunkPosition(1, -1, 0)
      LocalPosition(0, 0, 0).global(c) shouldEqual Position(32, 0, -32)
    }

    "Return global 0, 0, -33 for local 0, 0, 31 in chunk 0, -2" in {
      val c = ChunkPosition(0, -2, 0)
      LocalPosition(0, 0, 31).global(c) shouldEqual Position(0, 0, -33)
    }

    "Return global 32, 0, 32 for local 0, 0, 0 in chunk 1, 1" in {
      val c = ChunkPosition(1, 1, 0)
      LocalPosition(0, 0, 0).global(c) shouldEqual Position(32, 0, 32)
    }

  }

  "A ChunkPosition" should {
    "Return local index 0 for chunk 0, 0, 0" in {
      ChunkPosition(0, 0, 0).index shouldEqual 0
    }

    "Return local index 8*8*8 - 1 for chunk 7, 7, 7" in {
      ChunkPosition(7, 7, 7).index shouldEqual (8*8*8 - 1)
    }

  }

}
