package konstructs

import org.scalatest.{ Matchers, WordSpec }

import konstructs.api._

class GeometrySpec extends WordSpec with Matchers {

  "A Position" should {
    "return chunk 0, 0 for 0, 0, 0" in {
      ChunkPosition(Position(0, 0, 0)) shouldEqual ChunkPosition(0, 0, 0)
    }

    "return chunk 0, 1 for 0, 0, 32" in {
      ChunkPosition(Position(0, 0, 32)) shouldEqual ChunkPosition(0, 1, 0)
    }

    "return chunk 1, 0 for 32, 0, 0" in {
      ChunkPosition(Position(32, 0, 0)) shouldEqual ChunkPosition(1, 0, 0)
    }

    "return chunk 1, 1 for 32, 0, 32" in {
      ChunkPosition(Position(32, 0, 32)) shouldEqual ChunkPosition(1, 1, 0)
    }

    "return chunk 0, 0 for 31, 0, 31" in {
      ChunkPosition(Position(31, 0, 31)) shouldEqual ChunkPosition(0, 0, 0)
    }

    "return chunk 0, -1 for 0, 0, -1" in {
      ChunkPosition(Position(0, 0, -1)) shouldEqual ChunkPosition(0, -1, 0)
    }

    "return chunk -1, -1 for -1, 0, -1" in {
      ChunkPosition(Position(-1, 0, -1)) shouldEqual ChunkPosition(-1, -1, 0)
    }

    "return chunk -1, -1 for -32, 0, -32" in {
      ChunkPosition(Position(-32, 0, -32)) shouldEqual ChunkPosition(-1, -1, 0)
    }

    "return chunk -2, -2 for -33, 0, -33" in {
      ChunkPosition(Position(-33, 0, -33)) shouldEqual ChunkPosition(-2, -2, 0)
    }

    "return chunk -1, 0 for -1, 0, 0" in {
      ChunkPosition(Position(-1, 0, 0)) shouldEqual ChunkPosition(-1, 0, 0)
    }

  }

  "A Box" should {
    "contain 0, 0, 0 in (-32, 0, -32) (32, 0, 32)" in {
      Box(Position(-32, 0, -32), Position(32, 1, 32)).contains(Position(0, 0, 0)) shouldEqual true
    }

    "contain ChunkPosition(0, 0, 0) in (-32, 0, -32) (32, 0, 32)" in {
      Box(Position(-32, 0, -32), Position(32, 1, 32)).contains(ChunkPosition(0, 0, 0)) shouldEqual true
    }

    "single block query (boundary)" in {
      Box(Position(0, 0, 31), Position(0, 0, 32)).chunked shouldEqual Set(
        Box(Position(0, 0, 31), Position(0, 0, 32))
      )
    }

    "match two block query (boundary)" in {
      Box(Position(0, 0, 31), Position(0, 0, 33)).chunked shouldEqual Set(
        Box(Position(0, 0, 31), Position(0, 0, 32)),
        Box(Position(0, 0, 32), Position(0, 0, 33))
      )
    }

    "match single block query (negative boundary)" in {
      Box(Position(0, 0, -33), Position(0, 0, -32)).chunked shouldEqual Set(
        Box(Position(0, 0, -33), Position(0, 0, -32))
      )
    }

    "match two block query (negative boundary)" in {
      Box(Position(0, 0, -33), Position(0, 0, -31)).chunked shouldEqual Set(
        Box(Position(0, 0, -33), Position(0, 0, -32)),
        Box(Position(0, 0, -32), Position(0, 0, -31))
      )
    }

    "split in two chunks (one dimension)" in {
      Box(Position(0, 0, 1), Position(0, 0, 33)).chunked shouldEqual Set(
        Box(Position(0, 0, 1), Position(0, 0, 32)),
        Box(Position(0, 0, 32), Position(0, 0, 33))
      )
    }

    "split in three chunks (one dimension)" in {
      Box(Position(0, 0, -1), Position(0, 0, 33)).chunked shouldEqual Set(
        Box(Position(0, 0, -1), Position(0, 0, 0)),
        Box(Position(0, 0, 0), Position(0, 0, 32)),
        Box(Position(0, 0, 32), Position(0, 0, 33))
      )
    }

    "split in four chunks (two dimensions)" in {
      Box(Position(0, 1, 1), Position(0, 33,33)).chunked shouldEqual Set(
        Box(Position(0,1,1),Position(0,32,32)),
        Box(Position(0,1,32),Position(0,32,33)),
        Box(Position(0,32,1),Position(0,33,32)),
        Box(Position(0,32,32),Position(0,33,33)))
    }

    "within one chunk" in {
      Box(Position(1, 1, 1), Position(12, 12, 12)).chunked shouldEqual Set(
        Box(Position(1, 1, 1),Position(12, 12, 12)))
    }

    "negative only (one dimension)" in {
      Box(Position(0, 0, -67), Position(0, 0, -1)).chunked shouldEqual Set(
        Box(Position(0, 0, -67), Position(0, 0, -64)),
        Box(Position(0, 0, -64), Position(0, 0, -32)),
        Box(Position(0, 0, -32), Position(0, 0, -1))
      )

    }

    "throw exception if end is bigger than start" in {
      intercept[IllegalArgumentException] {
        Box(Position(0, 0, -67), Position(0, 0, -69))
      }
    }

    "return a correct box size" in {
      Box(Position(0, 0, 0), Position(1, 1, 1)).blocks shouldEqual 1
      Box(Position(0, 0, 0), Position(2, 2, 2)).blocks shouldEqual 8
      Box(Position(-1, -1, -1), Position(1, 1, 1)).blocks shouldEqual 8
    }

    "return a correct internal indexes (1 block box)" in {
      val box = Box(Position(0, 0, 0), Position(1, 1, 1))
      box.index(Position(0, 0, 0)) shouldEqual 0
    }

    "return a correct internal indexes (4 block box, no z)" in {
      val box = Box(Position(0, 0, 0), Position(2, 2, 1))
      box.index(Position(0, 0, 0)) shouldEqual 0
      box.index(Position(1, 1, 0)) shouldEqual 3
    }

    "return a correct internal indexes (8 block box)" in {
      val box = Box(Position(0, 0, 0), Position(2, 2, 2))
      box.index(Position(0, 0, 0)) shouldEqual 0
      box.index(Position(1, 1, 0)) shouldEqual 6
      box.index(Position(1, 1, 1)) shouldEqual 7
    }

    "return a correct internal indexes (45 block box)" in {
      val box = Box(Position(0, 0, 0), Position(3, 3, 5))
      box.index(Position(0, 0, 0)) shouldEqual 0
      box.index(Position(1, 1, 0)) shouldEqual 20
      box.index(Position(2, 2, 4)) shouldEqual 44
    }

    "return a correct internal indexes (8 block box away from center)" in {
      val box = Box(Position(1, 1, 1), Position(3, 3, 3))
      box.index(Position(1, 1, 1)) shouldEqual 0
      box.index(Position(2, 2, 1)) shouldEqual 6
      box.index(Position(2, 2, 2)) shouldEqual 7
    }

  }

}
