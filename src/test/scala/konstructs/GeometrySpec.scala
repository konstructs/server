package konstructs

import org.scalatest.{ Matchers, WordSpec }

import konstructs.api._

class GeometrySpec extends WordSpec with Matchers {

  "A Position" should {
    "return chunk 0, 0 for 0, 0, 0" in {
      ChunkPosition(new Position(0, 0, 0)) shouldEqual ChunkPosition(0, 0, 0)
    }

    "return chunk 0, 1 for 0, 0, 32" in {
      ChunkPosition(new Position(0, 0, 32)) shouldEqual ChunkPosition(0, 1, 0)
    }

    "return chunk 1, 0 for 32, 0, 0" in {
      ChunkPosition(new Position(32, 0, 0)) shouldEqual ChunkPosition(1, 0, 0)
    }

    "return chunk 1, 1 for 32, 0, 32" in {
      ChunkPosition(new Position(32, 0, 32)) shouldEqual ChunkPosition(1, 1, 0)
    }

    "return chunk 0, 0 for 31, 0, 31" in {
      ChunkPosition(new Position(31, 0, 31)) shouldEqual ChunkPosition(0, 0, 0)
    }

    "return chunk 0, -1 for 0, 0, -1" in {
      ChunkPosition(new Position(0, 0, -1)) shouldEqual ChunkPosition(0, -1, 0)
    }

    "return chunk -1, -1 for -1, 0, -1" in {
      ChunkPosition(new Position(-1, 0, -1)) shouldEqual ChunkPosition(-1, -1, 0)
    }

    "return chunk -1, -1 for -32, 0, -32" in {
      ChunkPosition(new Position(-32, 0, -32)) shouldEqual ChunkPosition(-1, -1, 0)
    }

    "return chunk -2, -2 for -33, 0, -33" in {
      ChunkPosition(new Position(-33, 0, -33)) shouldEqual ChunkPosition(-2, -2, 0)
    }

    "return chunk -1, 0 for -1, 0, 0" in {
      ChunkPosition(new Position(-1, 0, 0)) shouldEqual ChunkPosition(-1, 0, 0)
    }

  }

  "A Box" should {
    "contain 0, 0, 0 in (-32, 0, -32) (32, 0, 32)" in {
      new Box(new Position(-32, 0, -32), new Position(32, 1, 32)).contains(new Position(0, 0, 0)) shouldEqual true
    }

    "contain ChunkPosition(0, 0, 0) in (-32, 0, -32) (32, 0, 32)" in {
      BoxChunking.contains(new Box(new Position(-32, 0, -32), new Position(32, 1, 32)), ChunkPosition(0, 0, 0)) shouldEqual true
    }

    "single block query (boundary)" in {
      BoxChunking.chunked(new Box(new Position(0, 0, 31), new Position(0, 0, 32))) shouldEqual Set(
        new Box(new Position(0, 0, 31), new Position(0, 0, 32))
      )
    }

    "match two block query (boundary)" in {
      BoxChunking.chunked(new Box(new Position(0, 0, 31), new Position(0, 0, 33))) shouldEqual Set(
        new Box(new Position(0, 0, 31), new Position(0, 0, 32)),
        new Box(new Position(0, 0, 32), new Position(0, 0, 33))
      )
    }

    "match single block query (negative boundary)" in {
      BoxChunking.chunked(new Box(new Position(0, 0, -33), new Position(0, 0, -32))) shouldEqual Set(
        new Box(new Position(0, 0, -33), new Position(0, 0, -32))
      )
    }

    "match two block query (negative boundary)" in {
      BoxChunking.chunked(new Box(new Position(0, 0, -33), new Position(0, 0, -31))) shouldEqual Set(
        new Box(new Position(0, 0, -33), new Position(0, 0, -32)),
        new Box(new Position(0, 0, -32), new Position(0, 0, -31))
      )
    }

    "split in two chunks (one dimension)" in {
      BoxChunking.chunked(new Box(new Position(0, 0, 1), new Position(0, 0, 33))) shouldEqual Set(
        new Box(new Position(0, 0, 1), new Position(0, 0, 32)),
        new Box(new Position(0, 0, 32), new Position(0, 0, 33))
      )
    }

    "split in three chunks (one dimension)" in {
      BoxChunking.chunked(new Box(new Position(0, 0, -1), new Position(0, 0, 33))) shouldEqual Set(
        new Box(new Position(0, 0, -1), new Position(0, 0, 0)),
        new Box(new Position(0, 0, 0), new Position(0, 0, 32)),
        new Box(new Position(0, 0, 32), new Position(0, 0, 33))
      )
    }

    "split in four chunks (two dimensions)" in {
      BoxChunking.chunked(new Box(new Position(0, 1, 1), new Position(0, 33,33))) shouldEqual Set(
        new Box(new Position(0,1,1),new Position(0,32,32)),
        new Box(new Position(0,1,32),new Position(0,32,33)),
        new Box(new Position(0,32,1),new Position(0,33,32)),
        new Box(new Position(0,32,32),new Position(0,33,33)))
    }

    "within one chunk" in {
      BoxChunking.chunked(new Box(new Position(1, 1, 1), new Position(12, 12, 12))) shouldEqual Set(
        new Box(new Position(1, 1, 1),new Position(12, 12, 12)))
    }

    "negative only (one dimension)" in {
      BoxChunking.chunked(new Box(new Position(0, 0, -67), new Position(0, 0, -1))) shouldEqual Set(
        new Box(new Position(0, 0, -67), new Position(0, 0, -64)),
        new Box(new Position(0, 0, -64), new Position(0, 0, -32)),
        new Box(new Position(0, 0, -32), new Position(0, 0, -1))
      )

    }

    "throw exception if end is bigger than start" in {
      intercept[IllegalArgumentException] {
        new Box(new Position(0, 0, -67), new Position(0, 0, -69))
      }
    }

    "return a correct box size" in {
      new Box(new Position(0, 0, 0), new Position(1, 1, 1)).getNumberOfBlocks shouldEqual 1
      new Box(new Position(0, 0, 0), new Position(2, 2, 2)).getNumberOfBlocks shouldEqual 8
      new Box(new Position(-1, -1, -1), new Position(1, 1, 1)).getNumberOfBlocks shouldEqual 8
    }

    "return a correct internal indexes (1 block box)" in {
      val box = new Box(new Position(0, 0, 0), new Position(1, 1, 1))
      box.arrayIndex(new Position(0, 0, 0)) shouldEqual 0
    }

    "return a correct internal indexes (4 block box, no z)" in {
      val box = new Box(new Position(0, 0, 0), new Position(2, 2, 1))
      box.arrayIndex(new Position(0, 0, 0)) shouldEqual 0
      box.arrayIndex(new Position(1, 1, 0)) shouldEqual 3
    }

    "return a correct internal indexes (8 block box)" in {
      val box = new Box(new Position(0, 0, 0), new Position(2, 2, 2))
      box.arrayIndex(new Position(0, 0, 0)) shouldEqual 0
      box.arrayIndex(new Position(1, 1, 0)) shouldEqual 6
      box.arrayIndex(new Position(1, 1, 1)) shouldEqual 7
    }

    "return a correct internal indexes (45 block box)" in {
      val box = new Box(new Position(0, 0, 0), new Position(3, 3, 5))
      box.arrayIndex(new Position(0, 0, 0)) shouldEqual 0
      box.arrayIndex(new Position(1, 1, 0)) shouldEqual 20
      box.arrayIndex(new Position(2, 2, 4)) shouldEqual 44
    }

    "return a correct internal indexes (8 block box away from center)" in {
      val box = new Box(new Position(1, 1, 1), new Position(3, 3, 3))
      box.arrayIndex(new Position(1, 1, 1)) shouldEqual 0
      box.arrayIndex(new Position(2, 2, 1)) shouldEqual 6
      box.arrayIndex(new Position(2, 2, 2)) shouldEqual 7
    }

  }

}
