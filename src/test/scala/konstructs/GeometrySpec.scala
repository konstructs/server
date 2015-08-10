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

  }

}
