package konstructs.shard

import org.scalatest.{ Matchers, WordSpec }

class DbSpec extends WordSpec with Matchers {

  "A ChunkPosition" should {
    "Return local index 0 for chunk 0, 0, 0" in {
      ShardActor.index(ChunkPosition(0, 0, 0)) shouldEqual 0
    }

    "Return local index 8*8*8 - 1 for chunk 7, 7, 7" in {
      ShardActor.index(ChunkPosition(7, 7, 7)) shouldEqual (8*8*8 - 1)
    }

  }

}
