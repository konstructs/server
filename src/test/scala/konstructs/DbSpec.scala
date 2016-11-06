package konstructs.shard

import org.scalatest.{ Matchers, WordSpec }

class DbSpec extends WordSpec with Matchers {

  "A ChunkPosition" should {
    "Return local index 0 for chunk 0, 0, 0" in {
      ShardActor.index(ChunkPosition(0, 0, 0), ShardPosition(0,0,0)) shouldEqual 0
    }

    "Return local index 8*8*8 - 1 for chunk 7, 7, 7" in {
      ShardActor.index(ChunkPosition(7, 7, 7), ShardPosition(0,0,0)) shouldEqual (8*8*8 - 1)
    }

    "Return local index 0 for chunk 8, 8, 8" in {
      ShardActor.index(ChunkPosition(8, 8, 8), ShardPosition(1,1,1)) shouldEqual 0
    }

    "Return local index 8*8*8 - 1 for chunk 15, 15, 15" in {
      ShardActor.index(ChunkPosition(15, 15, 15), ShardPosition(1,1,1)) shouldEqual (8*8*8 - 1)
    }

    "Return local index 0 for chunk -8, -8, -8" in {
      ShardActor.index(ChunkPosition(-8, -8, -8), ShardPosition(-1,-1,-1)) shouldEqual 0
    }

    "Return local index 8*8*8 - 1 for chunk -1, -1, -1" in {
      ShardActor.index(ChunkPosition(-1, -1, -1), ShardPosition(-1,-1,-1)) shouldEqual (8*8*8 - 1)
    }

    "Return ShardPosition(-1,-1,-1) for ChunkPosition(-1,-1,-1)" in {
      ShardPosition(ChunkPosition(-1,-1,-1)) shouldEqual ShardPosition(-1,-1,-1)
    }

    "Return ShardPosition(-1,-1,-1) for ChunkPosition(-8,-8,-8)" in {
      ShardPosition(ChunkPosition(-8,-8,-8)) shouldEqual ShardPosition(-1,-1,-1)
    }

    "Return ShardPosition(-2,-2,-2) for ChunkPosition(-9,-9,-9)" in {
      ShardPosition(ChunkPosition(-9,-9,-9)) shouldEqual ShardPosition(-2,-2,-2)
    }

    "Return ShardPosition(-1,-2,2) for ChunkPosition(-7,-9,16)" in {
      ShardPosition(ChunkPosition(-7,-9,16)) shouldEqual ShardPosition(-1,-2,2)
    }
  }

}
