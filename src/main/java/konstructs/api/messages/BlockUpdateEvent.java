package konstructs.api.messages;

import konstructs.api.BlockUpdate;
import konstructs.api.Position;

import java.util.Map;
public class BlockUpdateEvent {
    private final Map<Position, BlockUpdate> updatedBlocks;

    public BlockUpdateEvent(Map<Position, BlockUpdate> updatedBlocks) {
        this.updatedBlocks = updatedBlocks;
    }

    public Map<Position, BlockUpdate> getUpdatedBlocks() {
        return updatedBlocks;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        BlockUpdateEvent that = (BlockUpdateEvent) o;

        return updatedBlocks.equals(that.updatedBlocks);

    }

    @Override
    public int hashCode() {
        return updatedBlocks.hashCode();
    }

    @Override
    public String toString() {
        return "BlockUpdateEvent(" +
                "updatedBlocks=" + updatedBlocks +
                ')';
    }
}
