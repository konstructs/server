package konstructs.api.messages;

import konstructs.api.BlockTypeId;
import konstructs.api.Position;

import java.util.Map;

/**
 * EventBlockUpdate is a message received when a block is updated.
 */
public class EventBlockUpdated {
    private final Map<Position, BlockTypeId> blocks;

    /**
     * Construct an immutable EventBlockUpdated
     * @param blocks The Mapping between Positions and BlockTypeId
     *               of the updated blocks.
     */
    public EventBlockUpdated(Map<Position, BlockTypeId> blocks) {
        this.blocks = blocks;
    }

    /**
     * Get the mapping between position and BlockTypeId for all
     * the updated blocks
     * @return The Position to BlockTypeId mapping
     */
    public Map<Position, BlockTypeId> getBlocks() {
        return blocks;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        EventBlockUpdated that = (EventBlockUpdated) o;

        return blocks.equals(that.blocks);

    }

    @Override
    public int hashCode() {
        return blocks.hashCode();
    }

    @Override
    public String toString() {
        return "EventBlockUpdated(" +
                "blocks=" + blocks +
                ')';
    }
}
