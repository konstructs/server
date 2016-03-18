package konstructs.api.messages;

import konstructs.api.BlockFilter;
import konstructs.api.BlockTypeId;
import konstructs.api.Position;

import java.util.Map;

/**
 * ReplaceBlocks is a message  that replaces a set of positions with BlockTypeIds
 * if the block to be replaced matches a given filter.
 * @see konstructs.api.BlockTypeId
 * @see konstructs.api.BlockFilterFactory
 */
public class ReplaceBlocks {
    private final BlockFilter filter;
    private final Map<Position, BlockTypeId> blocks;

    /**
     * Construct an immutable ReplaceBlocks message
     * @param filter The filter that must match any block to be replaced
     * @param blocks The mapping between the Positions and the BlockTypeIds
     *               to be replaced
     */
    public ReplaceBlocks(BlockFilter filter, Map<Position, BlockTypeId> blocks) {
        this.filter = filter;
        this.blocks = blocks;
    }

    /**
     * Get the filter that must match
     * @return The filter
     */
    public BlockFilter getFilter() {
        return filter;
    }

    /**
     * Get the mapping between the Positions and the BlockTypeIds
     * to be replaced
     * @return The Position and BlockTypeId mapping
     */
    public Map<Position, BlockTypeId> getBlocks() {
        return blocks;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        ReplaceBlocks that = (ReplaceBlocks) o;

        if (!filter.equals(that.filter)) return false;
        return blocks.equals(that.blocks);

    }

    @Override
    public int hashCode() {
        int result = filter.hashCode();
        result = 31 * result + blocks.hashCode();
        return result;
    }

    @Override
    public String toString() {
        return "ReplaceBlocks(" +
                "filter=" + filter +
                ", blocks=" + blocks +
                ')';
    }
}
