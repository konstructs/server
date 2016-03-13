package konstructs.api.messages;

import konstructs.api.BlockFilter;
import konstructs.api.BlockTypeId;
import konstructs.api.Position;

import java.util.Map;

public class ReplaceBlocks {
    private final BlockFilter filter;
    private final Map<Position, BlockTypeId> blocks;

    public ReplaceBlocks(BlockFilter filter, Map<Position, BlockTypeId> blocks) {
        this.filter = filter;
        this.blocks = blocks;
    }

    public BlockFilter getFilter() {
        return filter;
    }

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
