package konstructs.api.messages;

import konstructs.api.Block;
import konstructs.api.BlockFilter;
import konstructs.api.Position;

public final class ReplaceBlock {
    private final BlockFilter filter;
    private final Position position;
    private final Block block;

    public ReplaceBlock(BlockFilter filter, Position position, Block block) {
        this.filter = filter;
        this.position = position;
        this.block = block;
    }

    public BlockFilter getFilter() {
        return filter;
    }

    public Position getPosition() {
        return position;
    }

    public Block getBlock() {
        return block;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        ReplaceBlock that = (ReplaceBlock) o;

        if (!filter.equals(that.filter)) return false;
        if (!position.equals(that.position)) return false;
        return block.equals(that.block);

    }

    @Override
    public int hashCode() {
        int result = filter.hashCode();
        result = 31 * result + position.hashCode();
        result = 31 * result + block.hashCode();
        return result;
    }
}
