package konstructs.api.messages;

import konstructs.api.Block;
import konstructs.api.Position;

public class BlockViewed {
    private final Position position;
    private final Block block;

    public BlockViewed(Position position, Block block) {
        this.position = position;
        this.block = block;
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

        BlockViewed that = (BlockViewed) o;

        if (!position.equals(that.position)) return false;
        return block.equals(that.block);

    }

    @Override
    public int hashCode() {
        int result = position.hashCode();
        result = 31 * result + block.hashCode();
        return result;
    }

    @Override
    public String toString() {
        return "BlockViewed(" +
                "position=" + position +
                ", block=" + block +
                ')';
    }
}
