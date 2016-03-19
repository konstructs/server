package konstructs.api.messages;

import konstructs.api.Block;
import konstructs.api.Position;

/**
 * BlockRemoved is a message received in response to a RemoveBlock
 * message, It contains the block removed.
 */
public class BlockRemoved {
    private final Position position;
    private final Block block;

    /**
     * Construct an immutable BlockRemoved message
     * @param position The position from which the block was removed
     * @param block The block that was removed
     */
    public BlockRemoved(Position position, Block block) {
        this.position = position;
        this.block = block;
    }

    /**
     * Get the position from which the block was removed
     * @return The position from which the block was removed
     */
    public Position getPosition() {
        return position;
    }

    /**
     * Get the block that was removed
     * @return The block that was removed
     */
    public Block getBlock() {
        return block;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        BlockRemoved that = (BlockRemoved) o;

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
        return "BlockRemoved(" +
                "position=" + position +
                ", block=" + block +
                ')';
    }
}
