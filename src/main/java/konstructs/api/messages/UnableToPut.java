package konstructs.api.messages;

import konstructs.api.Block;
import konstructs.api.Position;

/**
 * UnableToPut is a message received as a response to a PutBlock message
 * when the block present at the position was not VACUUM. This message
 * returns the block that was to be put onto that position to the sender.
 * @see PutBlock
 * @see konstructs.api.BlockTypeId#VACUUM
 */
public class UnableToPut {
    private final Position position;
    private final Block block;

    /**
     * Constructs and immutable UnableToPut message
     * @param position The position onto which it was not possible to put the block
     * @param block The block itself
     */
    public UnableToPut(Position position, Block block) {
        this.position = position;
        this.block = block;
    }

    /**
     * Get the position onto which it was not possible to put the block
     * @return The position
     */
    public Position getPosition() {
        return position;
    }

    /**
     * Get the block itself
     * @return The block
     */
    public Block getBlock() {
        return block;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        UnableToPut that = (UnableToPut) o;

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
        return "UnableToPut(" +
                "position=" + position +
                ", block=" + block +
                ')';
    }
}
