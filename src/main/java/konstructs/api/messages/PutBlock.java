package konstructs.api.messages;

import konstructs.api.Block;
import konstructs.api.Position;

/**
 * PutBlock is a message that ask the world to put a given block
 * on a given position if the block presently there is VACUUM
 *
 * @see konstructs.api.BlockTypeId#VACUUM
 */
public final class PutBlock {
    private final Position position;
    private final Block block;

    /**
     * Construct an immutable PutBLock
     * @param position The position the block is to be put onto
     * @param block The block to be put
     */
    public PutBlock(Position position, Block block) {
        this.position = position;
        this.block = block;
    }

    /**
     * Get the position onto which the block is to be put
     * @return The position onto whicht the block is to be put
     */
    public Position getPosition() {
        return position;
    }

    /**
     * Get the block to be put
     * @return The block
     */
    public Block getBlock() {
        return block;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        PutBlock putBlock = (PutBlock) o;

        if (!position.equals(putBlock.position)) return false;
        return block.equals(putBlock.block);

    }

    @Override
    public int hashCode() {
        int result = position.hashCode();
        result = 31 * result + block.hashCode();
        return result;
    }

    @Override
    public String toString() {
        return "PutBlock(" +
                "position=" + position +
                ", block=" + block +
                ')';
    }
}
