package konstructs.api.messages;

import konstructs.api.Position;

/**
 * RemoveBlock is a message that is sent to remove a block on a
 * given position. Removing a block is equal to replacing it with
 * a VACUUM block.
 *
 * @see konstructs.api.BlockTypeId#VACUUM
 */
public class RemoveBlock {
    private final Position position;

    /**
     * Construct an immutable RemoveBlock message
     * @param position The position where the block should be removed
     */
    public RemoveBlock(Position position) {
        this.position = position;
    }

    /**
     * Get the position to remove
     * @return The position to remove
     */
    public Position getPosition() {
        return position;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        RemoveBlock that = (RemoveBlock) o;

        return position.equals(that.position);

    }

    @Override
    public int hashCode() {
        return position.hashCode();
    }

    @Override
    public String toString() {
        return "RemoveBlock(" +
                "position=" + position +
                ')';
    }
}
