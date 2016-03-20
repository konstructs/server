package konstructs.api.messages;

import konstructs.api.Position;

/**
 * ViewBlock is a message that ask to view a block. This message
 * does not change the world, but rather returns a copy of the block.
 * This can be useful to inspect the world, even though there is of
 * course no guarantee that the block returned is still present when
 * the ViewBlockResult message is received.
 *
 * @see ViewBlockResult
 */
public class ViewBlock {
    private final Position position;

    /**
     * Constructs an immutable ViewBlock message
     * @param position The position to be viewed
     */
    public ViewBlock(Position position) {
        this.position = position;
    }

    /**
     * Get the position to be viewed
     * @return The position to be viewed
     */
    public Position getPosition() {
        return position;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        ViewBlock viewBlock = (ViewBlock) o;

        return position.equals(viewBlock.position);

    }

    @Override
    public int hashCode() {
        return position.hashCode();
    }

    @Override
    public String toString() {
        return "ViewBlock(" +
                "position=" + position +
                ')';
    }
}
