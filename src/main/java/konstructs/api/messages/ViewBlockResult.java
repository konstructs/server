package konstructs.api.messages;

import konstructs.api.Block;
import konstructs.api.Position;

/**
 * ViewBlockResult is a message received in response to ViewBlock message.
 * It contains a copy of the block viewed, leaving the block itself
 * intact in the world.
 */
public class ViewBlockResult {
    private final Position position;
    private final Block block;

    /**
     * Construct an immutable ViewBlockResult message
     * @param position The position of the viewed block
     * @param block A copy of the viewed block
     */
    public ViewBlockResult(Position position, Block block) {
        this.position = position;
        this.block = block;
    }

    /**
     * Get the position of the viewed block
     * @return The position of the viewed block
     */
    public Position getPosition() {
        return position;
    }

    /**
     * Get the copy of the viewed block
     * @return A  copy of the viewed block
     */
    public Block getBlock() {
        return block;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        ViewBlockResult that = (ViewBlockResult) o;

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
        return "ViewBlockResult(" +
                "position=" + position +
                ", block=" + block +
                ')';
    }
}
