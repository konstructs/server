package konstructs.api.messages;

import konstructs.api.BlockTypeId;
import konstructs.api.Box;
import konstructs.api.Position;

import java.util.Arrays;
import java.util.Map;

/**
 * BoxQueryResult is a message received as a response to the BoxQuery
 * message. It contains the Box of the original query as well as an
 * array of blocks that can be inspected using the Box.
 *
 * This  class also contains a small number of helper methods that can
 * be used to inspect the result (internally using the Box class).
 *
 * @see Box
 */
public class BoxQueryResult {
    private final Box box;
    private final BlockTypeId blocks[];

    /**
     * Construct an immutable BoxQueryResult
     * @param box The box from the query
     * @param blocks The array of  blocks that was in the query
     */
    public BoxQueryResult(Box box, BlockTypeId[] blocks) {
        this.box = box;
        this.blocks = blocks;
    }

    /**
     * Get the box of the original query
     * @return The box from the original query
     */
    public Box getBox() {
        return box;
    }

    /**
     * Get the blocks matched by the query (i.e. inside the Box)
     * @return The blocks in the box
     */
    public BlockTypeId[] getBlocks() {
        return blocks;
    }

    /**
     * Get a block on the specified global position. The position
     * must be inside the Box of the original query.
     * @param p The position to be returned
     * @return The BlockTypeId found on the position
     * @see Box
     */
    public BlockTypeId get(Position p) {
        return box.get(p, blocks);
    }

    /**
     * Get a block on the specified local position. The position
     * is local to the from position of the box of the original
     * query.
     * @param p The local position to be returned
     * @return The BlockTypeId found on the position
     */
    public BlockTypeId getLocal(Position p) {
        return box.getLocal(p, blocks);
    }

    /**
     * Get a mapping between Position and BlockTypeId for all
     * blocks matched by the original query. The positions are
     * global positions.
     * @return A mapping between (global) Position and BlockTypeId
     */
    public Map<Position, BlockTypeId> getAsMap() {
        return box.getAsMap(blocks);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        BoxQueryResult that = (BoxQueryResult) o;

        if (!box.equals(that.box)) return false;

        return Arrays.equals(blocks, that.blocks);

    }

    @Override
    public int hashCode() {
        int result = box.hashCode();
        result = 31 * result + Arrays.hashCode(blocks);
        return result;
    }

    @Override
    public String toString() {
        return "BoxQueryResult(" +
                "box=" + box +
                ", blocks=" + Arrays.toString(blocks) +
                ')';
    }
}
