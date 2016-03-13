package konstructs.api.messages;

import konstructs.api.BlockTypeId;
import konstructs.api.Box;
import konstructs.api.Position;

import java.util.Arrays;
import java.util.Map;

public class BoxQueryResult {
    private final Box box;
    private final BlockTypeId blocks[];

    public BoxQueryResult(Box box, BlockTypeId[] blocks) {
        this.box = box;
        this.blocks = blocks;
    }

    public Box getBox() {
        return box;
    }

    public BlockTypeId[] getBlocks() {
        return blocks;
    }

    public BlockTypeId get(Position p) {
        return box.get(p, blocks);
    }

    public BlockTypeId getLocal(Position p) {
        return box.getLocal(p, blocks);
    }

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
