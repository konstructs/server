package konstructs.api;

/**
 * Created by petter on 2016-03-19.
 */
public class BlockUpdate {
    private final Block before;
    private final Block after;

    public BlockUpdate(Block before, Block after) {
        this.before = before;
        this.after = after;
    }

    public Block getBefore() {
        return before;
    }

    public Block getAfter() {
        return after;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        BlockUpdate that = (BlockUpdate) o;

        if (!before.equals(that.before)) return false;
        return after.equals(that.after);

    }

    @Override
    public int hashCode() {
        int result = before.hashCode();
        result = 31 * result + after.hashCode();
        return result;
    }

    @Override
    public String toString() {
        return "BlockUpdate(" +
                "before=" + before +
                ", after=" + after +
                ')';
    }
}
