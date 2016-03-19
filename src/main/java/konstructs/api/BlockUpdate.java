package konstructs.api;

/**
 * Created by petter on 2016-03-19.
 */
public class BlockUpdate {
    private final Block from;
    private final Block to;

    public BlockUpdate(Block from, Block to) {
        this.from = from;
        this.to = to;
    }

    public Block getFrom() {
        return from;
    }

    public Block getTo() {
        return to;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        BlockUpdate that = (BlockUpdate) o;

        if (!from.equals(that.from)) return false;
        return to.equals(that.to);

    }

    @Override
    public int hashCode() {
        int result = from.hashCode();
        result = 31 * result + to.hashCode();
        return result;
    }

    @Override
    public String toString() {
        return "BlockUpdate(" +
                "from=" + from +
                ", to=" + to +
                ')';
    }
}
