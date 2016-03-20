package konstructs.api;

/**
 * BlockFilterOr is a class that represents a logical OR
 * between two BlockFilters. It is created via the or method
 * of the BlockFilter base class.
 *
 * @see BlockFilter#or(BlockFilter)
 */
public class BlockFilterOr extends BlockFilter {
    private final BlockFilter f1;
    private final BlockFilter f2;

    /**
     * Constructs an immutable BlockFilterOr
     * <p>
     *     Note: You rarely need to create an instance of the class,
     *     but it is rather usually created via the BlockFilter or
     *     method.
     * </p>
     * @param f1 The first filter
     * @param f2 The second filter
     * @see BlockFilter#or(BlockFilter)
     */
    public BlockFilterOr(BlockFilter f1, BlockFilter f2) {
        super();
        this.f1 = f1;
        this.f2 = f2;
    }

    @Override
    public boolean matches(BlockTypeId blockTypeId, BlockType blockType) {
        return f1.matches(blockTypeId, blockType) || f2.matches(blockTypeId, blockType);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        BlockFilterOr that = (BlockFilterOr) o;

        if (!f1.equals(that.f1)) return false;
        return f2.equals(that.f2);

    }

    @Override
    public int hashCode() {
        int result = f1.hashCode();
        result = 31 * result + f2.hashCode();
        return result;
    }

    @Override
    public String toString() {
        return "BlockFilterOr(" +
                "f1=" + f1 +
                ", f2=" + f2 +
                ')';
    }
}
