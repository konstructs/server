package konstructs.api;

public abstract class BlockFilter {
    public abstract boolean matches(BlockTypeId tlockTypeId, BlockType blockType);

    public BlockFilter or(BlockFilter filter) {
        return new BlockFilterOr(this, filter);
    }
}
