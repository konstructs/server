package konstructs.api;

/**
 * BlockFilter is an abstract base class for constructing BlockFilters.
 */
public abstract class BlockFilter {
    /**
     * Check if this filter matches the given BlockTypeId and BlockType.
     * @param blockTypeId The block type id to match with
     * @param blockType The block type to match with
     * @return True if this filter matches the given parameters
     */
    public abstract boolean matches(BlockTypeId blockTypeId, BlockType blockType);

    /**
     * Create a new block filter that matches if either this block filter
     * or the block filter given to this method matches the block.
     * @param filter The filter that is or'ed with this filter
     * @return A new filter which contains both this filter and the filter given
     */
    public BlockFilter or(BlockFilter filter) {
        return new BlockFilterOr(this, filter);
    }
}
