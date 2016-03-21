package konstructs.api;

/**
 * BlockFilterFactory is a class with static factory methods for quickly
 * and easily creating BlockFilters. A simple example:
 * <pre>
 *     BlockFilterFactory
 *         .withName("grass")
 *         .withObstacle(true)
 *         .or(BlockFilterFactory
 *             .withNamespace("org/konstructs"));
 * </pre>
 * This creates a filter that matches blocks that are named "grass" AND are obstacles
 * OR blocks that have the namespace "org/konstructs".
 */
public class BlockFilterFactory {
    /**
     * The EVERYTHING BlockFilter matches everything.
     */
    public static final BlockFilterNode EVERYTHING = new BlockFilterNode(null, null, null, null, null);

    /**
     * The EMPTY BlockFilter is an alias for the EVERYTHING BlockFilter
     */
    public static final BlockFilterNode EMPTY = EVERYTHING;

    /**
     * The NOTHING BlockFilter matches nothing.
     */
    public static final BlockFilter NOTHING = new BlockFilter() {
            @Override
            public boolean matches(BlockTypeId blockTypeId, BlockType blockType) {
                return false;
            }

            @Override
            public BlockFilter or(BlockFilter filter) {
                return filter;
            }
        };

    /**
     * The VACUUM BlockFilter matches only VACUUM.
     */
    public static BlockFilterNode VACUUM = EMPTY.withBlockTypeId(BlockTypeId.VACUUM);

    /**
     * Factory method to create a BlockFilter with the given namespace
     * @param namespace The namespace to set in the new BlockFilter
     * @return The new BlockFilter with namespace set
     */
    public static BlockFilterNode withNamespace(String namespace) {
        return EMPTY.withNamespace(namespace);
    }

    /**
     * Factory method to create a BlockFilter with the given name
     * @param name The name to set in the new BlockFilter
     * @return The new BlockFilter with name set
     */
    public static BlockFilterNode withName(String name) {
        return EMPTY.withName(name);
    }

    /**
     * Factory method to create a BlockFilter with the given BlockTypeId.
     * This method sets both the namespace and the name.
     * @param blockTypeId The BlockTypeId to set in the new BlockFilter
     * @return The new BlockFilter with both namespace and name set
     */
    public static BlockFilterNode withBlockTypeId(BlockTypeId blockTypeId) {
        return EMPTY.withBlockTypeId(blockTypeId);
    }

    /**
     * Factory method to create a BlockFilter with the given shape
     * @param shape The shape to set in the new BlockFilter
     * @return The new BlockFilter with the shape set
     */
    public static BlockFilterNode withShape(String shape) {
        return EMPTY.withShape(shape);
    }

    /**
     * Factory method to create a BlockFilter with transparent property set
     * @param transparent Must the block be transparent to match?
     * @return The new BlockFilter with the transparent property set
     */
    public static BlockFilterNode withTransparent(Boolean transparent) {
        return EMPTY.withTransparent(transparent);
    }

    /**
     * Factory method to create a new BlockFilter with the obstacle property set
     * @param obstacle Must the block be an obstacle to match?
     * @return The new BlockFilter with the obstacle property set
     */
    public static BlockFilterNode withObstacle(Boolean obstacle) {
        return EMPTY.withObstacle(obstacle);
    }
}
