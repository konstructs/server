package konstructs.api;

/**
 * BlockFilterNode is a BlockFilter that is used to aggregate a
 * set of properties of a block that needs to be matched. Each
 * property (set using the with* methods) is required to match
 * for the filter as a whole to match. In technical terms it can
 * be said that the BlockFilterNode uses logical AND operator to
 * aggregate the properties to be checked.
 *
 * <p>
 *     Note: The easiest way to create a BlockFilter is to use the
 *     BlockFilterFactory!
 * </p>
 *
 * @see BlockFilterFactory
 */
public class BlockFilterNode extends BlockFilter {
    private final String namespace;
    private final String name;
    private final String shape;
    private final Boolean transparent;
    private final Boolean obstacle;

    /**
     * Constructs an immutable BlockFilterNode. All parameters may be
     * null (i.e. unset).
     * <p>
     *     Note: The easiest way to create a BlockFilter is to use the
     *     BlockFilterFactory!
     * </p>
     *
     * @param namespace The namespace of the BlockTypeId
     * @param name The names of the BlockTypeId
     * @param shape The shape of the BlockType
     * @param transparent Is the BlockType transparent?
     * @param obstacle Is the BlockType an obstacle?
     * @see BlockFilterFactory
     */
    public BlockFilterNode(String namespace, String name, String shape, Boolean transparent, Boolean obstacle) {
        this.namespace = namespace;
        this.name = name;
        this.shape = shape;
        this.transparent = transparent;
        this.obstacle = obstacle;
    }

    /**
     * Create a new BlockFilterNode with the specific namespace set
     * @param namespace The namespace to set
     * @return The new BlockFilterNode with the namespace set
     */
    public BlockFilterNode withNamespace(String namespace) {
        return new BlockFilterNode(namespace, name, shape, transparent, obstacle);
    }

    /**
     * Create a new BlockFilterNode with the specific name set
     * @param name The name to set
     * @return The new BlockFilterNode with name set
     */
    public BlockFilterNode withName(String name) {
        return new BlockFilterNode(namespace, name, shape, transparent, obstacle);
    }

    /**
     * Create a new BlockFilterNode with the specific BlockTypeId set.
     * This method sets both namespace and name.
     * @param blockTypeId The BlockTypeId to set
     * @return The new BlockFilterNode with both the namespace and name set
     */
    public BlockFilterNode withBlockTypeId(BlockTypeId blockTypeId) {
        return withNamespace(blockTypeId.getNamespace())
                .withName(blockTypeId.getName());
    }

    /**
     * Create a new BlockFilterNode with the specific shape set
     * @param shape The shape to set
     * @return The new BlockFilterNode with the shape set
     */
    public BlockFilterNode withShape(String shape) {
        return new BlockFilterNode(namespace, name, shape, transparent, obstacle);
    }

    /**
     * Create a new BlockFilterNode with the transparent property set
     * @param transparent Must the block be transparent?
     * @return The new BlockFilterNode with the transparent property set
     */
    public BlockFilterNode withTransparent(Boolean transparent) {
        return new BlockFilterNode(namespace, name, shape, transparent, obstacle);
    }

    /**
     * Create a new BlockFilterNode with the obstacle property set
     * @param obstacle Must the block be an obstacle?
     * @return The new BlockFilterNode with the obstacle property set
     */
    public BlockFilterNode withObstacle(Boolean obstacle) {
        return new BlockFilterNode(namespace, name, shape, transparent, obstacle);
    }

    @Override
    public boolean matches(BlockTypeId blockTypeId, BlockType blockType) {
        return ((namespace == null || namespace.equals(blockTypeId.getNamespace()))
                && (name == null || name.equals(blockTypeId.getName()))
                && (shape == null || shape.equals(blockType.getShape()))
                && (transparent == null || transparent.equals(blockType.isTransparent()))
                && (obstacle == null || obstacle.equals(blockType.isObstacle())));
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        BlockFilterNode that = (BlockFilterNode) o;

        if (namespace != null ? !namespace.equals(that.namespace) : that.namespace != null) return false;
        if (name != null ? !name.equals(that.name) : that.name != null) return false;
        if (shape != null ? !shape.equals(that.shape) : that.shape != null) return false;
        if (transparent != null ? !transparent.equals(that.transparent) : that.transparent != null) return false;
        return obstacle != null ? obstacle.equals(that.obstacle) : that.obstacle == null;

    }

    @Override
    public int hashCode() {
        int result = namespace != null ? namespace.hashCode() : 0;
        result = 31 * result + (name != null ? name.hashCode() : 0);
        result = 31 * result + (shape != null ? shape.hashCode() : 0);
        result = 31 * result + (transparent != null ? transparent.hashCode() : 0);
        result = 31 * result + (obstacle != null ? obstacle.hashCode() : 0);
        return result;
    }

    @Override
    public String toString() {
        return "BlockFilterNode(" +
                "namespace='" + namespace + '\'' +
                ", name='" + name + '\'' +
                ", shape='" + shape + '\'' +
                ", transparent=" + transparent +
                ", obstacle=" + obstacle +
                ')';
    }
}
