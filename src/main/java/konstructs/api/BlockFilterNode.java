package konstructs.api;

public class BlockFilterNode extends BlockFilter {
    private final String namespace;
    private final String name;
    private final String shape;
    private final Boolean transparent;
    private final Boolean obstacle;

    public BlockFilterNode(String namespace, String name, String shape, Boolean transparent, Boolean obstacle) {
        this.namespace = namespace;
        this.name = name;
        this.shape = shape;
        this.transparent = transparent;
        this.obstacle = obstacle;
    }

    public BlockFilterNode withNamespace(String namespace) {
        return new BlockFilterNode(namespace, name, shape, transparent, obstacle);
    }

    public BlockFilterNode withName(String name) {
        return new BlockFilterNode(namespace, name, shape, transparent, obstacle);
    }

    public BlockFilterNode withBlockTypeId(BlockTypeId blockTypeId) {
        return withNamespace(blockTypeId.getNamespace())
                .withName(blockTypeId.getName());
    }

    public BlockFilterNode withShape(String shape) {
        return new BlockFilterNode(namespace, name, shape, transparent, obstacle);
    }

    public BlockFilterNode withTransparent(Boolean transparent) {
        return new BlockFilterNode(namespace, name, shape, transparent, obstacle);
    }

    public BlockFilterNode withObstacle(Boolean obstacle) {
        return new BlockFilterNode(namespace, name, shape, transparent, obstacle);
    }

    @Override
    public boolean matches(BlockTypeId blockTypeId, BlockType blockType) {
        return ((namespace == null || namespace == blockTypeId.getNamespace())
                && (name == null || name == blockTypeId.getName())
                && (shape == null || shape == blockType.getShape()) &&
                (transparent == null || transparent == blockType.isTransparent()) &&
                (obstacle == null || obstacle == blockType.isObstacle()));
    }
}
