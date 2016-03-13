package konstructs.api;

public class BlockFilterFactory {

    public static BlockFilterNode EMPTY = new BlockFilterNode(null, null, null, null, null);

    public static BlockFilterNode VACUUM = EMPTY.withBlockTypeId(BlockTypeId.VACUUM);

    public static BlockFilterNode withNamespace(String namespace) {
        return EMPTY.withNamespace(namespace);
    }

    public static BlockFilterNode withName(String name) {
        return EMPTY.withName(name);
    }

    public BlockFilterNode withBlockTypeId(BlockTypeId blockTypeId) {
        return EMPTY.withBlockTypeId(blockTypeId);
    }

    public BlockFilterNode withShape(String shape) {
        return EMPTY.withShape(shape);
    }

    public BlockFilterNode withTransparent(Boolean transparent) {
        return EMPTY.withTransparent(transparent);
    }

    public BlockFilterNode withObstacle(Boolean obstacle) {
        return EMPTY.withObstacle(obstacle);
    }
}
