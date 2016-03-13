package konstructs.api;

import java.util.UUID;

/**
 *
 */
public final class Block {
    private final UUID id;
    private final BlockTypeId type;

    public static Block createWithId(BlockTypeId t) {
        return new Block(UUID.randomUUID(), t);
    }

    public static Block createWithId(String t) {
        return createWithId(BlockTypeId.fromString(t));
    }

    public static Block create(BlockTypeId t) {
        return new Block(null, t);
    }

    public static Block create(String t) {
        return create(BlockTypeId.fromString(t));
    }

    public Block(UUID id, BlockTypeId type) {
        this.id = id;
        this.type = type;
    }

    public UUID getId() {
        return id;
    }

    public BlockTypeId getType() {
        return type;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Block block = (Block) o;

        if (id != null ? !id.equals(block.id) : block.id != null) return false;
        return type.equals(block.type);

    }

    @Override
    public int hashCode() {
        int result = id != null ? id.hashCode() : 0;
        result = 31 * result + type.hashCode();
        return result;
    }

    @Override
    public String toString() {
        return "Block(" +
                "id=" + id +
                ", type=" + type +
                ')';
    }
}
