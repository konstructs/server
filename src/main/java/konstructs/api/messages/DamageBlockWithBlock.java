package konstructs.api.messages;

import konstructs.api.Block;
import konstructs.api.Position;

/**
 * DamageBlockWithBlock is a message that is used to damage a block with another block.
 * This means that it will damage the health of both blocks with the other blocks damage value.
 * @see konstructs.api.Health
 */
public class DamageBlockWithBlock {
    private final Position toDamage;
    private final Block using;

    /**
     * Create a new immutable DamageBlockWithBlock instance
     * @param toDamage The position in the world to damage
     * @param using The block to be used to damage the position
     */
    public DamageBlockWithBlock(Position toDamage, Block using) {
        this.toDamage = toDamage;
        this.using = using;
    }

    /**
     * Returns the position to damage
     * @return the position to damage
     */
    public Position getToDamage() {
        return toDamage;
    }

    /**
     * Returns the block to damage the position with
     * @return Block to deal damage with
     */
    public Block getUsing() {
        return using;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        DamageBlockWithBlock that = (DamageBlockWithBlock) o;

        if (!toDamage.equals(that.toDamage)) return false;
        return using.equals(that.using);

    }

    @Override
    public int hashCode() {
        int result = toDamage.hashCode();
        result = 31 * result + using.hashCode();
        return result;
    }

    @Override
    public String toString() {
        return "DamageBlockWithBlock(" +
                "toDamage=" + toDamage +
                ", using=" + using +
                ')';
    }
}
