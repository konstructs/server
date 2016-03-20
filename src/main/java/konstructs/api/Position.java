package konstructs.api;

/**
 * A Position is a class representing a position in the block space,
 * i.e. the position of a block. In mathematical terms, it is a
 * three dimensional vector.
 *
 * In the API there is a difference between a global and a local Position.
 * A global Position is assumed to be relative to the 0,0,0 Position in the
 * given world. A local position is always relative to another global Position.
 *
 * Since the block space is a discrete space, the position only handles integers.
 */
public final class Position {
    public final static Position ONE = new Position(1, 1, 1);
    private final int x;
    private final int y;
    private final int z;

    /**
     * Constructs an immutable Position
     * @param x The position in the x dimension (left, right)
     * @param y The position in the y dimension (up, down)
     * @param z The position in the z dimension (forward, backward)
     */
    public Position(int x, int y, int z) {
        this.x = x;
        this.y = y;
        this.z = z;
    }

    /**
     * Add to this position all dimensions of another position
     * and return a new position
     * @param position The position to be added to this position
     * @return A new position that is the sum of this position and the given position
     */
    public Position add(Position position) {
        return new Position(x + position.getX(), y + position.getY(), z + position.getZ());
    }

    /**
     * Subtract from this position all dimensions of another position
     * and return a new position
     * @param position The position to be subtracted from this position
     * @return A new position that is the difference between this position and the position given
     */
    public Position subtract(Position position) {
        return new Position(x - position.getX(), y - position.getY(), z - position.getZ());
    }

    /**
     * Add the given value to this position's x dimension
     * and return a new position
     * @param i The value to add
     * @return A new position with an x dimension that is the sum of
     *         this position's x dimension and the value given
     */
    public Position addX(int i) {
        return new Position(x + i, y, z);
    }

    /**
     * Add the given value to this position's y dimension
     * and return a new position
     * @param i The value to add
     * @return A new position with an y dimension that is the sum of
     *         this position's y dimension and the value given
     */
    public Position addY(int i) {
        return new Position(x, y + i, z);
    }

    /**
     * Add the given value to this position's z dimension
     * and return a new position
     * @param i The value to add
     * @return A new position with a z dimension that is the sum of
     *         this position's z dimension and the value given
     */
    public Position addZ(int i) {
        return new Position(x, y, z + i);
    }

    /**
     * Subtract the given value from this position's x dimension
     * * and return a new position
     * @param i The value to subtract
     * @return A new position with an x dimension that is the difference
     *         between this position's x dimension and the value given
     */
    public Position subtractX(int i) {
        return new Position(x - i, y, z);
    }

    /**
     * Subtract the given value from this position's y dimension
     * and return a new position
     * @param i The value to subtract
     * @return A new position with an y dimension that is the difference
     *         between this position's y dimension and the value given
     */
    public Position subtractY(int i) {
        return new Position(x, y - i, z);
    }

    /**
     * Subtract the given value from this position's z dimension
     * and return a new position
     * @param i The value to subtract
     * @return A new position with a z dimension that is the difference
     *         between this position's z dimension and the value given
     */
    public Position subtractZ(int i) {
        return new Position(x, y, z - i);
    }

    /**
     * Return a new position with the x dimension set to the given value
     * @param i The value to which x should be set
     * @return A new position with the x dimension set to the given value
     */
    public Position withX(int i) {
        return new Position(i, y, z);
    }

    /**
     * Return a new position with the y dimension set to the given value
     * @param i The value to which y should be set
     * @return A new position with the y dimension set to the given value
     */
    public Position withY(int i) {
        return new Position(x, i, z);
    }

    /**
     * Return a new position with the z dimension set to the given value
     * @param i The value to which z should be set
     * @return A new position with the z dimension set to the given value
     */
    public Position withZ(int i) {
        return new Position(x, y, i);
    }

    /**
     * Get the x dimension of this position
     * @return The x dimension of this position
     */
    public int getX() {
        return x;
    }

    /**
     * Get the y dimension of this position
     * @return The y dimension of this position
     */
    public int getY() {
        return y;
    }

    /**
     * Get the z dimension of this position
     * @return The z dimension of this position
     */
    public int getZ() {
        return z;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Position position = (Position) o;

        if (x != position.x) return false;
        if (y != position.y) return false;
        return z == position.z;

    }

    @Override
    public int hashCode() {
        int result = x;
        result = 31 * result + y;
        result = 31 * result + z;
        return result;
    }

    @Override
    public String toString() {
        return "Position(" +
                "x=" + x +
                ", y=" + y +
                ", z=" + z +
                ')';
    }
}
