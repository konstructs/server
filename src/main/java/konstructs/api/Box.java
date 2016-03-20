package konstructs.api;


import java.util.HashMap;
import java.util.Map;

/**
 * Box is a class that represents a 3 dimensional bounding box. This
 * bounding box is used to select a set of blocks in the world. It is
 * defined by two corners, the from corner and the until corner. Therefore,
 * the block on the from position is included in the box, while the block
 * on the until position is excluded from the box. A two dimensional example
 * of what this means:
 * <p>
 *     From is (1, 1) and until is (3,3). Positions marked with "*" are included
 *     in the box.
 * </p>
 * <pre>
 *     (0,3)   (1,3)   (2,3)   (3,3)
 *     (0,2)  *(1,2)* *(2,2)*  (3,2)
 *     (0,1)  *(1,1)* *(2,1)*  (3,1)
 *     (0,0)   (1,0)   (2,0)   (3,0)
 * </pre>
 * <p>
 *     To make it easier to create a Box there are a couple of factory methods like
 *     createWithSize and createAround.
 * </p>
 * <p>
 *     The Box also provides methods to index a one dimensional array that contains
 *     world data withing the box. In the example above, the box bounds around 4
 *     blocks, (1,1), (1,2), (2,1), (2,2). If we want to access data from an array
 *     that contains these 4 values we can use the methods arrayIndex and arrayIndexLocal
 *     to get the indexes for the array. Consider the following 2 dimensional example
 *     (box is still the example from above):
 * </p>
 * <pre>
 *     BlockTypeId[] blocks = {
 *         new BlockTypeId("org/constructs", "stone"),
 *         new BlockTypeId("org/constructs", "stone"),
 *         new BlockTypeId("org/constructs", "stone"),
 *         new BlockTypeId("org/constructs", "stone")
 *         };
 *     // This returns the data of the block at global position 1,1
 *     blocks[box.arrayIndex(new Position(1,1));
 *
 *     // This returns the data of the block at global position 2,2
 *     // As you can see, the arrayIndexLocal uses a position relative to
 *     // from
 *     blocks[box.arrayIndexLocal(new Position(1,1));
 * </pre>
 * <p>
 *     To reduce the boilerplate even further, there are two more helper methods,
 *     get and getLocal, by taking the array as an argument the provide a simple way
 *     to directly get data from an array using a local or global position.
 * </p>
 * @see #createAround(Position, Position)
 * @see #createWithSize(Position, Position)
 * @see #get(Position, Object[])
 * @see #getLocal(Position, Object[])
 */
public final class Box {
    /**
     * Factory method for creating a Box with a given size.
     * @param from The from corner of the Box
     * @param size The size of the Box (the until corner is the size added to from)
     * @return The new Box bounding the area starting at from with size size
     */
    public static Box createWithSize(Position from, Position size) {
        return new Box(from, from.add(size));
    }

    /**
     * Factory method for creating a Box around a central block
     * @param center The block in the center of the box
     * @param radi The number of blocks the box extends in each dimension
     * @return The new Box bounding the are around the center with radi
     *         number of blocks in both directions in each dimension
     */
    public static Box createAround(Position center, Position radi) {
        return new Box(center.subtract(radi), center.add(radi.add(Position.ONE)));
    }

    private final Position from;
    private final Position until;
    private final Position size;

    /**
     * Constructs an immutable Box.
     * <p>
     *     Note: There are a couple of factory methods as well like
     *     createWithSize and createAround.
     * </p>
     * <p>
     *     Note: From must be smaller than until in all dimensions.
     * </p>
     * @param from The starting corner of the bounding box, inclusive
     * @param until The end corner of the bounding box, exclusive
     * @see #createAround(Position, Position)
     * @see #createWithSize(Position, Position)
     */
    public Box(Position from, Position until) {
        if(from.getX() > until.getX() || from.getY() > until.getY() || from.getZ() > until.getZ())
            throw new IllegalArgumentException("From must be smaller than until in all dimensions");

        this.from = from;
        this.until = until;
        this.size = until.subtract(from);
    }

    /**
     * Get the from corner
     * @return The from corner
     */
    public Position getFrom() {
        return from;
    }

    /**
     * Get the until corner
     * @return The until corner
     */
    public Position getUntil() {
        return until;
    }

    /**
     * Get the distance between the from and until corner in
     * all dimensions.
     * @return The distance (or size) of the box in all dimensions
     */
    public Position getSize() {
        return size;
    }

    /**
     * Returns true if the position is within this bounding box
     * @param p The position to be checked
     * @return True if the position is withing this bounding box
     */
    public boolean contains(Position p) {
        return p.getX() >= from.getZ() && p.getX() < until.getX() && p.getY() >= from.getY() && p.getY() < until.getY() && p.getZ() >= from.getZ() && p.getZ() < until.getZ();
    }

    /**
     * Return the array index for a given local position. A local
     * position us a position that is relative to the from corner.
     * @param p A Position that is relative to from
     * @return The index in a data array of local position p
     */
    public int arrayIndexLocal(Position p) {
        return p.getX() * size.getY() * size.getZ() + p.getY() * size.getZ() + p.getZ();
    }

    /**
     * Return the array index for a given global position.
     * @param p A Position that is withing the bounds of this box
     * @return The index in a data array of position p
     */
    public int arrayIndex(Position p) {
        return arrayIndexLocal(p.subtract(from));
    }

    /**
     * Return an element of blocks based on the position p
     * @param p A Position that is withing the bounds of this box
     * @param blocks An array of data of the size of this box
     * @param <T> The type of the array data, e.g. BlockTypeId
     * @return The data element that was found on position p
     */
    public <T> T get(Position p,  T blocks[]) {
        return blocks[arrayIndex(p)];
    }

    /**
     * Return an element of blocks based on the local position p
     * @param p A Position that is relative to the from corner of this box
     * @param blocks An array of data of the size of this box
     * @param <T> The type of the array data, e.g. BlockTypeId
     * @return The data element that was found on position p
     */
    public <T> T getLocal(Position p, T blocks[]) {
        return blocks[arrayIndexLocal(p)];
    }

    /**
     * Returns a mapping from (global) Positions to T (e.g. BlockTypeId)
     * @param blocks An array of data of the size of this box
     * @param <T> The type of the array data, e.g. BlockTypeId
     * @return A mapping from Position to T
     */
    public <T> Map<Position, T> getAsMap(T blocks[]) {
        Map<Position, T> map = new HashMap<>();
        for(int x = from.getX(); x < until.getX(); x++) {
            for(int y = from.getY(); y < until.getY(); y++) {
                for(int z = from.getZ(); z < until.getZ(); z++) {
                    Position p = new Position(x, y, z);
                    T v = get(p, blocks);
                    map.put(p, v);
                }
            }
        }
        return map;
    }

    /**
     * The number of blocks bound by this box. This is equal to the size (length)
     * of an array with data for this box.
     * @return The number of blocks bound by this box
     */
    public int getNumberOfBlocks() {
        return size.getX() * size.getY() * size.getZ();
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Box box = (Box) o;

        if (!from.equals(box.from)) return false;
        return until.equals(box.until);

    }

    @Override
    public int hashCode() {
        int result = from.hashCode();
        result = 31 * result + until.hashCode();
        return result;
    }

    @Override
    public String toString() {
        return "Box(" +
                "from=" + from +
                ", until=" + until +
                ')';
    }

}
