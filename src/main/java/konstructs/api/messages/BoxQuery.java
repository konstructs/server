package konstructs.api.messages;

import konstructs.api.Box;

/**
 * BoxQuery is a message to query the world for a rectangular volume of
 * blocks. The volume is defined by the Box class. Please see the documentation
 * of the Box class for details on how it works.
 * @see Box
 */
public class BoxQuery {
    private final Box box;

    /**
     * Construct an immutable BoxQuery
     * @param box The box queried for
     */
    public BoxQuery(Box box) {
        this.box = box;
    }

    /**
     * Get the box queried for
     * @return The box queried for
     */
    public Box getBox() {
        return box;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        BoxQuery boxQuery = (BoxQuery) o;

        return box.equals(boxQuery.box);

    }

    @Override
    public int hashCode() {
        return box.hashCode();
    }

    @Override
    public String toString() {
        return "BoxQuery(" +
                "box=" + box +
                ')';
    }
}
