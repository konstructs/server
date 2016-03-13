package konstructs.api.messages;

import konstructs.api.Box;

public class BoxQuery {
    private final Box box;

    public BoxQuery(Box box) {
        this.box = box;
    }

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
