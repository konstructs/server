package konstructs.api.messages;

import konstructs.api.Position;

public class DiscardBlock {
    private final Position position;

    public DiscardBlock(Position position) {
        this.position = position;
    }

    public Position getPosition() {
        return position;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        DiscardBlock that = (DiscardBlock) o;

        return position.equals(that.position);

    }

    @Override
    public int hashCode() {
        return position.hashCode();
    }

    @Override
    public String toString() {
        return "DiscardBlock(" +
                "position=" + position +
                ')';
    }
}
