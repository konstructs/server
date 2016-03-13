package konstructs.api.messages;

import konstructs.api.Position;

public class RemoveBlock {
    private final Position position;

    public RemoveBlock(Position position) {
        this.position = position;
    }

    public Position getPosition() {
        return position;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        RemoveBlock that = (RemoveBlock) o;

        return position.equals(that.position);

    }

    @Override
    public int hashCode() {
        return position.hashCode();
    }

    @Override
    public String toString() {
        return "RemoveBlock(" +
                "position=" + position +
                ')';
    }
}
