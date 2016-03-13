package konstructs.api.messages;

import konstructs.api.Position;

public class ViewBlock {
    private final Position position;

    public ViewBlock(Position position) {
        this.position = position;
    }

    public Position getPosition() {
        return position;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        ViewBlock viewBlock = (ViewBlock) o;

        return position.equals(viewBlock.position);

    }

    @Override
    public int hashCode() {
        return position.hashCode();
    }

    @Override
    public String toString() {
        return "ViewBlock(" +
                "position=" + position +
                ')';
    }
}
