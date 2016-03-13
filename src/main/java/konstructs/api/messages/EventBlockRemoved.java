package konstructs.api.messages;

import konstructs.api.Position;

import java.util.Set;

public class EventBlockRemoved {
    private final Set<Position> positions;

    public EventBlockRemoved(Set<Position> positions) {
        this.positions = positions;
    }

    public Set<Position> getPositions() {
        return positions;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        EventBlockRemoved that = (EventBlockRemoved) o;

        return positions.equals(that.positions);

    }

    @Override
    public int hashCode() {
        return positions.hashCode();
    }

    @Override
    public String toString() {
        return "EventBlockRemoved(" +
                "positions=" + positions +
                ')';
    }
}
