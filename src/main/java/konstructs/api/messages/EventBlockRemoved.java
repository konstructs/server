package konstructs.api.messages;

import konstructs.api.Position;

import java.util.Set;

/**
 * EvenBlockRemoved is a message received when a block is removed from the
 * world. This message is received for any block removed regardless of the
 * way it is removed. (Not yet true for all cases).
 */
public class EventBlockRemoved {
    private final Set<Position> positions;

    /**
     * Construct an immutable EventBlockRemoved message
     * @param positions The positions that was removed
     */
    public EventBlockRemoved(Set<Position> positions) {
        this.positions = positions;
    }

    /**
     * Get the set  of  Position that was removed
     * @return The set of positions removed
     */
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
