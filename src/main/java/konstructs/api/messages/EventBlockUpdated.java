package konstructs.api.messages;

import konstructs.api.BlockTypeId;
import konstructs.api.Position;

import java.util.Map;

public class EventBlockUpdated {
    private final Map<Position, BlockTypeId> blocks;

    public EventBlockUpdated(Map<Position, BlockTypeId> blocks) {
        this.blocks = blocks;
    }

    public Map<Position, BlockTypeId> getBlocks() {
        return blocks;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        EventBlockUpdated that = (EventBlockUpdated) o;

        return blocks.equals(that.blocks);

    }

    @Override
    public int hashCode() {
        return blocks.hashCode();
    }

    @Override
    public String toString() {
        return "EventBlockUpdated(" +
                "blocks=" + blocks +
                ')';
    }
}
