package konstructs.api.messages;

import konstructs.api.Block;
import konstructs.api.Position;

public class ReplaceBlockResult {
    private final Position position;
    private final Block block;
    private final boolean successful;

    public ReplaceBlockResult(Position position, Block block, boolean successful) {
        this.position = position;
        this.block = block;
        this.successful = successful;
    }

    public Position getPosition() {
        return position;
    }

    public Block getBlock() {
        return block;
    }

    public boolean isSuccessful() {
        return successful;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        ReplaceBlockResult that = (ReplaceBlockResult) o;

        if (successful != that.successful) return false;
        if (!position.equals(that.position)) return false;
        return block.equals(that.block);

    }

    @Override
    public int hashCode() {
        int result = position.hashCode();
        result = 31 * result + block.hashCode();
        result = 31 * result + (successful ? 1 : 0);
        return result;
    }

    @Override
    public String toString() {
        return "ReplaceBlockResult(" +
                "position=" + position +
                ", block=" + block +
                ", successful=" + successful +
                ')';
    }
}
