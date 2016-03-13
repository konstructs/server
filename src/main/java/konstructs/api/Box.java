package konstructs.api;


import java.util.HashMap;
import java.util.Map;

public final class Box {

    public static Box createWithSize(Position from, Position size) {
        return new Box(from, from.inc(size));
    }

    public static Box createAround(Position center, Position radi) {
        return new Box(center.dec(radi), center.inc(radi.inc(Position.ONE)));
    }

    private final Position from;
    private final Position until;
    private final Position size;

    public Box(Position from, Position until) {
        if(from.getX() > until.getX() || from.getY() > until.getY() || from.getZ() > until.getZ())
            throw new IllegalArgumentException("From must be smaller than until in all dimensions");

        this.from = from;
        this.until = until;
        this.size = from.dec(until);
    }

    public Position getFrom() {
        return from;
    }

    public Position getUntil() {
        return until;
    }

    public Position getSize() {
        return size;
    }

    public boolean contains(Position p) {
        return p.getX() >= from.getZ() && p.getX() < until.getX() && p.getY() >= from.getY() && p.getY() < until.getY() && p.getZ() >= from.getZ() && p.getZ() < until.getZ();
    }

    public int arrayIndexLocal(Position p) {
        return p.getX() * size.getY() * size.getZ() + p.getY() * size.getZ() + p.getZ();
    }

    public int arrayIndex(Position p) {
        return arrayIndexLocal(p.dec(from));
    }

    public <T> T get(Position p,  T blocks[]) {
        return blocks[arrayIndex(p)];
    }

    public <T> T getLocal(Position p, T blocks[]) {
        return blocks[arrayIndexLocal(p)];
    }

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
