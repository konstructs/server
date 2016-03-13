package konstructs.api;

public final class Position {
    public final static Position ONE = new Position(1, 1, 1);
    private final int x;
    private final int y;
    private final int z;

    public Position(int x, int y, int z) {
        this.x = x;
        this.y = y;
        this.z = z;
    }

    public Position inc(Position add) {
        return new Position(x + add.getX(), y + add.getY(), z + add.getZ());
    }

    public Position dec(Position subtract) {
        return new Position(x - subtract.getX(), y - subtract.getY(), z - subtract.getZ());
    }

    public Position incX(int i) {
        return new Position(x + i, y, z);
    }

    public Position incY(int i) {
        return new Position(x, y + i, z);
    }

    public Position incZ(int i) {
        return new Position(x, y, z + i);
    }

    public Position decX(int i) {
        return new Position(x - i, y, z);
    }

    public Position decY(int i) {
        return new Position(x, y - i, z);
    }

    public Position decZ(int i) {
        return new Position(x, y, z - i);
    }

    public Position withX(int i) {
        return new Position(i, y, z);
    }

    public Position withY(int i) {
        return new Position(x, i, z);
    }

    public Position withZ(int i) {
        return new Position(x, y, i);
    }

    public int getX() {
        return x;
    }

    public int getY() {
        return y;
    }

    public int getZ() {
        return z;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Position position = (Position) o;

        if (x != position.x) return false;
        if (y != position.y) return false;
        return z == position.z;

    }

    @Override
    public int hashCode() {
        int result = x;
        result = 31 * result + y;
        result = 31 * result + z;
        return result;
    }

    @Override
    public String toString() {
        return "Position(" +
                "x=" + x +
                ", y=" + y +
                ", z=" + z +
                ')';
    }
}
