package konstructs.api;

public final class InventoryView {
    private final int columnOffset;
    private final int rowOffset;
    private final int columns;
    private final int rows;

    public InventoryView(int rowOffset, int columnOffset, int rows, int columns) {
        this.columnOffset = columnOffset;
        this.rowOffset = rowOffset;
        this.columns = columns;
        this.rows = rows;
    }

    public int getColumnOffset() {
        return columnOffset;
    }

    public int getRowOffset() {
        return rowOffset;
    }

    public int getColumns() {
        return columns;
    }

    public int getRows() {
        return rows;
    }

    public int translate(int pos) {
        int r = pos / View.COLUMNS;
        int c = pos % View.COLUMNS;
        int row = r - rowOffset;
        int column = c - columnOffset;
        return row * columns + column;
    }

    public boolean contains(int pos) {
        int row = pos / View.COLUMNS;
        int col = pos % View.COLUMNS;
        return row >= rowOffset && row < rowOffset + rows && col >= columnOffset && col < columnOffset + columns;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        InventoryView that = (InventoryView) o;

        if (columnOffset != that.columnOffset) return false;
        if (rowOffset != that.rowOffset) return false;
        if (columns != that.columns) return false;
        return rows == that.rows;

    }

    @Override
    public int hashCode() {
        int result = columnOffset;
        result = 31 * result + rowOffset;
        result = 31 * result + columns;
        result = 31 * result + rows;
        return result;
    }

    @Override
    public String toString() {
        return "InventoryView(" +
                "columnOffset=" + columnOffset +
                ", rowOffset=" + rowOffset +
                ", columns=" + columns +
                ", rows=" + rows +
                ')';
    }
}
