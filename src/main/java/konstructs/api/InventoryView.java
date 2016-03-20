package konstructs.api;

/**
 * InventoryView is a class that describes the layout (in two dimensions)
 * of an Inventory as well as a placement of within a View. It has a
 * rowOffset and columnsOffset which defines where it is placed within a View.
 * It has a number of rows and columns which are mapped into a Inventory of
 * size rows * columns.
 * <p>
 * When a message is received from the HUD this class can be used to identify
 * if it was directed to this specific view (i.e. if the action was done within
 * the area of this view) using the contains method. It can also generate a one
 * dimensional index that can be used to access the slots in an underlying
 * Inventory.
 * </p>
 * @see Inventory
 * @see View
 */
public final class InventoryView {
    private final int columnOffset;
    private final int rowOffset;
    private final int columns;
    private final int rows;

    /**
     * Creates an immutable inventory view.
     * @param rowOffset The row offset at which this InventoryView is placed in the HUD
     * @param columnOffset The column offset at which this InventoryView is placed in the HUD
     * @param rows The number of rows in this InventoryView
     * @param columns The number of columns in this InventoryView
     */
    public InventoryView(int rowOffset, int columnOffset, int rows, int columns) {
        this.columnOffset = columnOffset;
        this.rowOffset = rowOffset;
        this.columns = columns;
        this.rows = rows;
    }

    /**
     * Get the column offset at which this InventoryView is placed in the HUD
     * @return The columns offset
     */
    public int getColumnOffset() {
        return columnOffset;
    }

    /**
     * Get the row offset at which this InventoryView is placed in the HUD
     * @return The row offset
     */
    public int getRowOffset() {
        return rowOffset;
    }

    /**
     * Get the number of columns in this InventoryView
     * @return The number of columns
     */
    public int getColumns() {
        return columns;
    }

    /**
     * Get the number of rows in this InventoryView
     * @return The number of rows
     */
    public int getRows() {
        return rows;
    }

    /**
     * Translates a position in the HUD to a position in a Inventory of
     * the same size as this InventoryView (columns*rows).
     * @param position The position in the HUD
     * @return The position in the Inventory
     */
    public int translate(int position) {
        int r = position / View.COLUMNS;
        int c = position % View.COLUMNS;
        int row = r - rowOffset;
        int column = c - columnOffset;
        return row * columns + column;
    }

    /**
     * Check whether this InventoryView contains a specific HUD position.
     * If this method returns true, then the translate method can safely
     * be used to translate the HUD position given to a position in an
     * Inventory.
     * @param position The HUD position / index to check
     * @return True if this InventoryView contains the HUD position
     */
    public boolean contains(int position) {
        int row = position / View.COLUMNS;
        int col = position % View.COLUMNS;
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
