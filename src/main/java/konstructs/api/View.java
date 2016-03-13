package konstructs.api;

import java.util.HashMap;
import java.util.Map;

/**
 * Created by petter on 2016-02-29.
 */
public final class View {
    public static final int COLUMNS = 17;
    public static final int ROWS = 14;
    public static final View EMPTY = new View(new HashMap());
    private final Map<Integer, Stack> items;

    public View(Map<Integer, Stack> items) {
        this.items = items;
    }

    public Map<Integer, Stack> getItems() {
        return items;
    }

    public View add(InventoryView inventoryView, Inventory inventory) {
        Map<Integer, Stack> newItems = new HashMap(items);
        for(int row = 0; row < inventoryView.getRows(); row++) {
            for (int column = 0; column < inventoryView.getColumns(); column++) {
                int r = row + inventoryView.getRowOffset();
                int c = column + inventoryView.getColumnOffset();
                newItems.put(r * COLUMNS + c, inventory.getStacks()[row * inventoryView.getColumns() + column]);
            }
        }
        return new View(newItems);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        View view = (View) o;

        return items.equals(view.items);

    }

    @Override
    public int hashCode() {
        return items.hashCode();
    }

    @Override
    public String toString() {
        return "View(" +
                "items=" + items +
                ')';
    }
}
