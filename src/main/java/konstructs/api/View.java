package konstructs.api;

import java.util.HashMap;
import java.util.Map;

/**
 * View is a class that represents the HUD as shown to the user
 * The HUD has 14 rows and 17 columns. To populate the View
 * one can place Inventories into the View with the help of
 * the InventoryView class.
 *
 * The different parts of the HUD is accessed through the HUD index
 * this index is a simple index treating the HUD as a flat array,
 * rather than a 2 dimensional surface. View and InventoryView automatically
 * takes care of all the conversions between HUD indexes and the 2 dimensional
 * coordinates of the actual on screen HUD.
 *
 * @see Inventory
 * @see InventoryView
 */
public final class View {
    public static final int COLUMNS = 17;
    public static final int ROWS = 14;
    /**
     * Definition of an empty view with no content. This is
     * the starting point to create a view.
     */
    public static final View EMPTY = new View(new HashMap());
    private final Map<Integer, Stack> items;

    private View(Map<Integer, Stack> items) {
        this.items = items;
    }

    /**
     * Get the mapping between HUD indexes and stacks of this view
     * @return A mapping from HUD index to Stack
     * @see Stack
     */
    public Map<Integer, Stack> getItems() {
        return items;
    }

    /**
     * Add an inventory to this View
     * Returns a new view to which the Inventory has been added
     * @param inventoryView The InventoryView that defines where in the HUD
     *                      the inventory should be shown as well as the dimensions
     *                      of it
     * @param inventory The Inventory to show in the HUD
     * @return A new View that shows the given inventory at the given position
     * @see Inventory
     * @see InventoryView
     */
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
