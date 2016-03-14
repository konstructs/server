package konstructs.api;

import java.util.Arrays;

/**
 * Created by petter on 2016-02-29.
 */
public final class Inventory {
    public static Inventory createEmpty(int size) {
        return new Inventory(new Stack[size]);
    }

    private final Stack[] stacks;

    public Inventory(Stack[] stacks) {
        this.stacks = stacks;
    }

    public Stack[] getStacks() {
        return stacks;
    }

    public boolean isEmpty() {
        for(Stack s: stacks) {
            if(s != null) return false;
        }
        return true;
    }

    public Inventory withoutSlot(int slot) {
        Stack[] newStacks = Arrays.copyOf(stacks, stacks.length);
        newStacks[slot] = null;
        return new Inventory(newStacks);
    }

    public Inventory withSlot(int slot, Stack stack) {
        Stack[] newStacks = Arrays.copyOf(stacks, stacks.length);
        newStacks[slot] = stack;
        return new Inventory(newStacks);
    }

    public Stack getStack(int slot) {
        return stacks[slot];
    }

    public Block stackHead(int slot) {
        Stack s = stacks[slot];
        if(s == null) return null;
        return s.getHead();
    }

    public Inventory stackTail(int slot) {
        Stack stack = stacks[slot];
        if(stack == null) return this;
        return withSlot(slot, stack.getTail());
    }

    public boolean accepts(Block b) {
        for(int i = 0; i < stacks.length; i++) {
            Stack s = stacks[i];
            if(s == null || s.accepts(b)) {
                return true;
            }
        }
        return false;
    }

    public Inventory accept(Block block) {
       for (int i = 0; i < stacks.length; i++) {
           Stack s = stacks[i];
           if (s != null && s.accepts(block)) {
               return withSlot(i, s.accept(block));
           }
       }
       for (int i = 0; i < stacks.length; i++) {
           Stack s = stacks[i];
           if (s == null) {
               return withSlot(i, Stack.createFromBlock(block));
           }
       }
       return this;
    }

    public boolean acceptsPartOf(Stack stack) {
        for(int i = 0; i < stacks.length; i++) {
            Stack s = stacks[i];
            if(s == null || s.acceptsPartOf(stack)) {
                return true;
            }
        }
        return false;
    }

    public AcceptResult<Inventory> acceptPartOf(Stack stack) {
        Stack left = stack;
        Inventory result = this;
        /* Try to distribute the stack over existing stacks */
        for (int i = 0; i < stacks.length; i++) {
            Stack s = stacks[i];
            if (s != null && s.acceptsPartOf(left)) {
                AcceptResult<Stack> r = s.acceptPartOf(left);
                if (r.getGiving() == null) {
                    return new AcceptResult(result.withSlot(i, r.getAccepting()), null);
                } else {
                    result = result.withSlot(i, r.getAccepting());
                    left = r.getGiving();
                }
            }
        }
        /* Try to fit any leftovers in an empty stack */
        for (int i = 0; i < stacks.length; i++) {
            Stack s = stacks[i];
            if (s == null) {
                return new AcceptResult(result.withSlot(i, left), null);
            }
        }
        /* Return the leftovers (this may be the complete stack if the inventory is full) */
        return new AcceptResult(result, left);
    }

    public Inventory remove(Stack stack) {
        Stack toRemove = null;
        Inventory result = this;
        for (int i = 0; i < stacks.length; i++) {
            Stack s = stacks[i];
            if (s.getTypeId().equals(stack.getTypeId())) {
                if(toRemove == null) {
                    toRemove = s.take(stack.size());
                    result = withSlot(i, s.drop(stack.size()));
                } else {
                    int n = stack.size() - toRemove.size();
                    AcceptResult<Stack> r = toRemove.acceptPartOf(s.take(n));
                    result = withSlot(i, s.drop(n));
                    toRemove = r.getAccepting();
                }
                if(stack.equals(toRemove)) {
                    return result;
                }
            }
        }

        /* Not enough blocks in inventory to fully provide stack */
        return this;
    }

    public Inventory moveSlot(int from, int to) {
        Stack fromStack = stacks[from];
        Stack toStack = stacks[to];
        return withSlot(from, toStack).withSlot(to, fromStack);
    }

    private static class PatternFrame {
        private final int firstColumn;
        private final int lastColumn;
        private final int firstRow;
        private final int lastRow;

        public PatternFrame(int firstColumn, int lastColumn, int firstRow, int lastRow) {
            this.firstColumn = firstColumn;
            this.lastColumn = lastColumn;
            this.firstRow = firstRow;
            this.lastRow = lastRow;
        }

        public int getFirstColumn() {
            return firstColumn;
        }

        public int getLastColumn() {
            return lastColumn;
        }

        public int getFirstRow() {
            return firstRow;
        }

        public int getLastRow() {
            return lastRow;
        }

        public int getNumberOfStacks() {
            int columns = lastColumn - firstColumn;
            int rows = lastRow - firstRow;
            return columns *rows;
        }

        public int getColumns() {
            return lastColumn - firstColumn;
        }

        @Override
        public String toString() {
            return "PatternFrame(" +
                    "firstColumn=" + firstColumn +
                    ", lastColumn=" + lastColumn +
                    ", firstRow=" + firstRow +
                    ", lastRow=" + lastRow +
                    ')';
        }
    }

    private boolean isRowEmpty(int row, InventoryView view) {
        for(int c = 0; c < view.getColumns(); c++) {
            if(stacks[row * view.getColumns() + c] != null) return false;
        }
        return true;
    }

    private boolean isColumnEmpty(int column, InventoryView view) {
        for(int r = 0; r < view.getRows(); r++) {
            if(stacks[r * view.getColumns() + column] != null) return false;
        }
        return true;
    }

    private PatternFrame getPatternFrame(InventoryView view) {
        int firstRow = 0;
        int lastRow = view.getRows();
        int firstColumn = 0;
        int lastColumn = view.getColumns();

        for(int r = 0; r < view.getRows(); r++) {
            if(isRowEmpty(r, view))
                firstRow = r + 1;
            else
                break;
        }

        for(int r = view.getRows() - 1; r >= 0 ; r--) {
            if(isRowEmpty(r, view))
                lastRow = r;
            else
                break;
        }

        for(int c = 0; c < view.getColumns(); c++) {
            if(isColumnEmpty(c, view))
                firstColumn = c + 1;
            else
                break;
        }

        for(int c = view.getColumns() - 1; c >= 0; c--) {
            if(isColumnEmpty(c, view))
                lastColumn = c;
            else
                break;
        }

        return new PatternFrame(firstColumn, lastColumn, firstRow, lastRow);
    }

    public Pattern getPattern(InventoryView view) {
        if(isEmpty()) {
            return null;
        } else {
            PatternFrame frame = getPatternFrame(view);
            Stack[] patternStacks = new Stack[frame.getNumberOfStacks()];
            for(int row = frame.getFirstRow(); row < frame.getLastRow(); row++) {
                for(int column = frame.getFirstColumn(); column < frame.getLastColumn(); column++) {
                    Stack s = stacks[row * view.getColumns() + column];
                    patternStacks[(row - frame.getFirstRow()) * frame.getColumns() + column - frame.getFirstColumn()] = s;
                }
            }
            return new Pattern(patternStacks, frame.getLastRow() - frame.getFirstRow(), frame.getLastColumn() - frame.getFirstColumn());
        }
    }

    public Inventory removePattern(Pattern pattern) {
        Inventory newInventory = this;
        for(Stack stack: pattern.getStacks()) {
            newInventory = newInventory.remove(stack);
        }
        return newInventory;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Inventory inventory = (Inventory) o;

        // Probably incorrect - comparing Object[] arrays with Arrays.equals
        return Arrays.equals(stacks, inventory.stacks);

    }

    @Override
    public int hashCode() {
        return Arrays.hashCode(stacks);
    }

    @Override
    public String toString() {
        return "Inventory(" +
                "stacks=" + Arrays.toString(stacks) +
                ')';
    }
}
