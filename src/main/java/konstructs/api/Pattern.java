package konstructs.api;

import java.util.Arrays;

/**
 * Created by petter on 2016-02-29.
 */
public final class Pattern {
    private final Stack[] stacks;
    private final int columns;
    private final int rows;

    public Pattern(Stack[] stacks, int columns, int rows) {
        this.stacks = stacks;
        this.columns = columns;
        this.rows = rows;
    }

    public Stack[] getStacks() {
        return stacks;
    }

    public int getColumns() {
        return columns;
    }

    public int getRows() {
        return rows;
    }

    public int size() {
        return stacks.length;
    }

    public int getComplexity() {
        int complexity = 0;
        for(Stack s: stacks) {
            if(s != null)
                complexity += s.size();
        }
        return complexity;
    }

    public boolean contains(Pattern p) {
        if(p.getRows() == rows && p.getColumns() == columns && size() == p.size()) {
            for(int i = 0; i < size(); i++) {
                Stack self = stacks[i];
                Stack other = p.getStacks()[i];
                if(!self.getTypeId().equals(other.getTypeId())) return false;
                if(self.size() < other.size()) return false;
            }
            return true;
        } else {
            return false;
        }
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Pattern pattern = (Pattern) o;

        if (columns != pattern.columns) return false;
        if (rows != pattern.rows) return false;
        // Probably incorrect - comparing Object[] arrays with Arrays.equals
        return Arrays.equals(stacks, pattern.stacks);

    }

    @Override
    public int hashCode() {
        int result = Arrays.hashCode(stacks);
        result = 31 * result + columns;
        result = 31 * result + rows;
        return result;
    }

    @Override
    public String toString() {
        return "Pattern(" +
                "stacks=" + Arrays.toString(stacks) +
                ", columns=" + columns +
                ", rows=" + rows +
                ')';
    }
}
