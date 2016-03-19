package konstructs.api;

/**
 * Konstruct is a class that describes a recipe for creating new
 * blocks. It contains a pattern that should be matched for a
 * resulting stack to be provided. For more details on crafting,
 * please see Pattern and InventoryView.
 * @see Pattern
 * @see InventoryView
 */
public class Konstruct {
    private final Pattern pattern;
    private final Stack result;

    /**
     * Construct an immutable Konstruct
     * @param pattern The pattern that needs to be matched
     * @param result The resulting stack
     */
    public Konstruct(Pattern pattern, Stack result) {
        this.pattern = pattern;
        this.result = result;
    }

    /**
     * Get the pattern
     * @return The pattern
     */
    public Pattern getPattern() {
        return pattern;
    }

    /**
     * Get the stack
     * @return The stack
     */
    public Stack getResult() {
        return result;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Konstruct konstruct = (Konstruct) o;

        if (!pattern.equals(konstruct.pattern)) return false;
        return result.equals(konstruct.result);

    }

    @Override
    public int hashCode() {
        int result1 = pattern.hashCode();
        result1 = 31 * result1 + result.hashCode();
        return result1;
    }

    @Override
    public String toString() {
        return "Konstruct(" +
                "pattern=" + pattern +
                ", result=" + result +
                ')';
    }
}
