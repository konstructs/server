package konstructs.api;

/**
 * AcceptResult is a class that holds the result from an inventory or
 * stack accepting a number of blocks. Accepting in this context here
 * means merging one stack into an inventory or another stack. Since
 * the stack is limited to holding 64 blocks, it is not certain that
 * all blocks from the giving stack will fit into the accepting stack
 * (you are merging the giving stack into the accepting stack/inventory).
 * Therefore this class is used to hold the the remaining blocks of the
 * giving stack and the new resulting inventory/stack which contains the
 * rest of the blocks.
 *
 * @param <T> Inventory or Stack (the type of the accepting entity)
 * @see Stack
 * @see Inventory
 */
public class AcceptResult<T> {
    private final T accepting;
    private final Stack giving;

    /**
     * Constructs an immutable AcceptResult
     * @param accepting the resulting accepting stack/inventory
     * @param giving the remains of the giving stack
     */
    public AcceptResult(T accepting, Stack giving) {
        this.accepting = accepting;
        this.giving = giving;
    }

    /**
     * Returns the resulting stack/inventory with blocks it could accept
     * from giving.
     * @return The accepting inventory/stack
     */
    public T getAccepting() {
        return accepting;
    }

    /**
     * Returns the remains of the giving stack, it may be null if all
     * blocks from it fit into accepting.
     * @return The remains of the giving stack
     */
    public Stack getGiving() {
        return giving;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        AcceptResult that = (AcceptResult) o;

        if (!accepting.equals(that.accepting)) return false;
        return giving.equals(that.giving);

    }

    @Override
    public int hashCode() {
        int result = accepting.hashCode();
        result = 31 * result + giving.hashCode();
        return result;
    }

    @Override
    public String toString() {
        return "AcceptResult(" +
                "accepting=" + accepting +
                ", giving=" + giving +
                ')';
    }
}