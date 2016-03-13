package konstructs.api;

/**
 * Created by petter on 2016-02-29.
 */
public class AcceptResult<T> {
    private final T accepting;
    private final Stack giving;

    public AcceptResult(T accepting, Stack giving) {
        this.accepting = accepting;
        this.giving = giving;
    }

    public T getAccepting() {
        return accepting;
    }

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