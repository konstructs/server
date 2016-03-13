package konstructs.api;

public class Konstruct {
    private final Pattern pattern;
    private final Stack result;

    public Konstruct(Pattern pattern, Stack result) {
        this.pattern = pattern;
        this.result = result;
    }

    public Pattern getPattern() {
        return pattern;
    }

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
