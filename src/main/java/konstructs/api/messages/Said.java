package konstructs.api.messages;

public class Said {
    private final String text;

    public Said(String text) {
        this.text = text;
    }

    public String getText() {
        return text;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Said said = (Said) o;

        return text.equals(said.text);

    }

    @Override
    public int hashCode() {
        return text.hashCode();
    }

    @Override
    public String toString() {
        return "Said(" +
                "text='" + text + '\'' +
                ')';
    }
}
