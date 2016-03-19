package konstructs.api;

import java.io.Serializable;

/**
 * BlockTypeId is a class that holds the <code>namespace</code> and
 * <code>name</code> of a block type. This information is very static
 * and should never change after a new block type has been introduced
 * into the game. The <code>namespace</code> should preferable be
 * something unique for the plugin, like a domain name or the name of
 * the plugin. Avoid generic namespaces like "stones", "forest". Use
 * namespaces like "your/domain/stones" where are in control of
 * "your.domain" or "steampunk/metal" where "steampunk" is the name of
 * your unique plugin. Since the <code>name</code> is always prefixed
 * by <code>namespace</code> it must only be unique within your
 * plugins namespace. It is immutable and serializable.
 */
public final class BlockTypeId implements Serializable {
    public final static BlockTypeId VACUUM = new BlockTypeId("org/konstructs", "vacuum");

    /**
     * Create a new immutable BlockTypeId from a name string. A name
     * string consist of the namespace appended with a / and then the
     * name, e.g. namespace "org/konstructs" and name "grass" would
     * become "org/kosntructs/grass".
     * @param id the block id to be parsed for
     * @return a new immutable BlockTypeId
     */
    public static BlockTypeId fromString(String id) {
        int lastSlash = id.lastIndexOf('/');
        String namespace = id.substring(0, lastSlash);
        String name = id.substring(lastSlash + 1);
        return new BlockTypeId(namespace, name);
    }

    private final String namespace;
    private final String name;
    /**
     * Constructs an immutable BlockTypeId.
     * @param namespace the namespace of this BlockTypeId
     * @param name the name of this BlockTypeId
     * @see BlockTypeId
     */
    public BlockTypeId(String namespace, String name) {
        this.namespace = namespace;
        this.name = name;
    }

    /**
     * Returns the namespace of this <code>BlockTypeId</code>
     * @return the namespace
     * @see BlockTypeId
     */
    public String getNamespace() {
        return namespace;
    }


    /**
     * Returns the name of this <code>BlockTypeId</code>
     * @return the name
     * @see BlockTypeId
     */
    public String getName() {
        return name;
    }

    /**
     * Returns a copy with a new value for the namespace
     * field.
     * @param namespace the new value for namespace
     * @return the copy
     */
    public BlockTypeId withNamespace(String namespace) {
        return new BlockTypeId(namespace, name);
    }

    /**
     * Returns a copy with a new value for the name
     * field.
     * @param name the new value for name
     * @return the copy
     */
    public BlockTypeId withName(String name) {
        return new BlockTypeId(namespace, name);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        BlockTypeId that = (BlockTypeId) o;

        if (!namespace.equals(that.namespace)) return false;
        return name.equals(that.name);

    }

    @Override
    public int hashCode() {
        int result = namespace.hashCode();
        result = 31 * result + name.hashCode();
        return result;
    }

    @Override
    public String toString() {
        return "BlockTypeId(" +
                "namespace='" + namespace + '\'' +
                ", name='" + name + '\'' +
                ')';
    }
}
