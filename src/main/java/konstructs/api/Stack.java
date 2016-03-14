package konstructs.api;

import java.util.Arrays;

public final class Stack {
    public static final int MAX_SIZE = 64;

    public static Stack createFromBlock(Block block) {
        Block[] blocks = { block };
        return new Stack(block.getType(), blocks);
    }

    private final BlockTypeId typeId;
    private final Block[] blocks;

    public Stack(BlockTypeId typeId, Block[] blocks) {
        if(blocks.length < 1)
            throw new IllegalArgumentException("A stack must contain at least one block");
        this.typeId = typeId;
        this.blocks = blocks;
    }

    public Block[] getBlocks() {
        return blocks;
    }

    public BlockTypeId  getTypeId() {
        return typeId;
    }

    public int size() {
        return blocks.length;
    }

    public int getRoomLeft() {
        return Stack.MAX_SIZE - size();
    }

    public boolean isFull() {
        return blocks.length == Stack.MAX_SIZE;
    }

    public Block getHead() {
        return blocks[0];
    }

    public Stack getTail() {
        if(blocks.length == 1) return null;
        int newLength = blocks.length - 1;
        return new Stack(typeId, Arrays.copyOf(blocks, newLength));
    }

    public Stack take(int n) {
        return new Stack(typeId, Arrays.copyOf(blocks, Math.min(n, blocks.length)));
    }

    public Stack drop(int n) {
        if(blocks.length <= n ) return null;
        return new Stack(typeId, Arrays.copyOfRange(blocks, n, blocks.length));
    }

    public boolean acceptsPartOf(Stack stack) {
        return stack.typeId.equals(typeId) && !isFull();
    }

    public boolean accepts(Block block) {
        return block.getType().equals(typeId) && !isFull();
    }

    public AcceptResult<Stack> acceptPartOf(Stack stack) {
        if (acceptsPartOf(stack)) {
            int r = getRoomLeft();
            Stack taken = stack.take(r);
            Block[] newBlocks = new Block[taken.size() + blocks.length];
            System.arraycopy(blocks, 0, newBlocks, 0, blocks.length);
            System.arraycopy(taken.getBlocks(), 0, newBlocks, blocks.length, taken.size());
            return new AcceptResult(new Stack(typeId, newBlocks), stack.drop(r));
        } else {
            return null;
        }
    }

    public Stack accept(Block block) {
        if(accepts(block)) {
            Block[] newBlocks = Arrays.copyOf(blocks, blocks.length + 1);
            newBlocks[blocks.length ] = block;
            return new Stack(typeId, newBlocks);
        } else {
            return null;
        }
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Stack stack = (Stack) o;

        if (!typeId.equals(stack.typeId)) return false;
        // Probably incorrect - comparing Object[] arrays with Arrays.equals
        return Arrays.equals(blocks, stack.blocks);

    }

    @Override
    public int hashCode() {
        int result = typeId.hashCode();
        result = 31 * result + Arrays.hashCode(blocks);
        return result;
    }
}
