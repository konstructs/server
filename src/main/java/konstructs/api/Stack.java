package konstructs.api;

import java.util.Arrays;

/**
 * Stack is a class that describes a collection of blocks of
 * a certain BlockTypeId. All blocks of a stack must be of the
 * same BlockTypeId. A stack holds a maximum of 64 blocks.
 * <p>
 * Since stacks are immutable, when changing the composition
 * of a stack a new stack is returned. This is the case of all
 * objects in konstructs, but since a stack is basically a list
 * it uses something called a head and a tail. These are concepts
 * common with immutable lists and if you already know about them
 * you can safely skip this section. They work as usual, except that
 * the empty list is represented by null. Otherwise a short introduction
 * to the head and the tail of the stack follows.
 * </p>
 * <p>
 * A stack must have at least one element. An empty stack is
 * represented by null. The head of the stack is the first block
 * in the stack. It can be accessed via the getHead() method.
 * The tail of the stack are all block of the stack, except the head
 * block. If the stack only contains the head block, the tail of
 * the stack is null. The tail of the stack is in itself a stack and
 * the getTail() method returns a new stack that contains the tail of
 * the current stack. An example of getting a block from a stack:
 * </p>
 * <pre>
 *     // Get the first block in the stack
 *     Block b = stack.head();
 *
 *     // Update the stack variable with the tail of the stack
 *     // It can be null and if it is, the stack is empty
 *     stack = stack.tail();
 * </pre>
 * <p>
 * More generic variant of the head and tail are the take(int) and drop(int) methods.
 * Take returns a new stack of the first n blocks in the stack in the same way
 * as head returns the first block. Drop returns a new stack without the first n
 * blocks in the stack, much in the same way as getTail returns all blocks except
 * the first one.
 * </p>
 * @see #getHead()
 * @see #getTail()
 * @see #take(int)
 * @see #drop(int)
 */
public final class Stack {
    public static final int MAX_SIZE = 64;

    public static Stack convertPre0_1(Stack stack) {
        if(stack == null || stack.getBlocks().length == 0) {
            return null;
        } else {
            return stack;
        }
    }

    /**
     * Create a new stack of size 1 from a given block
     * @param block The block to be included in the stack
     * @return The new stack
     */
    public static Stack createFromBlock(Block block) {
        Block[] blocks = { block };
        return new Stack(blocks);
    }

    private final Block[] blocks;

    /**
     * Construct an immutable Stack
     * All blocks must be of the same BlockTypeId
     * The array of blocks must be at least one block long
     * @param blocks The to be included in the stack
     */
    public Stack(Block[] blocks) {
        this.blocks = blocks;
        if(blocks.length < 1)
            throw new IllegalArgumentException("A stack must contain at least one block");
        for(Block block: blocks) {
            if(block == null) {
                throw new IllegalArgumentException("All blocks must be non-null");
            }
            if(!block.getType().equals(getTypeId())) {
                throw new IllegalArgumentException("All blocks in a stack must be of the same BlockTypeId");
            }
        }
    }

    /**
     * Get the array of blocks contained in this stack
     * @return The array of blocks of the stack
     */
    public Block[] getBlocks() {
        return blocks;
    }

    /**
     * Get the BlockTypeId of all blocks in the stack
     * @return The BlockTypeId of all blocks in the stack
     */
    public BlockTypeId getTypeId() {
        return blocks[0].getType();
    }

    /**
     * Get the size of this stack (i.e. the length of the blocks array)
     * @return The size of this stack
     */
    public int size() {
        return blocks.length;
    }

    /**
     * Get the number of blocks that still fit in this stack
     * @return The number of blocks that still fit
     */
    public int getRoomLeft() {
        return Stack.MAX_SIZE - size();
    }

    /**
     * Check if this stack is full
     * @return True if this stack is full
     */
    public boolean isFull() {
        return blocks.length == Stack.MAX_SIZE;
    }

    /**
     * Return the head of this stack
     * @return The head of the stack
     */
    public Block getHead() {
        return blocks[0];
    }

    /**
     * Return the tail of this stack
     * @return The tail of this stack
     */
    public Stack getTail() {
        if(blocks.length == 1) return null;
        int newLength = blocks.length - 1;
        return new Stack(Arrays.copyOf(blocks, newLength));
    }

    /**
     * Returns a new stack with the first n blocks of this stack
     * @param n The number of blocks to take
     * @return A new stack with the n first blocks of this stack
     */
    public Stack take(int n) {
        return new Stack(Arrays.copyOf(blocks, Math.min(n, blocks.length)));
    }

    /**
     * Returns a new stack without the first n blocks if this stack
     * @param n The number of blocks to drop
     * @return A new stack without the first n blocks of this stack
     */
    public Stack drop(int n) {
        if(blocks.length <= n ) return null;
        return new Stack(Arrays.copyOfRange(blocks, n, blocks.length));
    }

    /**
     * Check whether this tacks can accept part of another stack.
     * @param stack The stack to check
     * @return True if both stacks are of the same type and this
     *         stack is not yet full
     */
    public boolean acceptsPartOf(Stack stack) {
        return stack.getTypeId().equals(getTypeId()) && !isFull();
    }

    /**
     * Check whether this stack can accept a block
     * @param block The block to check
     * @return True if the block is of the same type as the stack and
     *         the stack is not yet full
     */
    public boolean accepts(Block block) {
        return block.getType().equals(getTypeId()) && !isFull();
    }

    /**
     * Accept blocks from another stack
     * Please first validate that the stack can be accepted using accpetsPartOf
     * @param stack The stack from which parts should be accepted
     * @return The AcceptResult that contains any part of the given stack
     * that could not be fitted within this stack as well as a new stack
     * with the blocks of the given stack added
     * @see AcceptResult
     */
    public AcceptResult<Stack> acceptPartOf(Stack stack) {
        if (acceptsPartOf(stack)) {
            int r = getRoomLeft();
            Stack taken = stack.take(r);
            Block[] newBlocks = new Block[taken.size() + blocks.length];
            System.arraycopy(blocks, 0, newBlocks, 0, blocks.length);
            System.arraycopy(taken.getBlocks(), 0, newBlocks, blocks.length, taken.size());
            return new AcceptResult(new Stack(newBlocks), stack.drop(r));
        } else {
            return null;
        }
    }

    /**
     * Accept a single block
     * Please first validate that the block can be accepted with the accepts method
     * @param block The block to be accepted
     * @return A new stack containing the given block
     */
    public Stack accept(Block block) {
        if(accepts(block)) {
            Block[] newBlocks = Arrays.copyOf(blocks, blocks.length + 1);
            newBlocks[blocks.length ] = block;
            return new Stack(newBlocks);
        } else {
            return null;
        }
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Stack stack = (Stack) o;

        return Arrays.equals(blocks, stack.blocks);

    }

    @Override
    public int hashCode() {
        return Arrays.hashCode(blocks);
    }

    @Override
    public String toString() {
        return "Stack(" +
                "blocks=" + Arrays.toString(blocks) +
                ')';
    }
}
