package konstructs.api;

import java.util.UUID;
import java.util.Map;

/**
 * BlockFactory is an interface that provides access to information about
 * blocks. You can query it for a blocks BlockType as well as the underlying
 * w-value (you usually never need to know this) used in the block database.
 */
public interface BlockFactory {
    /**
     * Creates a new block with the given w-value and ID
     * <p>
     * Note: This method is mainly for internal use and is only required
     * when altering the low level block format.
     * </p>
     * @param uuid The ID of the new block
     * @param w The w-value that identifies the blocks BlockTypeId
     * @return The newly created Block
     */
    public Block createBlock(UUID uuid, int w);

    /**
     * Creates a new block with the given w-value
     * <p>
     * Note: This method is mainly for internal use and is only required
     * when altering the low level block format.
     * </p>
     * @param w The w-value that identifies the blocks BlockTypeId
     * @return The newly created Block
     */
    public Block createBlock(int w);

    /**
     * Get the w-value of a block.
     * <p>
     * Note: This method is mainly for internal use and is only required
     * when altering the low level block format.
     * </p>
     * @param block The block for which the w-value is required
     * @return The w-value of the block
     */
    public int getW(Block block);

    /**
     * Get the w-value of a stack.
     * <p>
     * Note: This method is mainly for internal use and is only required
     * when altering the low level block format.
     * </p>
     * @param stack The stack for which the w-value is required
     * @return The w-value of the stack
     */
    public int getW(Stack stack);

    /**
     * Get the w-value for a BlockTypeId.
     * <p>
     * Note: This method is mainly for internal use and is only required
     * when altering the low level block format.
     * </p>
     * @param typeId The BlockTypeId for which the w-value is required
     * @return The w-value of the BlockTypeId
     */
    public int getW(BlockTypeId typeId);

    /**
     * Get the BlockTypeId for a given w-value.
     * <p>
     * Note: This method is mainly for internal use and is only required
     * when altering the low level block format.
     * </p>
     * @param w The w-value for which the BlockTypeId is required
     * @return The BlockTypeId associated with the w-value
     */
    public BlockTypeId getBlockTypeId(int w);

    /**
     * Get a BlockType
     * @param typeId The BlockTypeId that identifies the BlockType
     * @return The BlockType
     */
    public BlockType getBlockType(BlockTypeId typeId);

    /**
     * Returns the complete mapping between BlockTypeId and BlockType.
     * This is immutable and can be saved for easy access.
     * @return The BlockTypeId to BlockType mapping
     */
    public Map<BlockTypeId, BlockType> getBlockTypes();

    /**
     * Returns the complete mapping between w-values and BlockTypeIds
     * <p>
     * Note: This method is mainly for internal use and is only required
     * when altering the low level block format.
     * </p>
     * @return The w-value to BlockTypeId mapping
     */
    public Map<Integer, BlockTypeId> getWMapping();
}
