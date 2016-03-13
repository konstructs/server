package konstructs.api;

import java.util.UUID;
import java.util.Map;

public interface BlockFactory {
    public Block createBlock(UUID uuid, int w);
    public Block createBlock(int w);
    public int getW(Block block);
    public int getW(Stack stack);
    public int getW(BlockTypeId typeId);
    public BlockTypeId getBlockTypeId(int w);
    public BlockType getBlockType(BlockTypeId typeId);
    public Map<BlockTypeId, BlockType> getBlockTypes();
    public Map<Integer, BlockTypeId> getWMapping();
}
