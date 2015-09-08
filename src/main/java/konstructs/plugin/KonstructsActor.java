package konstructs.plugin;

import akka.actor.ActorRef;
import akka.actor.UntypedActorWithStash;
import konstructs.api.BlockDataUpdate;
import konstructs.api.GetBlockResponse;
import konstructs.api.PutBlock;
import konstructs.api.GetBlock;
import konstructs.api.DestroyBlock;
import konstructs.api.Position;
import konstructs.api.ReceiveStack;
import konstructs.api.Block;
import java.util.Collection;
import java.util.concurrent.TimeUnit;
import scala.concurrent.duration.Duration;

public abstract class KonstructsActor extends UntypedActorWithStash {

    ActorRef universe;

    public KonstructsActor(ActorRef universe) {
        this.universe = universe;
    }

    /**
     * Called by Akka when we receive a message
     */
    public void onReceive(Object message) {

        if (message instanceof GetBlockResponse) {
            GetBlockResponse blockPosition = (GetBlockResponse)message;
            onGetBlockResponse(blockPosition);
            return;
        }

        if (message instanceof BlockDataUpdate) {
            BlockDataUpdate blockUpdate = (BlockDataUpdate)message;
            onBlockDataUpdate(blockUpdate);
            return;
        }

        if (message instanceof ReceiveStack) {
            ReceiveStack receiveBlock = (ReceiveStack)message;
            onReceiveStack(receiveBlock);
            return;
        }

    }

    /**
     * Return universe ActorRef.
     * @return ActorRef
     */
    public ActorRef getUniverse() {
        return universe;
    }

    /**
     * This function is called when we receive a GetBlockResponse message.
     */
    public void onGetBlockResponse(GetBlockResponse blockPosition) {
        System.out.println("called onGetBlockResponse: not implemented");
    }

    /**
     * This function is called when we receive a BlockDataUpdate message.
     */
    public void onBlockDataUpdate(BlockDataUpdate blockUpdate) {
        System.out.println("called onBlockDataUpdate: not implemented");
    }

    /**
     * This function is called when we receive a ReceiveStack message.
     */
    public void onReceiveStack(ReceiveStack receiveBlock) {
        System.out.println("called ReceiveStack: not implemented");
    }

    /**
     * Write a collection of blocks to the world.
     * @param   blocks      A collection of blocks.
     */
    public void putBlocks(Collection<PutBlock> blocks) {
        for (PutBlock b : blocks) {
            putBlock(b);
        }
    }

    /**
     * Write a single block to the world.
     * @param   b   A block
     */
    public void putBlock(PutBlock b) {
        universe.tell(b, getSelf());
    }

    /**
     * Write a single block to the world.
     * @param   p   The position of the block
     * @param   b   The block
     */
    public void putBlock(Position p, Block b) {
        putBlock(new PutBlock(p, b));
    }

    /**
     * Ask the server for a block
     * @param   p   The position
     */
    public void getBlock(Position p) {
        universe.tell(new GetBlock(p), getSelf());
    }

    /**
     * Destroy a block.
     * @param   p   The position
     */
    public void destroyBlock(Position p) {
        universe.tell(new DestroyBlock(p), getSelf());
    }

    /**
     * Destroy a collection of blocks.
     * @param   blocks      A collection of blocks.
     */
    public void destroyBlocks(Collection<PutBlock> blocks) {
        for (PutBlock b : blocks) {
            destroyBlock(b.pos());
        }
    }

    /**
     * Schedule a message to my self
     * @param   obj  The object to send
     * @param   msec Time to wait, in milliseconds
     */
    public void scheduleSelfOnce(Object obj, int msec) {
        getContext().system().scheduler().scheduleOnce(
                Duration.create(msec, TimeUnit.MILLISECONDS),
                getSelf(), obj, getContext().system().dispatcher(), null);
    }

}
