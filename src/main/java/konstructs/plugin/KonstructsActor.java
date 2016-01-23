package konstructs.plugin;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.TimeUnit;
import java.util.ArrayList;
import scala.concurrent.duration.Duration;

import akka.actor.ActorRef;
import akka.actor.UntypedActorWithStash;
import konstructs.api.*;
import konstructs.Box;

public abstract class KonstructsActor extends UntypedActorWithStash {

    ActorRef universe;

    public KonstructsActor(ActorRef universe) {
        this.universe = universe;
    }

    /**
     * Called by Akka when we receive a message
     */
    public void onReceive(Object message) {

        if (message instanceof BlockViewed) {
            BlockViewed blockPosition = (BlockViewed)message;
            onBlockViewed(blockPosition);
            return;
        }

        if (message instanceof ReceiveStack) {
            ReceiveStack receiveBlock = (ReceiveStack)message;
            onReceiveStack(receiveBlock);
            return;
        }

        if (message instanceof EventBlockRemoved) {
            EventBlockRemoved removedBlock = (EventBlockRemoved)message;
            onEventBlockRemoved(removedBlock);
            return;
        }

        if (message instanceof EventBlockUpdated) {
            EventBlockUpdated updatedBlock = (EventBlockUpdated)message;
            onEventBlockUpdated(updatedBlock);
            return;
        }

        if (message instanceof BoxQueryResult) {
            BoxQueryResult result = (BoxQueryResult)message;
            onBoxQueryResult(result);
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
     * This function is called when we receive a BlockViewed message.
     */
    public void onBlockViewed(BlockViewed blockPosition) {
        System.out.println("called onBlockViewed: not implemented");
    }

    /**
     * This function is called when we receive a ReceiveStack message.
     */
    public void onReceiveStack(ReceiveStack receiveBlock) {
        System.out.println("called onReceiveStack: not implemented");
    }

    /**
     * This function is called when we receive a BoxQueryResut
     */
    public void onBoxQueryResult(BoxQueryResult result) {
        System.out.println("called onBoxQueryResult: not implemented");
    }

    /**
     * Write a block to the world replacing any existing block
     * @param   position   The position at which the block will be written
     * @param   block      A collection of blocks.
     */
    public void replaceBlock(Position position, BlockTypeId block) {
        replaceBlock(position, block, BlockFilterFactory.empty());
    }

    /**
     * Write a block to the world
     * @param   position   The position at which the block will be written
     * @param   block      A collection of blocks.
     * @param   filter     Filter that defines what type of block that can be replaced
     */
    public void replaceBlock(Position position, BlockTypeId block, BlockFilter filter) {
        Map<Position, BlockTypeId> blocks = new HashMap<Position, BlockTypeId>();
        blocks.put(position, block);
        replaceBlocks(blocks, filter);
    }

    /**
     * Write a collection of blocks to the world replacing any existing blocks
     * @param   blocks      A collection of blocks.
     */
    public void replaceBlocks(Map<Position, BlockTypeId> blocks) {
        replaceBlocks(blocks, BlockFilterFactory.empty());
    }

    /**
     * Write a collection of blocks to the world.
     * @param   blocks      A collection of blocks.
     * @param   filter      Filter that defines what type of block that can be replaced
     */
    public void replaceBlocks(Map<Position, BlockTypeId> blocks, BlockFilter filter) {
        universe.tell(new ReplaceBlocks(filter, blocks), getSelf());
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
    public void viewBlock(Position p) {
        universe.tell(new ViewBlock(p), getSelf());
    }

    /** Query for a box of blocks
     *  @param start Starting corner of box
     *  @param end End corner of box
     */
    public void boxQuery(Position start, Position end) {
        universe.tell(new BoxQuery(new Box(start, end)), getSelf());
    }

    /**
     * Called when a block is removed.
     * @param block     The block
     */
    public void onEventBlockRemoved(EventBlockRemoved block) {
        System.out.println("called onEventBlockRemoved: not implemented");
    }

    /**
     * Called when a block is updated/created
     * @param block     The block
     */
    public void onEventBlockUpdated(EventBlockUpdated block) {
        System.out.println("called onEventBlockUpdated: not implemented");
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
