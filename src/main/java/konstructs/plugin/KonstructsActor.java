package konstructs.plugin;

import akka.actor.ActorRef;
import akka.actor.UntypedActorWithStash;
import konstructs.api.*;
import konstructs.Box;

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
     * Write a collection of blocks to the world.
     * @param   blocks      A collection of blocks.
     */
    public void putBlocks(Collection<Placed<Block>> blocks) {
        for (Placed<Block> b : blocks) {
            putBlock(new PutBlock(b.position(), b.block()));
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
    public void viewBlock(Position p) {
        universe.tell(new ViewBlock(p), getSelf());
    }

    /**
     * Discard a block.
     * @param   p   The position
     */
    public void discardBlock(Position p) {
        universe.tell(new RemoveBlock(p), getSelf());
    }

    /**
     * Discard a collection of placed blocks.
     * (This method on use the position, not the block)
     * @param   blocks      A collection of blocks.
     */
    public void discardBlocks(Collection<Placed<Block>> blocks) {
        for (Placed<Block> b : blocks) {
            discardBlock(b.position());
        }
    }

    /**
     * Discard a collection of block positions.
     * @param   blocks      A collection of blocks.
     */
    public void discardPositions(Collection<Position> positions) {
        for (Position p : positions) {
            discardBlock(p);
        }
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
