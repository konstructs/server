package konstructs.plugin;

import akka.actor.ActorRef;
import akka.actor.UntypedActorWithStash;
import konstructs.api.*;

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
        }

        if (message instanceof EventBlockUpdated) {
            EventBlockUpdated updatedBlock = (EventBlockUpdated)message;
            onEventBlockUpdated(updatedBlock);
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
    public void viewBlock(Position p) {
        universe.tell(new ViewBlock(p), getSelf());
    }

    /**
     * Discard a block.
     * @param   p   The position
     */
    public void discardBlock(Position p) {
        universe.tell(new DiscardBlock(p), getSelf());
    }

    /**
     * Discard a collection of blocks.
     * @param   blocks      A collection of blocks.
     */
    public void discardBlocks(Collection<PutBlock> blocks) {
        for (PutBlock b : blocks) {
            discardBlock(b.pos());
        }
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
