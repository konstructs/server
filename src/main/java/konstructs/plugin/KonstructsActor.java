package konstructs.plugin;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.TimeUnit;

import konstructs.api.messages.BoxQueryResult;
import scala.concurrent.duration.Duration;

import akka.actor.ActorRef;
import akka.actor.UntypedActorWithStash;
import konstructs.api.*;
import konstructs.api.messages.*;

public abstract class KonstructsActor extends UntypedActorWithStash {

    ActorRef universe;

    public KonstructsActor(ActorRef universe) {
        this.universe = universe;
    }

    /**
     * Called by Akka when we receive a message
     */
    public void onReceive(Object message) {

        if (message instanceof ViewBlockResult) {
            ViewBlockResult blockPosition = (ViewBlockResult)message;
            onViewBlockResult(blockPosition);
            return;
        }

        if (message instanceof ReceiveStack) {
            ReceiveStack receiveBlock = (ReceiveStack)message;
            onReceiveStack(receiveBlock);
            return;
        }

        if (message instanceof BlockUpdateEvent) {
            BlockUpdateEvent event = (BlockUpdateEvent)message;
            onBlockUpdateEvent(event);
            return;
        }

        if (message instanceof BoxQueryResult) {
            BoxQueryResult result = (BoxQueryResult)message;
            onBoxQueryResult(result);
            return;
        }

        unhandled(message);
    }

    /**
     * Return universe ActorRef.
     * @return ActorRef
     */
    public ActorRef getUniverse() {
        return universe;
    }

    /**
     * This function is called when we receive a ViewBlockResult message.
     */
    public void onViewBlockResult(ViewBlockResult blockPosition) {
        unhandled(blockPosition);
    }

    /**
     * This function is called when we receive a ReceiveStack message.
     */
    public void onReceiveStack(ReceiveStack receiveStack) {
        unhandled(receiveStack);
    }

    /**
     * This function is called when we receive a BoxQueryResult
     */
    public void onBoxQueryResult(BoxQueryResult result) {
        unhandled(result);
    }

    /**
     * Called when a block is updated/created
     * @param block     The block
     */
    public void onBlockUpdateEvent(BlockUpdateEvent event) {
        unhandled(event);
    }

    /**
     * Ask the server for a block
     * @param   p   The position
     */
    public void viewBlock(Position p) {
        universe.tell(new ViewBlock(p), getSelf());
    }

    /** Query for a box of blocks
     *  @param from Starting corner of box (this block is included)
     *  @param until End corner of box (this block is excluded)
     */
    public void boxQuery(Box box) {
        universe.tell(box, getSelf());
    }

    /**
     * Schedule a message to my self
     * @param   obj  The object to send
     * @param   msec Time to wait, in milliseconds
     */
    public void scheduleSelfOnce(Object obj, int msec) {
        scheduleOnce(obj, msec, getSelf());
    }

    /**
     * Schedule a message to another actor
     * @param   obj  The object to send
     * @param   msec Time to wait, in milliseconds
     * @param   to   The actor that will receive the message
     */
    public void scheduleOnce(Object obj, int msec, ActorRef to) {
        getContext().system().scheduler().scheduleOnce(
                Duration.create(msec, TimeUnit.MILLISECONDS),
                to, obj, getContext().system().dispatcher(), null);
    }

}
