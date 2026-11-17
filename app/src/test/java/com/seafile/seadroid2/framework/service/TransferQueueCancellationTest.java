package com.seafile.seadroid2.framework.service;

import com.seafile.seadroid2.framework.worker.queue.TransferModel;
import com.seafile.seadroid2.framework.worker.queue.TransferQueue;
import org.junit.Test;
import java.util.concurrent.CancellationException;
import static org.junit.Assert.*;

public class TransferQueueCancellationTest {
    private TransferModel model(String id) {
        TransferModel model = new TransferModel();
        model.setId(id);
        return model;
    }

    @Test public void stopAllClearPreservesNewSubmissionAndRejectsLateScanAndCleanup() {
        for (boolean categorized : new boolean[] {false, true}) {
            TransferQueue queue = new TransferQueue(categorized);
            TransferModel old = model("same-id");
            if (categorized) queue.put("camera", old); else queue.put(old);
            ExecutionCancellation token = new ExecutionCancellation();
            token.cancel(); // stopAll cancels the owner before clearing
            queue.clear();
            TransferModel next = model("same-id");
            if (categorized) queue.put("camera", next); else queue.put(next);
            ExecutionCancellation.enter(token);
            try {
                try {
                    if (categorized) queue.put("camera", old); else queue.put(old);
                    fail("Old scan must not admit models after stopAll");
                } catch (CancellationException expected) { }
                try { queue.clear(); fail("Late scan-error cleanup must not clear new queue"); }
                catch (CancellationException expected) { }
                try { queue.remove(old); fail("Old scan must not prune new submissions"); }
                catch (CancellationException expected) { }
                try { queue.pick(); fail("Old upload must not consume new submissions"); }
                catch (CancellationException expected) { }
                queue.retryInterrupted(old); // must not resurrect or modify the next submission
            } finally { ExecutionCancellation.leave(); }
            assertSame(next, queue.getById("same-id"));
            assertEquals(1, queue.getPendingCount());
            assertSame(next, queue.pick());
            queue.retryInterrupted(old);
            assertTrue(queue.isQueueEmpty());
            queue.retryInterrupted(next);
            assertEquals(1, queue.getPendingCount());
        }
    }
}
