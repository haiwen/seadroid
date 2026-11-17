package com.seafile.seadroid2.framework.service;

import org.junit.Test;
import static org.junit.Assert.*;

public class RunRequestsTest {
    @Test public void triggerDuringCancellationWaitsForOwnerExitAndMergesFullScan() {
        RunRequests old = new RunRequests();
        old.trigger(false);
        assertTrue(old.takeRepeat());
        old.cancel();
        old.trigger(false);
        old.trigger(true);
        old.trigger(false);
        assertFalse(old.takeRepeat()); // old owner cannot do more IO
        assertTrue(old.isCancelled());
        // Only finish consumes this after synchronous IO has exited.
        assertEquals(Boolean.TRUE, old.takeRestart());
        assertNull(old.takeRestart());
    }

    @Test public void secondStopCancelsPendingRestartButLaterTriggerIsAccepted() {
        RunRequests old = new RunRequests();
        old.cancel();
        old.trigger(true);
        old.cancel();
        assertNull(old.takeRestart());
        old.trigger(false);
        assertEquals(Boolean.FALSE, old.takeRestart());
    }

    @Test public void triggerBetweenDrainedQueueAndFinallyStartsNextGeneration() {
        RunRequests old = new RunRequests();
        assertFalse(old.takeRepeat());
        old.trigger(true);
        assertFalse(old.takeRepeat());
        assertEquals(Boolean.TRUE, old.takeRestart());
    }

    @Test public void liveTriggersAreCoalescedInCurrentGeneration() {
        RunRequests run = new RunRequests();
        run.trigger(false);
        run.trigger(true);
        assertTrue(run.takeRepeat());
        assertTrue(run.isFullScan());
        assertFalse(run.takeRepeat());
        assertNull(run.takeRestart());
    }
}
