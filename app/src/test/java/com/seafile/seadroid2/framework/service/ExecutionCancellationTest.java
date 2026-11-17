package com.seafile.seadroid2.framework.service;

import org.junit.Test;
import java.util.concurrent.CancellationException;
import static org.junit.Assert.*;

public class ExecutionCancellationTest {
    @Test public void cancellationIsOwnerScopedAndClearedBetweenExecutions() {
        ExecutionCancellation worker = new ExecutionCancellation();
        ExecutionCancellation ui = new ExecutionCancellation();
        worker.cancel();
        ExecutionCancellation.enter(ui);
        try { ExecutionCancellation.check(); } finally { ExecutionCancellation.leave(); }
        ExecutionCancellation.enter(worker);
        try { ExecutionCancellation.check(); fail(); }
        catch (CancellationException expected) { }
        finally { ExecutionCancellation.leave(); }
        ExecutionCancellation.check();
    }
    @Test public void postScanWaitCanBeCancelledBeforeUpload() throws Exception {
        ExecutionCancellation token = new ExecutionCancellation();
        java.util.concurrent.CountDownLatch scanningFinished = new java.util.concurrent.CountDownLatch(1);
        java.util.concurrent.atomic.AtomicBoolean uploaded = new java.util.concurrent.atomic.AtomicBoolean();
        java.util.concurrent.atomic.AtomicBoolean cancelled = new java.util.concurrent.atomic.AtomicBoolean();
        java.util.concurrent.ExecutorService executor = java.util.concurrent.Executors.newSingleThreadExecutor();
        try {
            java.util.concurrent.Future<?> chain = executor.submit(() -> {
                ExecutionCancellation.enter(token);
                try {
                    scanningFinished.countDown();
                    ExecutionCancellation.await(60000);
                    uploaded.set(true);
                } catch (CancellationException expected) {
                    cancelled.set(true);
                } finally { ExecutionCancellation.leave(); }
            });
            assertTrue(scanningFinished.await(2, java.util.concurrent.TimeUnit.SECONDS));
            token.cancel();
            chain.get(2, java.util.concurrent.TimeUnit.SECONDS);
            assertTrue(cancelled.get());
            assertFalse(uploaded.get());
        } finally { executor.shutdownNow(); }
    }

    @Test public void waitWithoutCancellationCompletes() {
        ExecutionCancellation token = new ExecutionCancellation();
        ExecutionCancellation.enter(token);
        try { ExecutionCancellation.await(1); }
        finally { ExecutionCancellation.leave(); }
    }

    @Test public void repeatedCancellationIsIdempotent() {
        ExecutionCancellation token = new ExecutionCancellation();
        ExecutionCancellation.enter(token);
        try {
            token.cancel(); token.cancel();
            try { ExecutionCancellation.check(); fail(); }
            catch (CancellationException expected) { }
        } finally { ExecutionCancellation.leave(); }
    }
}
