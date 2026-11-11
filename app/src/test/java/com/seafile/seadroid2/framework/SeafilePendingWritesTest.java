package com.seafile.seadroid2.provider;

import org.junit.Test;

import java.io.IOException;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import static org.junit.Assert.*;

public class SeafilePendingWritesTest {
    @Test
    public void failedSpoolDoesNotCutOffPreviousWrite() throws Exception {
        SeafileProvider.PendingWrites writes = new SeafileProvider.PendingWrites();
        SeafileProvider.PendingWrites.Write a = writes.register("document");
        SeafileProvider.PendingWrites.Write b = writes.register("document");
        IOException failure = new IOException("spool failed");
        writes.finish("document", b, failure);
        CompletableFuture<Void> waitingReader = writes.get("document");
        SeafileProvider.PendingWrites.Write c = writes.register("document");
        ExecutorService executor = Executors.newSingleThreadExecutor();
        try {
            Future<?> uploadC = executor.submit(c::awaitPrevious);
            assertFalse(b.completion.isDone());
            try {
                uploadC.get(100, TimeUnit.MILLISECONDS);
                fail("C must wait for A");
            } catch (TimeoutException expected) {
                assertFalse(waitingReader.isDone());
            }
            writes.finish("document", a, null);
            uploadC.get(2, TimeUnit.SECONDS);
            try {
                waitingReader.get(2, TimeUnit.SECONDS);
                fail("reader waiting on B must see its failure");
            } catch (ExecutionException expected) {
                assertSame(failure, expected.getCause());
            }
            assertSame(c.completion, writes.get("document"));
            writes.finish("document", c, null);
            assertNull(writes.get("document"));
        } finally {
            executor.shutdownNow();
        }
    }

    @Test
    public void failedWriteIsRemovedAndDoesNotBlockFutureReads() throws Exception {
        SeafileProvider.PendingWrites writes = new SeafileProvider.PendingWrites();
        SeafileProvider.PendingWrites.Write write = writes.register("document");
        CompletableFuture<Void> reader = writes.get("document");
        IOException failure = new IOException("upload failed");
        writes.finish("document", write, failure);
        try {
            reader.get();
            fail("waiting reader must see the failure");
        } catch (ExecutionException expected) {
            assertSame(failure, expected.getCause());
        }
        assertNull(writes.get("document"));
    }

    @Test
    public void rejectedSubmissionRetainsAndEventuallyCleansBarrier() {
        SeafileProvider.PendingWrites writes = new SeafileProvider.PendingWrites();
        SeafileProvider.PendingWrites.Write a = writes.register("document");
        SeafileProvider.PendingWrites.Write rejected = writes.register("document");
        writes.finish("document", rejected, new IllegalStateException("rejected"));
        assertSame(rejected.completion, writes.get("document"));
        assertFalse(rejected.completion.isDone());
        writes.finish("document", a, null);
        assertTrue(rejected.completion.isCompletedExceptionally());
        assertNull(writes.get("document"));
    }

    @Test
    public void unrelatedDocumentsCompleteIndependently() {
        SeafileProvider.PendingWrites writes = new SeafileProvider.PendingWrites();
        SeafileProvider.PendingWrites.Write a = writes.register("a");
        SeafileProvider.PendingWrites.Write b = writes.register("b");
        b.awaitPrevious();
        writes.finish("b", b, null);
        assertNull(writes.get("b"));
        assertSame(a.completion, writes.get("a"));
        assertFalse(a.completion.isDone());
        writes.finish("a", a, null);
        assertNull(writes.get("a"));
    }
}
