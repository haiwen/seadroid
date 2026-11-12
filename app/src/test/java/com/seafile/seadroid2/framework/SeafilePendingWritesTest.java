package com.seafile.seadroid2.provider;

import android.Manifest;
import android.app.Application;
import android.content.pm.ProviderInfo;
import android.os.CancellationSignal;
import android.os.Looper;
import android.os.OperationCanceledException;
import android.widget.Toast;

import com.seafile.seadroid2.R;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.robolectric.RobolectricTestRunner;
import org.robolectric.RuntimeEnvironment;
import org.robolectric.annotation.Config;
import org.robolectric.annotation.LooperMode;
import org.robolectric.shadows.ShadowToast;

import java.io.IOException;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.CancellationException;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import static org.junit.Assert.*;
import static org.robolectric.Shadows.shadowOf;

@RunWith(RobolectricTestRunner.class)
@Config(sdk = 28, application = Application.class)
public class SeafilePendingWritesTest {
    private static final String DOCUMENT_A = "account::::repo::::/folder/a.txt";
    private static final String DOCUMENT_B = "account::::repo::::/folder/b.txt";

    private static SeafileProvider.PendingWrites providerWrites() throws Exception {
        SeafileProvider provider = new SeafileProvider() {
            @Override
            public boolean onCreate() {
                return true;
            }
        };
        ProviderInfo info = new ProviderInfo();
        info.authority = SeafileProvider.AUTHORITY_OF_DOCUMENTS;
        info.exported = true;
        info.grantUriPermissions = true;
        info.readPermission = Manifest.permission.MANAGE_DOCUMENTS;
        info.writePermission = Manifest.permission.MANAGE_DOCUMENTS;
        provider.attachInfo(RuntimeEnvironment.getApplication(), info);
        Field field = SeafileProvider.class.getDeclaredField("pendingWrites");
        field.setAccessible(true);
        return (SeafileProvider.PendingWrites) field.get(provider);
    }

    @Test
    @LooperMode(LooperMode.Mode.PAUSED)
    public void failureShowsToastOnMainThreadAndSuccessShowsNothing() throws Exception {
        SeafileProvider.PendingWrites writes = providerWrites();
        shadowOf(Looper.getMainLooper()).idle();
        ShadowToast.reset();
        SeafileProvider.PendingWrites.Write failed = writes.register(DOCUMENT_A);
        ExecutorService executor = Executors.newSingleThreadExecutor();
        try {
            executor.submit(() -> writes.finish(DOCUMENT_A, failed, new IOException())).get(2, TimeUnit.SECONDS);
            assertEquals(0, ShadowToast.shownToastCount());
            shadowOf(Looper.getMainLooper()).idle();
            assertEquals(1, ShadowToast.shownToastCount());
            assertEquals(RuntimeEnvironment.getApplication().getString(R.string.editor_file_save_failed),
                    ShadowToast.getTextOfLatestToast());
            assertEquals(Toast.LENGTH_LONG, ShadowToast.getLatestToast().getDuration());

            SeafileProvider.PendingWrites.Write retry = writes.register(DOCUMENT_A);
            executor.submit(() -> writes.finish(DOCUMENT_A, retry, null)).get(2, TimeUnit.SECONDS);
            shadowOf(Looper.getMainLooper()).idle();
            assertEquals(1, ShadowToast.shownToastCount());
            assertFalse(shadowOf(ShadowToast.getLatestToast()).isCancelled());
        } finally {
            executor.shutdownNow();
        }
    }

    @Test
    @LooperMode(LooperMode.Mode.PAUSED)
    public void queuedFailureIsSuppressedAfterRetryWithoutAffectingOtherDocuments() throws Exception {
        SeafileProvider.PendingWrites writes = providerWrites();
        shadowOf(Looper.getMainLooper()).idle();
        ShadowToast.reset();
        ExecutorService executor = Executors.newSingleThreadExecutor();
        try {
            SeafileProvider.PendingWrites.Write a = writes.register(DOCUMENT_A);
            SeafileProvider.PendingWrites.Write b = writes.register(DOCUMENT_B);
            executor.submit(() -> writes.finish(DOCUMENT_A, a, new IOException())).get(2, TimeUnit.SECONDS);
            executor.submit(() -> writes.finish(DOCUMENT_B, b, new IOException())).get(2, TimeUnit.SECONDS);
            SeafileProvider.PendingWrites.Write retry = writes.register(DOCUMENT_A);
            executor.submit(() -> writes.finish(DOCUMENT_A, retry, null)).get(2, TimeUnit.SECONDS);
            assertEquals(0, ShadowToast.shownToastCount());
            shadowOf(Looper.getMainLooper()).runOneTask();
            assertEquals(0, ShadowToast.shownToastCount());
            shadowOf(Looper.getMainLooper()).runOneTask();
            assertEquals(1, ShadowToast.shownToastCount());
            assertEquals(RuntimeEnvironment.getApplication().getString(R.string.editor_file_save_failed),
                    ShadowToast.getTextOfLatestToast());
            assertEquals(Toast.LENGTH_LONG, ShadowToast.getLatestToast().getDuration());
        } finally {
            executor.shutdownNow();
        }
    }

    @Test
    @LooperMode(LooperMode.Mode.PAUSED)
    public void laterFailureDoesNotReviveObsoleteQueuedToast() throws Exception {
        SeafileProvider.PendingWrites writes = providerWrites();
        shadowOf(Looper.getMainLooper()).idle();
        ShadowToast.reset();
        SeafileProvider.PendingWrites.Write first = writes.register(DOCUMENT_A);
        writes.finish(DOCUMENT_A, first, new IOException());
        SeafileProvider.PendingWrites.Write retry = writes.register(DOCUMENT_A);
        writes.finish(DOCUMENT_A, retry, null);
        SeafileProvider.PendingWrites.Write later = writes.register(DOCUMENT_A);
        writes.finish(DOCUMENT_A, later, new IOException());
        assertEquals(0, ShadowToast.shownToastCount());
        shadowOf(Looper.getMainLooper()).runOneTask();
        assertEquals(0, ShadowToast.shownToastCount());
        shadowOf(Looper.getMainLooper()).runOneTask();
        assertEquals(1, ShadowToast.shownToastCount());
        assertEquals(RuntimeEnvironment.getApplication().getString(R.string.editor_file_save_failed),
                ShadowToast.getTextOfLatestToast());
        assertEquals(Toast.LENGTH_LONG, ShadowToast.getLatestToast().getDuration());
    }

    @Test
    public void failedSpoolDoesNotCutOffPreviousWriteOrFailWaitingReader() throws Exception {
        List<String> events = new ArrayList<>();
        SeafileProvider.PendingWrites writes = new SeafileProvider.PendingWrites(
                (id, failed) -> events.add(id + ":" + failed));
        SeafileProvider.PendingWrites.Write a = writes.register("document");
        SeafileProvider.PendingWrites.Write b = writes.register("document");
        writes.finish("document", b, new IOException("spool failed"));
        assertTrue(writes.hasFailure("document"));
        assertEquals(Arrays.asList("document:true"), events);
        CompletableFuture<Void> waitingReader = writes.get("document");
        SeafileProvider.PendingWrites.Write c = writes.register("document");
        ExecutorService executor = Executors.newSingleThreadExecutor();
        try {
            Future<?> uploadC = executor.submit(c::awaitPrevious);
            assertFalse(b.completion.isDone());
            try {
                uploadC.get(100, TimeUnit.MILLISECONDS);
                fail("C must wait for A and B");
            } catch (TimeoutException expected) {
                assertFalse(waitingReader.isDone());
            }
            writes.finish("document", a, null);
            uploadC.get(2, TimeUnit.SECONDS);
            waitingReader.get(2, TimeUnit.SECONDS);
            assertTrue(writes.hasFailure("document"));
            assertSame(c.completion, writes.get("document"));
            writes.finish("document", c, null);
            c.completion.get(2, TimeUnit.SECONDS);
            assertNull(writes.get("document"));
            assertFalse(writes.hasFailure("document"));
            assertEquals(Arrays.asList("document:true", "document:false"), events);
        } finally {
            executor.shutdownNow();
        }
    }

    @Test
    public void olderFailureCannotReplaceLaterFailure() throws Exception {
        List<Boolean> events = new ArrayList<>();
        SeafileProvider.PendingWrites writes = new SeafileProvider.PendingWrites((id, failed) -> events.add(failed));
        SeafileProvider.PendingWrites.Write a = writes.register("document");
        SeafileProvider.PendingWrites.Write b = writes.register("document");
        writes.finish("document", b, new IOException("later failure"));
        assertTrue(writes.hasFailure("document"));
        assertFalse(b.completion.isDone());
        writes.finish("document", a, new IOException("older failure"));
        b.completion.get(2, TimeUnit.SECONDS);
        assertTrue(writes.hasFailure("document"));
        assertEquals(Arrays.asList(true), events);
        writes.await("document", () -> {}, 2, TimeUnit.SECONDS);
    }

    @Test
    public void failedWriteAllowsWaitingAndLaterReadsAndSuccessClearsFailure() throws Exception {
        List<Boolean> events = new ArrayList<>();
        SeafileProvider.PendingWrites writes = new SeafileProvider.PendingWrites((id, failed) -> events.add(failed));
        SeafileProvider.PendingWrites.Write write = writes.register("document");
        CompletableFuture<Void> reader = writes.get("document");
        writes.finish("document", write, new IOException("upload failed"));
        reader.get(2, TimeUnit.SECONDS);
        assertNull(writes.get("document"));
        assertTrue(writes.hasFailure("document"));
        SeafileProvider.PendingWrites.Write retry = writes.register("document");
        writes.finish("document", retry, null);
        retry.completion.get(2, TimeUnit.SECONDS);
        assertFalse(writes.hasFailure("document"));
        assertEquals(Arrays.asList(true, false), events);
    }

    @Test
    public void waitingReadSwitchesToNewerBarrierAfterFailure() throws Exception {
        SeafileProvider.PendingWrites writes = new SeafileProvider.PendingWrites((id, failed) -> {});
        SeafileProvider.PendingWrites.Write first = writes.register("document");
        CountDownLatch waiting = new CountDownLatch(1);
        ExecutorService executor = Executors.newSingleThreadExecutor();
        try {
            Future<?> reader = executor.submit(() -> {
                try {
                    writes.await("document", waiting::countDown, 2, TimeUnit.SECONDS);
                } catch (Exception e) {
                    throw new AssertionError(e);
                }
            });
            assertTrue(waiting.await(2, TimeUnit.SECONDS));
            SeafileProvider.PendingWrites.Write second = writes.register("document");
            writes.finish("document", first, new IOException());
            try {
                reader.get(100, TimeUnit.MILLISECONDS);
                fail("reader must wait for the newer write");
            } catch (TimeoutException expected) {
                assertTrue(writes.hasFailure("document"));
            }
            writes.finish("document", second, new IOException());
            reader.get(2, TimeUnit.SECONDS);
            assertTrue(writes.hasFailure("document"));
            writes.await("document", () -> {}, 2, TimeUnit.SECONDS);
        } finally {
            executor.shutdownNow();
        }
    }

    @Test
    public void waitingReadHonorsTimeoutAndCancellation() throws Exception {
        SeafileProvider.PendingWrites writes = new SeafileProvider.PendingWrites((id, failed) -> {});
        SeafileProvider.PendingWrites.Write write = writes.register("document");
        try {
            writes.await("document", () -> {}, 20, TimeUnit.MILLISECONDS);
            fail("waiting must time out");
        } catch (TimeoutException expected) {
            assertFalse(write.completion.isDone());
        }
        try {
            writes.await("document", () -> { throw new CancellationException(); }, 2, TimeUnit.SECONDS);
            fail("waiting must honor cancellation");
        } catch (CancellationException expected) {
            assertFalse(write.completion.isDone());
        }
        writes.finish("document", write, null);
    }

    @Test
    public void rejectedSubmissionPreservesBarrierAndRecordsFailureInOrder() throws Exception {
        List<Boolean> events = new ArrayList<>();
        SeafileProvider.PendingWrites writes = new SeafileProvider.PendingWrites((id, failed) -> events.add(failed));
        SeafileProvider.PendingWrites.Write a = writes.register("document");
        SeafileProvider.PendingWrites.Write rejected = writes.register("document");
        writes.finish("document", rejected, new RejectedExecutionException());
        assertSame(rejected.completion, writes.get("document"));
        assertFalse(rejected.completion.isDone());
        assertTrue(writes.hasFailure("document"));
        assertEquals(Arrays.asList(true), events);
        writes.finish("document", a, null);
        rejected.completion.get(2, TimeUnit.SECONDS);
        assertNull(writes.get("document"));
        assertTrue(writes.hasFailure("document"));
        assertEquals(Arrays.asList(true), events);
    }

    @Test
    public void unrelatedDocumentsAndFailuresAreIndependent() throws Exception {
        List<String> events = new ArrayList<>();
        SeafileProvider.PendingWrites writes = new SeafileProvider.PendingWrites(
                (id, failed) -> events.add(id + ":" + failed));
        SeafileProvider.PendingWrites.Write a = writes.register("a");
        SeafileProvider.PendingWrites.Write b = writes.register("b");
        b.awaitPrevious();
        writes.finish("b", b, new IOException());
        b.completion.get(2, TimeUnit.SECONDS);
        assertTrue(writes.hasFailure("b"));
        assertFalse(writes.hasFailure("a"));
        assertSame(a.completion, writes.get("a"));
        writes.finish("a", a, new IOException());
        SeafileProvider.PendingWrites.Write retry = writes.register("b");
        writes.finish("b", retry, null);
        retry.completion.get(2, TimeUnit.SECONDS);
        assertTrue(writes.hasFailure("a"));
        assertFalse(writes.hasFailure("b"));
        assertEquals(Arrays.asList("b:true", "a:true", "b:false"), events);
    }

    @Test
    public void laterSuccessCannotOvertakeFailureNotification() throws Exception {
        CountDownLatch notifyingFailure = new CountDownLatch(1);
        CountDownLatch releaseNotification = new CountDownLatch(1);
        List<Boolean> events = new ArrayList<>();
        SeafileProvider.PendingWrites writes = new SeafileProvider.PendingWrites((id, failed) -> {
            if (failed) {
                notifyingFailure.countDown();
                try {
                    if (!releaseNotification.await(2, TimeUnit.SECONDS)) {
                        throw new AssertionError("notification was not released");
                    }
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                    throw new AssertionError(e);
                }
            }
            events.add(failed);
        });
        SeafileProvider.PendingWrites.Write b = writes.register("document");
        SeafileProvider.PendingWrites.Write c = writes.register("document");
        ExecutorService executor = Executors.newFixedThreadPool(2);
        try {
            Future<?> finishB = executor.submit(() -> writes.finish("document", b, new IOException()));
            assertTrue(notifyingFailure.await(2, TimeUnit.SECONDS));
            Future<?> finishC = executor.submit(() -> writes.finish("document", c, null));
            try {
                finishC.get(100, TimeUnit.MILLISECONDS);
                fail("success must not overtake the failure notification");
            } catch (TimeoutException expected) {
                assertFalse(c.completion.isDone());
            }
            releaseNotification.countDown();
            finishB.get(2, TimeUnit.SECONDS);
            finishC.get(2, TimeUnit.SECONDS);
            c.completion.get(2, TimeUnit.SECONDS);
            assertFalse(writes.hasFailure("document"));
            assertEquals(Arrays.asList(true, false), events);
        } finally {
            releaseNotification.countDown();
            executor.shutdownNow();
        }
    }

    @Test
    public void cancelledUploadReportsUnsavedData() throws Exception {
        List<Boolean> events = new ArrayList<>();
        SeafileProvider.PendingWrites writes = new SeafileProvider.PendingWrites((id, failed) -> events.add(failed));
        SeafileProvider.PendingWrites.Write cancelled = writes.register("document");
        CancellationSignal signal = new CancellationSignal();
        signal.cancel();
        try {
            OpenDocumentWriter.uploadFileToCloud(null, null, null, null, null, null, signal);
            fail("cancelled upload must fail");
        } catch (OperationCanceledException error) {
            writes.finish("document", cancelled, error);
        }
        cancelled.completion.get(2, TimeUnit.SECONDS);
        assertTrue(writes.hasFailure("document"));
        assertEquals(Arrays.asList(true), events);
        writes.await("document", () -> {}, 2, TimeUnit.SECONDS);
    }

    @Test
    public void asynchronousThrowingNotificationDoesNotBreakChainedBarrier() throws Exception {
        CountDownLatch notified = new CountDownLatch(1);
        SeafileProvider.PendingWrites writes = new SeafileProvider.PendingWrites((id, failed) -> {
            if (failed) {
                notified.countDown();
                throw new SecurityException("notifications denied");
            }
        });
        SeafileProvider.PendingWrites.Write a = writes.register("document");
        SeafileProvider.PendingWrites.Write b = writes.register("document");
        ExecutorService executor = Executors.newSingleThreadExecutor();
        try {
            Future<?> failed = executor.submit(() -> writes.finish("document", b, new IOException()));
            assertTrue(notified.await(2, TimeUnit.SECONDS));
            failed.get(2, TimeUnit.SECONDS);
            assertTrue(writes.hasFailure("document"));
            assertFalse(b.completion.isDone());
            writes.finish("document", a, null);
            b.completion.get(2, TimeUnit.SECONDS);
            assertTrue(writes.hasFailure("document"));
            writes.await("document", () -> {}, 2, TimeUnit.SECONDS);
        } finally {
            executor.shutdownNow();
        }
    }

    @Test
    public void throwingNotificationCallbackDoesNotBreakBarrier() throws Exception {
        SeafileProvider.PendingWrites writes = new SeafileProvider.PendingWrites((id, failed) -> {
            throw new SecurityException("notifications denied");
        });
        SeafileProvider.PendingWrites.Write failed = writes.register("document");
        SeafileProvider.PendingWrites.Write retry = writes.register("document");
        writes.finish("document", failed, new IOException());
        failed.completion.get(2, TimeUnit.SECONDS);
        assertTrue(writes.hasFailure("document"));
        retry.awaitPrevious();
        writes.finish("document", retry, null);
        retry.completion.get(2, TimeUnit.SECONDS);
        assertFalse(writes.hasFailure("document"));
        assertNull(writes.get("document"));
    }
}
