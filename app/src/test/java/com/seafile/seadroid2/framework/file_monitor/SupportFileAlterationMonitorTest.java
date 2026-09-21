package com.seafile.seadroid2.framework.file_monitor;

import static org.junit.Assert.*;

import com.elvishew.xlog.LogConfiguration;
import com.elvishew.xlog.XLog;

import org.apache.commons.io.monitor.FileAlterationObserver;
import org.junit.BeforeClass;
import org.junit.Test;

import java.io.File;
import java.io.IOException;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

public class SupportFileAlterationMonitorTest {
    @BeforeClass
    public static void initLogging() {
        XLog.init(new LogConfiguration.Builder().build(), (level, tag, message) -> {});
    }

    @Test
    public void failedInitializationIsRetriedWithoutKillingThread() throws Exception {
        AtomicInteger attempts = new AtomicInteger();
        CountDownLatch checked = new CountDownLatch(1);
        FileAlterationObserver observer = new FileAlterationObserver(new File(".")) {
            @Override public void initialize() throws Exception {
                if (attempts.incrementAndGet() == 1) throw new IOException("Storage unavailable");
            }
            @Override public void checkAndNotify() { checked.countDown(); }
        };
        AtomicReference<Throwable> uncaught = new AtomicReference<>();
        SupportFileAlterationMonitor monitor = monitor(uncaught, observer);
        try {
            monitor.start();
            assertTrue("Monitor must retry initialization", checked.await(3, TimeUnit.SECONDS));
            assertEquals(2, attempts.get());
        } finally {
            monitor.stop(3000);
        }
        assertNull(uncaught.get());
    }

    @Test
    public void failingFolderDoesNotStopOtherFoldersOrLaterPolls() throws Exception {
        CountDownLatch healthyChecks = new CountDownLatch(3);
        AtomicInteger failingChecks = new AtomicInteger();
        FileAlterationObserver failing = new FileAlterationObserver(new File(".")) {
            @Override public void initialize() {}
            @Override public void checkAndNotify() {
                failingChecks.incrementAndGet();
                throw new SecurityException("Permission revoked");
            }
        };
        FileAlterationObserver healthy = new FileAlterationObserver(new File(".")) {
            @Override public void initialize() {}
            @Override public void checkAndNotify() { healthyChecks.countDown(); }
        };
        AtomicReference<Throwable> uncaught = new AtomicReference<>();
        SupportFileAlterationMonitor monitor = monitor(uncaught, failing, healthy);
        try {
            monitor.start();
            assertTrue(healthyChecks.await(3, TimeUnit.SECONDS));
            assertTrue(failingChecks.get() >= 3);
        } finally {
            monitor.stop(3000);
        }
        assertNull(uncaught.get());
    }

    @Test
    public void stopNeverDestroysObserverWhileItIsPolling() throws Exception {
        CountDownLatch checking = new CountDownLatch(1);
        CountDownLatch release = new CountDownLatch(1);
        CountDownLatch destroyed = new CountDownLatch(1);
        AtomicBoolean inCheck = new AtomicBoolean();
        AtomicBoolean destroyedDuringCheck = new AtomicBoolean();
        FileAlterationObserver observer = new FileAlterationObserver(new File(".")) {
            @Override public void initialize() {}
            @Override public void checkAndNotify() {
                inCheck.set(true);
                checking.countDown();
                boolean released = false;
                while (!released) {
                    try { released = release.await(3, TimeUnit.SECONDS); }
                    catch (InterruptedException ignored) { /* Simulate non-interruptible I/O. */ }
                }
                inCheck.set(false);
            }
            @Override public void destroy() {
                destroyedDuringCheck.set(inCheck.get());
                destroyed.countDown();
            }
        };
        AtomicReference<Throwable> uncaught = new AtomicReference<>();
        SupportFileAlterationMonitor monitor = monitor(uncaught, observer);
        try {
            monitor.start();
            assertTrue(checking.await(3, TimeUnit.SECONDS));
            monitor.stop(20);
            assertEquals("Cleanup must wait for polling to finish", 1, destroyed.getCount());
            assertThrows(IllegalStateException.class, monitor::start);
        } finally {
            release.countDown();
            monitor.stop(3000);
        }
        assertTrue(destroyed.await(3, TimeUnit.SECONDS));
        assertFalse(destroyedDuringCheck.get());
        assertNull(uncaught.get());
    }

    private SupportFileAlterationMonitor monitor(AtomicReference<Throwable> uncaught,
                                                FileAlterationObserver... observers) {
        SupportFileAlterationMonitor monitor = new SupportFileAlterationMonitor(10, observers);
        monitor.setThreadFactory(task -> {
            Thread thread = new Thread(task, "file-monitor-test");
            thread.setDaemon(true);
            thread.setUncaughtExceptionHandler((t, error) -> uncaught.set(error));
            return thread;
        });
        return monitor;
    }
}
