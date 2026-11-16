package com.seafile.seadroid2.framework.service;

import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.enums.FeatureDataSource;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.robolectric.RobolectricTestRunner;
import org.robolectric.annotation.Config;
import org.robolectric.util.ReflectionHelpers;

import java.lang.reflect.Constructor;
import java.util.Map;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

@RunWith(RobolectricTestRunner.class)
@Config(sdk = 28, application = BackupThreadExecutorTest.TestApplication.class)
public class BackupThreadExecutorTest {
    private BackupThreadExecutor executor;
    private Map<FeatureDataSource, Object> running;

    public static class TestApplication extends SeadroidApplication {
        @Override
        public void onCreate() {
            // 保留应用上下文，但不启动生产环境服务。
        }
    }

    @Before
    public void setUp() {
        executor = ReflectionHelpers.callConstructor(BackupThreadExecutor.class);
        running = ReflectionHelpers.getField(executor, "running");
    }

    @Test
    public void repeatedAlbumStartsDoNotRepeatOrUpgradeFullScan() throws Exception {
        Object run = injectRunning(FeatureDataSource.ALBUM_BACKUP);
        RunRequests requests = ReflectionHelpers.getField(run, "requests");

        executor.runAlbumBackupTask(false);
        executor.runAlbumBackupTask(true);
        executor.runAlbumBackupTask(false);

        assertSame(run, running.get(FeatureDataSource.ALBUM_BACKUP));
        assertTrue(executor.isAlbumBackupRunning());
        assertFalse(requests.isFullScan());
        assertFalse(requests.takeRepeat());
        assertNull(requests.takeRestart());
    }

    @Test
    public void repeatedAlbumStartsDuringCancellationDoNotRequestRestart() throws Exception {
        Object run = injectRunning(FeatureDataSource.ALBUM_BACKUP);
        RunRequests requests = ReflectionHelpers.getField(run, "requests");
        ReflectionHelpers.callInstanceMethod(run, "cancel");

        executor.runAlbumBackupTask(false);
        executor.runAlbumBackupTask(true);

        assertSame(run, running.get(FeatureDataSource.ALBUM_BACKUP));
        assertTrue(requests.isCancelled());
        assertFalse(requests.isFullScan());
        assertFalse(requests.takeRepeat());
        assertNull(requests.takeRestart());
    }

    @Test
    public void repeatedAlbumStartAfterQueueDrainedDoesNotRequestRestart() throws Exception {
        Object run = injectRunning(FeatureDataSource.ALBUM_BACKUP);
        RunRequests requests = ReflectionHelpers.getField(run, "requests");
        assertFalse(requests.takeRepeat());

        executor.runAlbumBackupTask(true);

        assertSame(run, running.get(FeatureDataSource.ALBUM_BACKUP));
        assertFalse(requests.isFullScan());
        assertFalse(requests.takeRepeat());
        assertNull(requests.takeRestart());
    }

    @Test
    public void repeatedFolderStartsStillMergeFullScanAndRepeat() throws Exception {
        Object run = injectRunning(FeatureDataSource.FOLDER_BACKUP);
        RunRequests requests = ReflectionHelpers.getField(run, "requests");

        executor.runFolderBackupFuture(false);
        executor.runFolderBackupFuture(true);
        executor.runFolderBackupFuture(false);

        assertSame(run, running.get(FeatureDataSource.FOLDER_BACKUP));
        assertTrue(requests.isFullScan());
        assertTrue(requests.takeRepeat());
        assertFalse(requests.takeRepeat());
        assertNull(requests.takeRestart());
    }

    private Object injectRunning(FeatureDataSource source) throws Exception {
        Class<?> runClass = Class.forName(BackupThreadExecutor.class.getName() + "$Run");
        Constructor<?> constructor = runClass.getDeclaredConstructor(ParentEventTransfer.class);
        constructor.setAccessible(true);
        Object run = constructor.newInstance((Object) null);
        ReflectionHelpers.callInstanceMethod(run, "trigger",
                ReflectionHelpers.ClassParameter.from(boolean.class, false));
        RunRequests requests = ReflectionHelpers.getField(run, "requests");
        assertTrue(requests.takeRepeat());
        running.put(source, run);
        return run;
    }
}
