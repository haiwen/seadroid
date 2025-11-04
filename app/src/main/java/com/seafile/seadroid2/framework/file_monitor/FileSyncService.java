package com.seafile.seadroid2.framework.file_monitor;

import static android.app.PendingIntent.FLAG_IMMUTABLE;

import android.app.Notification;
import android.app.NotificationChannel;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.app.Service;
import android.content.Context;
import android.content.Intent;
import android.os.Binder;
import android.os.Handler;
import android.os.IBinder;
import android.os.Looper;
import android.text.TextUtils;

import androidx.annotation.Nullable;
import androidx.core.app.NotificationCompat;
import androidx.lifecycle.Observer;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.EncryptUtils;
import com.blankj.utilcode.util.FileUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.bus.BusHelper;
import com.seafile.seadroid2.enums.FeatureDataSource;
import com.seafile.seadroid2.framework.datastore.DataManager;
import com.seafile.seadroid2.framework.datastore.StorageManager;
import com.seafile.seadroid2.framework.datastore.sp_livedata.AlbumBackupSharePreferenceHelper;
import com.seafile.seadroid2.framework.datastore.sp_livedata.FolderBackupSharePreferenceHelper;
import com.seafile.seadroid2.framework.db.AppDatabase;
import com.seafile.seadroid2.framework.db.entities.FileCacheStatusEntity;
import com.seafile.seadroid2.framework.notification.base.NotificationUtils;
import com.seafile.seadroid2.framework.service.BackupThreadExecutor;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.framework.worker.BackgroundJobManagerImpl;
import com.seafile.seadroid2.framework.worker.GlobalTransferCacheList;
import com.seafile.seadroid2.framework.worker.queue.TransferModel;
import com.seafile.seadroid2.preferences.Settings;
import com.seafile.seadroid2.ui.camera_upload.CameraUploadManager;
import com.seafile.seadroid2.ui.main.MainActivity;

import org.apache.commons.io.monitor.FileAlterationListener;
import org.apache.commons.io.monitor.FileAlterationObserver;

import java.io.File;
import java.io.FileFilter;
import java.lang.ref.WeakReference;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.Executor;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;

/**
 * daemon service
 */
public class FileSyncService extends Service {
    private final String TAG = "FileSyncService";
    private MediaContentObserver mediaContentObserver;
    private SupportFileAlterationMonitor fileMonitor;

    /**
     * <p>
     * Since the file download location of the app is located in the <b>/Android/</b> directory,
     * special monitoring of this folder is required. When a change event occurs in the files in this folder,
     * need to re-upload it, because the user may have edited the cached text file in the app.
     * </p>
     *
     * <pre>
     * {@code
     * /storage/emulated/0/Android/media/com.seafile.seadroid2(.debug)/Seafile
     * /storage/emulated/0/Android/media/com.seafile.seadroid2(.debug)
     * /storage/emulated/0/Android/media
     * /storage/emulated/0/Android
     * }
     * </pre>
     */
    private final List<String> IGNORE_PATHS = new ArrayList<>();

    private final String TEMP_FILE_DIR = StorageManager.getInstance().getTempDir().getAbsolutePath();

    private final Handler mainThreadHandler = new Handler(Looper.getMainLooper());

    /**
     * TODO: Multiple cache directories
     */
    public void initIgnorePath() {
        if (!CollectionUtils.isEmpty(IGNORE_PATHS)) {
            return;
        }

        // /storage/emulated/0/Android/media/com.seafile.seadroid2(.debug)/Seafile
        String path = StorageManager.getInstance().getMediaDir().getAbsolutePath();
        IGNORE_PATHS.add(path);

        // /storage/emulated/0/Android/media/com.seafile.seadroid2
        String appCacheDir = Utils.getParentPath(path);
        IGNORE_PATHS.add(appCacheDir);

        // /storage/emulated/0/Android/media
        String mediaCacheDir = Utils.getParentPath(appCacheDir);
        IGNORE_PATHS.add(mediaCacheDir);

        // /storage/emulated/0/Android
        String androidCacheDir = Utils.getParentPath(mediaCacheDir);
        IGNORE_PATHS.add(androidCacheDir);

    }

    @Override
    public void onCreate() {
        super.onCreate();

        initIgnorePath();

        registerMediaContentObserver();

        startWorkers();

        compareFirstAndStartMonitor();
    }

    @Override
    public int onStartCommand(Intent intent, int flags, int startId) {

        boolean isTurnOn = Settings.BACKGROUND_BACKUP_SWITCH.queryValue();
        if (isTurnOn) {
            SLogs.d(TAG, "Background backup enabled, running in background by foreground service.");
            startForeground(NotificationUtils.NID_FILE_MONITOR_PERSISTENTLY, buildNotification());
        } else {
            SLogs.d(TAG, "Background backup disabled, running in bound mode.");
        }

        //
        startPeriodicScanTask();

        return START_STICKY;
    }

    @Nullable
    @Override
    public IBinder onBind(Intent intent) {
        boolean isTurnOn = Settings.BACKGROUND_BACKUP_SWITCH.queryValue();
        if (isTurnOn) {
            return null;
        }

        if (mBinder == null) {
            mBinder = new FileSyncService.FileSyncBinder(this);
        }
        return mBinder;
    }

    private IBinder mBinder = null;

    public static class FileSyncBinder extends Binder {
        private final WeakReference<FileSyncService> serviceRef;

        public FileSyncBinder(FileSyncService service) {
            this.serviceRef = new WeakReference<>(service);
        }

        public FileSyncService getService() {
            return serviceRef.get();
        }
    }


    private Notification buildNotification() {

        Intent intent = new Intent(this, MainActivity.class);
        PendingIntent pendingIntent = PendingIntent.getActivity(this, 1, intent, FLAG_IMMUTABLE);

        return new NotificationCompat.Builder(this, NotificationUtils.FILE_MONITOR_CHANNEL)
                .setSmallIcon(R.drawable.icon)
                .setContentTitle(getString(R.string.notification_background_file_monitor_title))
                .setContentText(getString(R.string.notification_background_file_monitor_content))
                .setOngoing(true)
                .setAutoCancel(false)
                .setPriority(NotificationCompat.PRIORITY_MAX)
                .setCategory(Notification.CATEGORY_SERVICE)
                .setVisibility(NotificationCompat.VISIBILITY_PUBLIC)
                .setContentIntent(pendingIntent)
                .build();
    }

    public void updateForegroundState(boolean isTurnOn) {
        if (isTurnOn) {
            startForeground(NotificationUtils.NID_FILE_MONITOR_PERSISTENTLY, buildNotification());
            SLogs.d(TAG, "Entered foreground mode");
        } else {
            stopForeground(true);
            SLogs.d(TAG, "Exited foreground mode");
        }
    }

    // 使用 Handler + Runnable 实现定时任务
    private final Handler periodicHandler = new Handler(Looper.getMainLooper());
    private boolean isPeriodicRunning = false;
    private final long periodicTime = 30 * 60 * 1000L;

    private final Runnable periodicTask = new Runnable() {
        @Override
        public void run() {
            if (isPeriodicRunning) {
                SLogs.d(TAG, "Periodic scan skipped (previous still running)");
                periodicHandler.postDelayed(this, periodicTime);
                return;
            }
            isPeriodicRunning = true;

            try {
                SLogs.d(TAG, "Periodic local file scan...");
                startWorkers();
            } catch (Exception e) {
                SLogs.e(TAG, "Periodic scan failed", e);
            } finally {
                // 每 30 分钟再次执行
                isPeriodicRunning = false;
                periodicHandler.postDelayed(this, periodicTime);
            }
        }
    };

    private void startPeriodicScanTask() {
        periodicHandler.removeCallbacks(periodicTask);

        periodicHandler.postDelayed(periodicTask, periodicTime);
    }

    /**
     * main thread executor
     */
    private final Executor mainThreadExecutor = new Executor() {
        private final Handler handler = new Handler(Looper.getMainLooper());

        @Override
        public void execute(Runnable command) {
            handler.post(command);
        }
    };

    private void compareFirstAndStartMonitor() {
        CompletableFuture.runAsync(() -> {
            Account account = SupportAccountManager.getInstance().getCurrentAccount();
            if (account == null) {
                SLogs.d(TAG, "scanLocalCacheFile", "account is null");
                return;
            }

            List<FileCacheStatusEntity> cacheList = AppDatabase.getInstance().fileCacheStatusDAO().getByAccountSync(account.getSignature());

            if (CollectionUtils.isEmpty(cacheList)) {
                return;
            }

            SLogs.d(TAG, "scanLocalCacheFile: cacheList size is " + cacheList.size());

            for (FileCacheStatusEntity entity : cacheList) {
                File file = new File(entity.target_path);
                if (!file.exists()) {
                    SLogs.d(TAG, "scanLocalCacheFile", "local file not exists, path: " + entity.target_path);
                    continue;
                }

                if (entity.file_md5 == null) {
                    SLogs.d(TAG, "scanLocalCacheFile", "file_md5 is null, path: " + entity.target_path);
                    continue;
                }

                String localMD5 = FileUtils.getFileMD5ToString(file).toLowerCase();
                if (TextUtils.equals(entity.file_md5, localMD5)) {
                    SLogs.d(TAG, "scanLocalCacheFile", "skip: local file md5 is same, path: " + entity.full_path);
                    continue;
                }

                SLogs.d(TAG, "scanLocalCacheFile", "file md5 is different, path: " + entity.full_path);

                onAppCacheFileChanged(file);
            }

            //When the Sync service is first launched, we can start another foreground service directly because the app is being visible to the user
            BackupThreadExecutor.getInstance().runLocalFileUpdateTask();
        }).thenRunAsync(new Runnable() {
            @Override
            public void run() {
                //start local path and app cache path listener
                restartFolderMonitor();
            }
        }, mainThreadExecutor);
    }

    private void registerMediaContentObserver() {
        mediaContentObserver = new MediaContentObserver(getBaseContext(), mainThreadHandler);
        mediaContentObserver.setOnMediaContentObserverListener(new MediaContentObserver.OnMediaContentObserverListener() {
            @Override
            public void onMediaContentObserver(boolean isFullScan) {
                SLogs.d(TAG, "onMediaContentObserver", "isFullScan: " + isFullScan);
                CameraUploadManager.getInstance().performSync(isFullScan);
            }
        });
        mediaContentObserver.register();
    }

    private void startWorkers() {
        if (AlbumBackupSharePreferenceHelper.isAlbumBackupEnable()) {
            CameraUploadManager.getInstance().performSync();
        }

        if (FolderBackupSharePreferenceHelper.isFolderBackupEnable()) {
            BackupThreadExecutor.getInstance().runFolderBackupFuture(true);
        }
    }

    @Override
    public void onTaskRemoved(Intent rootIntent) {
        super.onTaskRemoved(rootIntent);
        destroy();
    }

    private final FileFilter FILE_FILTER = file -> {
        if (file.getAbsolutePath().startsWith(TEMP_FILE_DIR)) {
            return false;
        }

        final String fileName = file.getName();

        if (FolderBackupSharePreferenceHelper.isFolderBackupSkipHiddenFiles()) {
            return !fileName.startsWith(".");
        }

        return true;
    };


    public void restartFolderMonitor() {
        startFolderMonitor();
    }

    private void stopFolderMonitor() {
        if (fileMonitor != null) {
            try {
                fileMonitor.stop();
            } catch (Exception e) {
                SLogs.w("FileSyncService", e);
            } finally {
                fileMonitor = null;
            }
        }
    }

    private void startFolderMonitor() {
        List<String> pathList = initFolderMonitorPath();

        if (CollectionUtils.isEmpty(pathList)) {
            return;
        }

        try {
            List<FileAlterationObserver> observerList = new ArrayList<>();
            for (String str : pathList) {
                boolean isExists = FileUtils.isFileExists(str);
                if (!isExists) {
                    SLogs.d(TAG, "startFolderMonitor()", "backup path not exists: " + str);
                    continue;
                }

                SLogs.d(TAG, "startFolderMonitor()", "backup path: " + str);
                FileAlterationObserver observer = new FileAlterationObserver(str, FILE_FILTER);

                observer.addListener(new FileSyncService.FolderStateChangedListener());
                observerList.add(observer);
            }

            //stopIfRunning
            if (fileMonitor != null) {
                fileMonitor.stopIfRunning();
            }

            fileMonitor = new SupportFileAlterationMonitor(5000L, observerList);
            fileMonitor.start();

        } catch (Exception e) {
            SLogs.w(e);
        }
    }

    private List<String> initFolderMonitorPath() {
        List<String> monitorPathList = new ArrayList<>();
        boolean isEnable = FolderBackupSharePreferenceHelper.readBackupSwitch();
        List<String> pathList = FolderBackupSharePreferenceHelper.readBackupPathsAsList();
        if (!isEnable || CollectionUtils.isEmpty(pathList)) {
            monitorPathList.add(IGNORE_PATHS.get(0));
        } else {
            // if not found ignore path in the path list, add the first ignore path to the path list
            boolean isFound = pathList.stream().anyMatch(IGNORE_PATHS::contains);
            if (!isFound) {
                pathList.add(IGNORE_PATHS.get(0));
            }
            monitorPathList.addAll(pathList);
        }
        return monitorPathList;
    }

    /**
     * it doesn't the 'change' event happen when download a large file
     *
     * @see #FILE_FILTER
     * @see DataManager#createTempFile()
     */
    private void doBackup(String action, File file) {
        // The file has changed: /storage/emulated/0/Android/media/com.seafile.seadroid2/Seafile
        if (file.getAbsolutePath().startsWith(IGNORE_PATHS.get(0))) {
            if ("change".equals(action)) {
                onAppCacheFileChanged(file);

                mainThreadHandler.post(new Runnable() {
                    @Override
                    public void run() {
                        SLogs.d(TAG, "onLocalFileChanged", file.getAbsolutePath());
                        BackupThreadExecutor.getInstance().runLocalFileUpdateTask();
                    }
                });

            }
        } else {

            mainThreadHandler.post(new Runnable() {
                @Override
                public void run() {
                    SLogs.d(TAG, "onBackupFileChanged", file.getAbsolutePath());
                    BackupThreadExecutor.getInstance().runFolderBackupFuture(true);
                }
            });

        }
    }

    private void onAppCacheFileChanged(File file) {
        TransferModel transferModel = new TransferModel();
        transferModel.file_name = file.getName();
        transferModel.file_size = file.length();
        transferModel.full_path = file.getAbsolutePath();
        transferModel.data_source = FeatureDataSource.AUTO_UPDATE_LOCAL_FILE;

        //repo data will be set in worker
//        transferModel.related_account = fileCacheStatusEntity.related_account;
//        transferModel.repo_id = fileCacheStatusEntity.repo_id;
//        transferModel.repo_name = fileCacheStatusEntity.repo_name;

        String id = EncryptUtils.encryptMD5ToString(file.getAbsolutePath());
        transferModel.setId(id);
        GlobalTransferCacheList.LOCAL_FILE_MONITOR_QUEUE.put(transferModel);
    }

    private class FolderStateChangedListener implements FileAlterationListener {

        public FolderStateChangedListener() {

        }

        @Override
        public void onStart(FileAlterationObserver observer) {
            SLogs.d(TAG, "onStart()", observer.getDirectory().getPath());
        }

        @Override
        public void onDirectoryCreate(File directory) {
            SLogs.d(TAG, "onDirectoryCreate()", directory.getAbsolutePath());
//            doBackup("create", directory);
        }

        @Override
        public void onDirectoryChange(File directory) {
            SLogs.d(TAG, "onDirectoryChange()", directory.getAbsolutePath());
//            doBackup("change", directory);
        }

        @Override
        public void onDirectoryDelete(File directory) {
            SLogs.d(TAG, "onDirectoryDelete()", directory.getAbsolutePath());
//            doBackup("delete", directory);
        }

        @Override
        public void onFileCreate(File file) {
            SLogs.d(TAG, "onFileCreate()", file.getAbsolutePath());
            doBackup("create", file);
        }

        @Override
        public void onFileChange(File file) {
            SLogs.d(TAG, "onFileChange()", file.getAbsolutePath());
            doBackup("change", file);
        }

        @Override
        public void onFileDelete(File file) {
            SLogs.d(TAG, "onFileDelete()", file.getAbsolutePath());
//            doBackup("delete", file);
        }

        @Override
        public void onStop(FileAlterationObserver observer) {
            SLogs.d(TAG, "onStop()");
        }
    }

    @Override
    public void onDestroy() {

        destroy();

        super.onDestroy();
    }

    private void destroy() {
        SLogs.e(TAG, "onDestroy()", "file monitor service destroy");
        stopFolderMonitor();

        // 移除定时任务回调
        periodicHandler.removeCallbacks(periodicTask);

        stopForeground(true);

        //
        if (mediaContentObserver != null) {
            mediaContentObserver.unregister();
            mediaContentObserver = null;
        }
    }
}
