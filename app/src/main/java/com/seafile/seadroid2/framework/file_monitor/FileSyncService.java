package com.seafile.seadroid2.framework.file_monitor;

import android.app.Service;
import android.content.Intent;
import android.os.Binder;
import android.os.Handler;
import android.os.IBinder;

import androidx.annotation.Nullable;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.NetworkUtils;
import com.seafile.seadroid2.framework.datastore.DataManager;
import com.seafile.seadroid2.framework.datastore.StorageManager;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.framework.worker.BackgroundJobManagerImpl;
import com.seafile.seadroid2.framework.worker.observer.MediaContentObserver;
import com.seafile.seadroid2.ui.camera_upload.CameraUploadManager;
import com.seafile.seadroid2.framework.datastore.sp.FolderBackupManager;

import org.apache.commons.io.monitor.FileAlterationListener;
import org.apache.commons.io.monitor.FileAlterationObserver;

import java.io.File;
import java.io.FileFilter;
import java.util.ArrayList;
import java.util.List;

public class FileSyncService extends Service {
    private MediaContentObserver mediaContentObserver;
    private SupportFileAlterationMonitor fileMonitor;

    /**
     * <p>
     * Since the file download location of the app is located in the /Android/ directory,
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

    /**
     * TODO: Multiple cache directories
     */
    public void initIgnorePath() {
        if (!CollectionUtils.isEmpty(IGNORE_PATHS)) {
            return;
        }

        // /storage/emulated/0/Android/media/com.seafile.seadroid2.debug/Seafile
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
    public int onStartCommand(Intent intent, int flags, int startId) {
        return START_STICKY;
    }

    @Nullable
    @Override
    public IBinder onBind(Intent intent) {
        return mBinder;
    }

    private final IBinder mBinder = new FileSyncService.FileSyncBinder();

    public class FileSyncBinder extends Binder {
        public FileSyncService getService() {
            return FileSyncService.this;
        }
    }

    @Override
    public void onCreate() {
        super.onCreate();

        initIgnorePath();

        //Detect updates to local media file
        mediaContentObserver = new MediaContentObserver(getBaseContext(), new Handler());
        mediaContentObserver.register();

        //download worker
        BackgroundJobManagerImpl.getInstance().scheduleOneTimeFilesDownloadScanWorker();

        //media upload worker
        CameraUploadManager.getInstance().performSync();

        //folder backup upload worker
        BackgroundJobManagerImpl.getInstance().scheduleFolderBackupScannerWorker(false);

        //file upload backup
        BackgroundJobManagerImpl.getInstance().startFileUploadWorker();
        if (!NetworkUtils.isRegisteredNetworkStatusChangedListener(networkStatusChangedListener)) {
            NetworkUtils.registerNetworkStatusChangedListener(networkStatusChangedListener);
        }
    }

    private final NetworkUtils.OnNetworkStatusChangedListener networkStatusChangedListener = new NetworkUtils.OnNetworkStatusChangedListener() {
        @Override
        public void onDisconnected() {
            SLogs.e("disconnected: " + NetworkUtils.getNetworkType());
        }

        @Override
        public void onConnected(NetworkUtils.NetworkType networkType) {
            SLogs.e("connected: " + networkType);
        }
    };

    private final FileFilter FILE_FILTER = file -> {

        if (file.getAbsolutePath().startsWith(TEMP_FILE_DIR)) {
            return false;
        }

        final String fileName = file.getName();

        if (FolderBackupManager.isFolderBackupSkipHiddenFiles()) {
            return !fileName.startsWith(".");
        }


        return true;
    };

    public void stopFolderMonitor() {
        if (fileMonitor != null) {
            try {
                fileMonitor.stop();
            } catch (Exception e) {
                SLogs.w("FileSyncService", e);
            }
        }
    }

    public void startFolderMonitor() {
        List<String> pathList = FolderBackupManager.readBackupPaths();

        boolean isFound = pathList.stream().anyMatch(IGNORE_PATHS::contains);
        if (!isFound) {
            // /storage/emulated/0/Android/media/com.seafile.seadroid2
            pathList.add(IGNORE_PATHS.get(0));
        }

        startFolderMonitor(pathList);
    }

    private void startFolderMonitor(List<String> pathList) {
        if (CollectionUtils.isEmpty(pathList)) {
            return;
        }

        try {
            List<FileAlterationObserver> observerList = new ArrayList<>();
            for (String str : pathList) {

                SLogs.d("backup path: " + str);
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


    /**
     * it doesn't the 'change' event happen when download a large file
     *
     * @see #FILE_FILTER
     * @see DataManager#createTempFile()
     */
    public void doBackup(String action, File file) {
        // The file has changed: /storage/emulated/0/Android/media/com.seafile.seadroid2/Seafile
        if (file.getAbsolutePath().startsWith(IGNORE_PATHS.get(0))) {
            if ("change".equals(action)) {
                BackgroundJobManagerImpl.getInstance().startDownloadedCheckerWorker(file.getAbsolutePath());
            }
        } else {
            BackgroundJobManagerImpl.getInstance().scheduleFolderBackupScannerWorker(true);
        }
    }

    private class FolderStateChangedListener implements FileAlterationListener {

        public FolderStateChangedListener() {

        }

        @Override
        public void onStart(FileAlterationObserver observer) {
            SLogs.d("FileMonitor onStart: " + observer.getDirectory());
        }

        @Override
        public void onDirectoryCreate(File directory) {
            SLogs.d("FileMonitor onDirectoryCreate = " + directory.getAbsolutePath());
//            doBackup("create", directory);
        }

        @Override
        public void onDirectoryChange(File directory) {
            SLogs.d("FileMonitor onDirectoryChange = " + directory.getAbsolutePath());
//            doBackup("change", directory);
        }

        @Override
        public void onDirectoryDelete(File directory) {
            SLogs.d("FileMonitor onDirectoryDelete = " + directory.getAbsolutePath());
//            doBackup("delete", directory);
        }

        @Override
        public void onFileCreate(File file) {
            SLogs.d("FileMonitor onFileCreate = " + file.getAbsolutePath());
            doBackup("create", file);
        }

        @Override
        public void onFileChange(File file) {
            SLogs.d("FileMonitor onFileChange = " + file.getAbsolutePath());
            doBackup("change", file);
        }

        @Override
        public void onFileDelete(File file) {
            SLogs.d("FileMonitor onFileDelete = " + file.getAbsolutePath());
//            doBackup("delete", file);
        }

        @Override
        public void onStop(FileAlterationObserver observer) {
            SLogs.d("FileMonitor onStop");
        }
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        if (fileMonitor != null) {
            try {
                fileMonitor.stop();
            } catch (Exception e) {
                SLogs.w("FileSyncService", e);
            }
        }

        if (mediaContentObserver != null) {
            mediaContentObserver.unregister();
        }

        if (NetworkUtils.isRegisteredNetworkStatusChangedListener(networkStatusChangedListener)) {
            NetworkUtils.unregisterNetworkStatusChangedListener(networkStatusChangedListener);
        }
    }

}
