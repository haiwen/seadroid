package com.seafile.seadroid2.worker;

import android.app.Service;
import android.content.Intent;
import android.os.Binder;
import android.os.Handler;
import android.os.IBinder;
import android.text.TextUtils;

import androidx.annotation.Nullable;

import com.blankj.utilcode.util.CollectionUtils;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.ui.folder_backup.RepoConfig;
import com.seafile.seadroid2.ui.selector.folder_selector.StringTools;
import com.seafile.seadroid2.util.SLogs;
import com.seafile.seadroid2.util.sp.FolderBackupConfigSPs;
import com.seafile.seadroid2.util.sp.SettingsManager;
import com.seafile.seadroid2.worker.observer.MediaContentObserver;

import org.apache.commons.io.monitor.FileAlterationListener;
import org.apache.commons.io.monitor.FileAlterationMonitor;
import org.apache.commons.io.monitor.FileAlterationObserver;

import java.io.File;
import java.io.FileFilter;
import java.util.ArrayList;
import java.util.List;

public class FileSyncService extends Service {
    private MediaContentObserver mediaContentObserver;
    private FileAlterationMonitor fileMonitor;

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

        mediaContentObserver = new MediaContentObserver(getBaseContext(), new Handler());
        mediaContentObserver.register();

        startFolderMonitor();

        //start folder backup upload worker
        doBackup();

        //download
        BackgroundJobManagerImpl.getInstance().scheduleOneTimeFilesDownloadSyncJob();

        //start media backup upload worker
        BackgroundJobManagerImpl.getInstance().scheduleOneTimeMediaSyncJob();
    }

    private final FileFilter FILE_FILTER = file -> {
        final String fileName = file.getName();

        //is jump hidden file or folder?
        //true：jump
        if (SettingsManager.getInstance().isFolderBackupJumpHiddenFiles()) {
            return !fileName.startsWith(".");
        }
        return true;
    };

    private void startFolderMonitor() {
        RepoConfig repoConfig = FolderBackupConfigSPs.getBackupConfigByCurrentAccount();
        List<String> pathList = FolderBackupConfigSPs.getBackupPathListByCurrentAccount();
        boolean folderAutomaticBackup = SettingsManager.getInstance().isFolderAutomaticBackup();
        if (folderAutomaticBackup && !CollectionUtils.isEmpty(pathList) && repoConfig != null) {
            startFolderMonitor(pathList);
        }
    }

    public void startFolderMonitor(List<String> backupPaths) {
        if (CollectionUtils.isEmpty(backupPaths)) {
            return;
        }

        List<FileAlterationObserver> fileAlterationObserverList = new ArrayList<>();
        for (String str : backupPaths) {
            SLogs.d("backup path: " + str);
            FileAlterationObserver folderFileObserver = new FileAlterationObserver(str, FILE_FILTER);
            folderFileObserver.addListener(new FileSyncService.FolderStateChangedListener());
            fileAlterationObserverList.add(folderFileObserver);
        }

        try {
            if (fileMonitor != null) {
                fileMonitor.stop();
            }

            fileMonitor = new FileAlterationMonitor(10000L, fileAlterationObserverList);
            fileMonitor.start();
        } catch (Exception e) {
            throw new RuntimeException("failed to start file monitor");
        }
    }

    public void doBackup(String action, File file) {
        BackgroundJobManagerImpl.getInstance().scheduleOneTimeFilesUploadSyncJob();
    }

    public void doBackup() {
        BackgroundJobManagerImpl.getInstance().scheduleOneTimeFilesUploadSyncJob();
    }

    private class FolderStateChangedListener implements FileAlterationListener {

        public FolderStateChangedListener() {

        }

        @Override
        public void onStart(FileAlterationObserver observer) {

            SLogs.d("onStart");
        }

        @Override
        public void onDirectoryCreate(File directory) {
            SLogs.d("onDirectoryCreate = " + directory.getAbsolutePath());
            doBackup("create", directory);
        }

        @Override
        public void onDirectoryChange(File directory) {
            SLogs.d("onDirectoryChange = " + directory.getAbsolutePath());
            doBackup("change", directory);
        }

        @Override
        public void onDirectoryDelete(File directory) {
            SLogs.d("onDirectoryDelete = " + directory.getAbsolutePath());
            doBackup("delete", directory);
        }

        @Override
        public void onFileCreate(File file) {
            SLogs.d("onFileCreate = " + file.getAbsolutePath());
            doBackup("create", file);
        }

        @Override
        public void onFileChange(File file) {
            SLogs.d("onFileChange = " + file.getAbsolutePath());
            doBackup("change", file);
        }

        @Override
        public void onFileDelete(File file) {
            SLogs.d("onFileDelete = " + file.getAbsolutePath());
            doBackup("delete", file);
        }

        @Override
        public void onStop(FileAlterationObserver observer) {
            SLogs.d("onStop");
        }
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        if (fileMonitor != null) {
            try {
                fileMonitor.stop();
            } catch (Exception e) {
                throw new RuntimeException("failed to stop file monitor");
            }
        }

        if (mediaContentObserver != null) {
            mediaContentObserver.unregister();
        }
    }

}
