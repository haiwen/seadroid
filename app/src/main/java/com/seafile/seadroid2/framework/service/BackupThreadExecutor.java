package com.seafile.seadroid2.framework.service;

import android.content.Context;
import android.os.Handler;
import android.os.Looper;
import android.text.TextUtils;

import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.enums.FeatureDataSource;
import com.seafile.seadroid2.framework.datastore.sp_livedata.AlbumBackupSharePreferenceHelper;
import com.seafile.seadroid2.framework.datastore.sp_livedata.FolderBackupSharePreferenceHelper;
import com.seafile.seadroid2.framework.executor.TaskExecutor;
import com.seafile.seadroid2.framework.service.download.FileDownloader;
import com.seafile.seadroid2.framework.service.upload.FileUploader;
import com.seafile.seadroid2.framework.service.upload.FolderBackupScanner;
import com.seafile.seadroid2.framework.service.upload.FolderBackupUploader;
import com.seafile.seadroid2.framework.service.upload.LocalFileUpdater;
import com.seafile.seadroid2.framework.service.upload.MediaBackupScanner;
import com.seafile.seadroid2.framework.service.upload.MediaBackupUploader;
import com.seafile.seadroid2.framework.service.upload.ShareToSeafileUploader;
import com.seafile.seadroid2.framework.util.SafeLogs;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.BiConsumer;

public class BackupThreadExecutor {
    private final String TAG = "BackupThreadExecutor";

    private static volatile BackupThreadExecutor singleton = null;
    private final Map<FeatureDataSource, ParentEventTransfer> transmitterMap = new HashMap<>();

    private final AtomicInteger runningTaskCount = new AtomicInteger(0);
    private final ThreadPoolExecutor _executor;

    private final BackupThreadNotificationDispatcher notificationDispatcher = new BackupThreadNotificationDispatcher(SeadroidApplication.getAppContext());

    private BackupThreadExecutor() {
        _executor = TaskExecutor.getInstance().getExecutor();

        if (transmitterMap.isEmpty()) {
            FileUploader fileUploader = new FileUploader(getApplicationContext(), notificationDispatcher);
            MediaBackupUploader mediaBackupUploader = new MediaBackupUploader(getApplicationContext(), notificationDispatcher);
            FolderBackupUploader folderBackupUploader = new FolderBackupUploader(getApplicationContext(), notificationDispatcher);
            ShareToSeafileUploader shareToSeafileUploader = new ShareToSeafileUploader(getApplicationContext(), notificationDispatcher);
            FileDownloader downloader = new FileDownloader(getApplicationContext(), notificationDispatcher);
            LocalFileUpdater localFileUpdater = new LocalFileUpdater(getApplicationContext(), notificationDispatcher);

            transmitterMap.put(FeatureDataSource.MANUAL_FILE_UPLOAD, fileUploader);
            transmitterMap.put(FeatureDataSource.ALBUM_BACKUP, mediaBackupUploader);
            transmitterMap.put(FeatureDataSource.FOLDER_BACKUP, folderBackupUploader);
            transmitterMap.put(FeatureDataSource.DOWNLOAD, downloader);
            transmitterMap.put(FeatureDataSource.AUTO_UPDATE_LOCAL_FILE, localFileUpdater);
            transmitterMap.put(FeatureDataSource.SHARE_FILE_TO_SEAFILE, shareToSeafileUploader);
        }
    }

    private <T extends ParentEventTransfer> T getTransmitter(FeatureDataSource dataSource) {
        if (!transmitterMap.containsKey(dataSource)) {
            throw new RuntimeException("You must provide a valid data source.");
        }
        return (T) transmitterMap.get(dataSource);
    }

    public static BackupThreadExecutor getInstance() {
        if (singleton == null) {
            synchronized (BackupThreadExecutor.class) {
                if (singleton == null) {
                    singleton = new BackupThreadExecutor();
                }
            }
        }
        return singleton;
    }

    private Context context;

    public Context getApplicationContext() {
        if (context == null) {
            context = SeadroidApplication.getAppContext();
        }
        return context;
    }


    //future
    private CompletableFuture<Void> fileDownloadFuture;
    private CompletableFuture<Void> manualFileUploadFuture;
    private CompletableFuture<Void> folderBackupFuture;
    private CompletableFuture<Void> albumBackupFuture;
    private CompletableFuture<Void> localFileUpdateFuture;
    private CompletableFuture<Void> shareFileUploadFuture;

    /**
     * Delay before the uploader starts after a scan completes. Gives a just-taken
     * photo a moment to finish being written by the camera, so that the uploader's
     * size check is not triggered spuriously. The delay is scheduled on the main
     * looper (non-blocking), the upload itself still runs on the executor.
     */
    private static final long UPLOAD_START_DELAY_MS = 1000L;

    private final Handler mainHandler = new Handler(Looper.getMainLooper());

    public boolean isFolderBackupRunning() {
        return folderBackupFuture != null && !folderBackupFuture.isDone();
    }

    public boolean isAlbumBackupRunning() {
        return albumBackupFuture != null && !albumBackupFuture.isDone();
    }

    public boolean isUploading() {
        return (manualFileUploadFuture != null && !manualFileUploadFuture.isDone())
                || (shareFileUploadFuture != null && !shareFileUploadFuture.isDone())
                || (localFileUpdateFuture != null && !localFileUpdateFuture.isDone())
                || (folderBackupFuture != null && !folderBackupFuture.isDone())
                || (albumBackupFuture != null && !albumBackupFuture.isDone());
    }

    public boolean isDownloading() {
        return fileDownloadFuture != null && !fileDownloadFuture.isDone();
    }

    public boolean isTransferring() {
        return isUploading() || isDownloading();
    }

    public void stopAll() {
        MediaBackupUploader mediaBackupUploader = getTransmitter(FeatureDataSource.ALBUM_BACKUP);
        if (mediaBackupUploader != null) {
            mediaBackupUploader.stop();
        }

        FolderBackupUploader folderBackupUploader = getTransmitter(FeatureDataSource.FOLDER_BACKUP);
        if (folderBackupUploader != null) {
            folderBackupUploader.stop();
        }

        FileUploader fileUploader = getTransmitter(FeatureDataSource.MANUAL_FILE_UPLOAD);
        if (fileUploader != null) {
            fileUploader.stop();
        }

        FileDownloader fileDownloader = getTransmitter(FeatureDataSource.DOWNLOAD);
        if (fileDownloader != null) {
            fileDownloader.stop();
        }

        // clear all notification
        notificationDispatcher.clearDelay();
    }

    public void stopSpecialTransmitter(String modelId, FeatureDataSource dataSource) {
        if (TextUtils.isEmpty(modelId)) {
            throw new RuntimeException("You must provide a valid parameter.");
        }

        if (FeatureDataSource.ALBUM_BACKUP == dataSource) {
            MediaBackupUploader mediaBackupUploader = getTransmitter(FeatureDataSource.ALBUM_BACKUP);
            mediaBackupUploader.stopById(modelId);
        } else if (FeatureDataSource.FOLDER_BACKUP == dataSource) {
            FolderBackupUploader folderBackupUploader = getTransmitter(FeatureDataSource.FOLDER_BACKUP);
            folderBackupUploader.stopById(modelId);
        } else if (FeatureDataSource.MANUAL_FILE_UPLOAD == dataSource) {
            FileUploader fileUploader = getTransmitter(FeatureDataSource.MANUAL_FILE_UPLOAD);
            fileUploader.stopById(modelId);
        } else if (FeatureDataSource.DOWNLOAD == dataSource) {
            FileDownloader fileDownloader = getTransmitter(FeatureDataSource.DOWNLOAD);
            fileDownloader.stopById(modelId);
        } else {
            throw new RuntimeException("You must provide a valid data source.");
        }

        SafeLogs.d(TAG, "stopById()", "stopped: " + modelId);
    }

    public void stopDownload() {
        if (fileDownloadFuture != null && !fileDownloadFuture.isDone()) {
            fileDownloadFuture.cancel(true);
        }
    }

    public void runDownloadTask() {
        fileDownloadFuture = runTask(new Runnable() {
            @Override
            public void run() {
                FileDownloader fileDownloader = getTransmitter(FeatureDataSource.DOWNLOAD);
                SeafException seafException = fileDownloader.download();

                if (seafException != SeafException.SUCCESS) {
                    SafeLogs.d(TAG, "runDownloadTask()", "download error: " + seafException);
                } else {
                    SafeLogs.d(TAG, "runDownloadTask()", "download success");
                }
            }
        }, new Runnable() {
            @Override
            public void run() {
                fileDownloadFuture = null;
            }
        });
    }

    public void stopManualFileUpload() {
        if (manualFileUploadFuture != null && !manualFileUploadFuture.isDone()) {
            manualFileUploadFuture.cancel(true);
        }
    }

    public void runManualFileUploadTask() {
        if (manualFileUploadFuture != null && !manualFileUploadFuture.isDone()) {
            SafeLogs.e(TAG, "album backup task is running, please wait");
            return;
        }

        manualFileUploadFuture = runTask(new Runnable() {
            @Override
            public void run() {
                FileUploader fileUploader = getTransmitter(FeatureDataSource.MANUAL_FILE_UPLOAD);
                SeafException seafException = fileUploader.upload();
                if (seafException != SeafException.SUCCESS) {
                    SafeLogs.d(TAG, "runFileUploadTask()", "upload error: " + seafException);
                } else {
                    SafeLogs.d(TAG, "runFileUploadTask()", "upload success");
                }
            }
        }, new Runnable() {
            @Override
            public void run() {
                manualFileUploadFuture = null;
            }
        });
    }

    public void stopAlbumBackup() {
        stopAlbumBackup(false);
    }

    public void stopAlbumBackup(boolean reRun) {
        SafeLogs.d(TAG, "stopAlbumBackup()", "reRun: " + reRun);

        if (reRun) {
            if (albumBackupFuture != null && !albumBackupFuture.isDone()) {
                albumBackupFuture.cancel(true);

                albumBackupFuture.whenComplete(new BiConsumer<Void, Throwable>() {
                    @Override
                    public void accept(Void unused, Throwable throwable) {
                        SafeLogs.e(TAG, "album backup task complete");
                        runAlbumBackupTask(true);
                    }
                });
            } else {
                runAlbumBackupTask(true);
            }
        } else {
            if (albumBackupFuture != null && !albumBackupFuture.isDone()) {
                albumBackupFuture.cancel(true);
            }
        }
    }

    public void runAlbumBackupTask(boolean isFullScan) {
        SafeLogs.d(TAG, "runAlbumBackupTask()", "isFullScan: " + isFullScan);

        if (albumBackupFuture != null && !albumBackupFuture.isDone()) {
            SafeLogs.e(TAG, "album backup task is running, please wait");
            return;
        }

//        if (BackgroundJobManagerImpl.getInstance().getAlbumModuleRunning()) {
//            SafeLogs.e(TAG, "album backup worker is running, please wait");
//            return;
//        }

        // The scan starts immediately on the executor. The uploader is scheduled
        // to start UPLOAD_START_DELAY_MS after the scan completes (non-blocking,
        // via the main looper), giving a just-taken photo time to finish being
        // written before the uploader checks its size.
        CompletableFuture<Void>[] chain = new CompletableFuture[1];
        chain[0] = CompletableFuture.supplyAsync(() -> runAlbumScan(isFullScan), _executor)
                .thenComposeAsync(shouldUpload -> shouldUpload
                                ? scheduleDelayedUpload(this::runAlbumUpload, chain)
                                : CompletableFuture.completedFuture(null),
                        _executor);
        albumBackupFuture = chain[0];
        chain[0].whenComplete((unused, throwable) -> albumBackupFuture = null);
    }

    private boolean runAlbumScan(boolean isFullScan) {
        try {
            boolean isEnable = AlbumBackupSharePreferenceHelper.readBackupSwitch();
            if (!isEnable) {
                SafeLogs.e("album backup is disable");
                return false;
            }

            MediaBackupScanner scanner = new MediaBackupScanner(getApplicationContext(), notificationDispatcher);
            SeafException scanSeafException = scanner.scan(isFullScan);
            if (scanSeafException != SeafException.SUCCESS) {
                SafeLogs.d(TAG, "runAlbumBackupTask()", "scan error: " + scanSeafException);
            } else {
                SafeLogs.d(TAG, "runAlbumBackupTask()", "scan success");
            }
            return true;
        } catch (Exception e) {
            SafeLogs.e(TAG, "runAlbumScan()", e.getMessage());
            return false;
        }
    }

    private void runAlbumUpload() {
        try {
            MediaBackupUploader mediaBackupUploader = getTransmitter(FeatureDataSource.ALBUM_BACKUP);
            SeafException uploadSeafException = mediaBackupUploader.upload();
            if (uploadSeafException != SeafException.SUCCESS) {
                SafeLogs.d(TAG, "runAlbumBackupTask()", "backup error: " + uploadSeafException);
            } else {
                SafeLogs.d(TAG, "runAlbumBackupTask()", "backup complete");
            }
        } catch (Exception e) {
            SafeLogs.e(TAG, "runAlbumUpload()", e.getMessage());
        }
    }

    public void stopFolderBackup() {
        stopFolderBackup(false);
    }

    public void stopFolderBackup(boolean reRun) {
        SafeLogs.d(TAG, "stopFolderBackup()");

        if (reRun) {
            if (folderBackupFuture != null && !folderBackupFuture.isDone()) {
                folderBackupFuture.cancel(true);

                folderBackupFuture.whenComplete(new BiConsumer<Void, Throwable>() {
                    @Override
                    public void accept(Void unused, Throwable throwable) {
                        SafeLogs.e(TAG, "folder backup task complete");
                        runFolderBackupFuture(true);
                    }
                });
            } else {
                runFolderBackupFuture(true);
            }
        } else {
            if (folderBackupFuture != null && !folderBackupFuture.isDone()) {
                folderBackupFuture.cancel(true);
            }
        }
    }

    public void runFolderBackupFuture(boolean isFullScan) {
        SafeLogs.d(TAG, "runFolderBackupFuture()", "isFullScan: " + isFullScan);

        if (folderBackupFuture != null && !folderBackupFuture.isDone()) {
            SafeLogs.e(TAG, "folder backup task is running, please wait");
            return;
        }

//        if (BackgroundJobManagerImpl.getInstance().getFolderModuleRunning()) {
//            SafeLogs.e(TAG, "folder backup worker is running, please wait");
//            return;
//        }

        // The scan starts immediately on the executor. The uploader is scheduled
        // to start UPLOAD_START_DELAY_MS after the scan completes (non-blocking,
        // via the main looper), giving a file that is still being written time to
        // finish before the uploader checks its size.
        CompletableFuture<Void>[] chain = new CompletableFuture[1];
        chain[0] = CompletableFuture.supplyAsync(() -> runFolderScan(isFullScan), _executor)
                .thenComposeAsync(shouldUpload -> shouldUpload
                                ? scheduleDelayedUpload(this::runFolderUpload, chain)
                                : CompletableFuture.completedFuture(null),
                        _executor);
        folderBackupFuture = chain[0];
        chain[0].whenComplete((unused, throwable) -> folderBackupFuture = null);
    }

    private boolean runFolderScan(boolean isFullScan) {
        try {
            boolean isEnable = FolderBackupSharePreferenceHelper.readBackupSwitch();
            if (!isEnable) {
                SafeLogs.d(TAG, "runFolderScan()", "folder backup switch is off, skip scheduling uploader");
                return false;
            }

            FolderBackupScanner scanner = new FolderBackupScanner(getApplicationContext(), notificationDispatcher);
            SeafException scanSeafException = scanner.scan(isFullScan);
            if (scanSeafException != SeafException.SUCCESS) {
                SafeLogs.d(TAG, "runFolderBackupScanTask()", "scan error: " + scanSeafException);
            } else {
                SafeLogs.d(TAG, "runFolderBackupScanTask()", "scan success");
            }
            return true;
        } catch (Exception e) {
            SafeLogs.e(TAG, "runFolderScan()", e.getMessage());
            return false;
        }
    }

    private void runFolderUpload() {
        try {
            FolderBackupUploader folderBackupUploader = getTransmitter(FeatureDataSource.FOLDER_BACKUP);
            SeafException uploadSeafException = folderBackupUploader.upload();
            if (uploadSeafException != SeafException.SUCCESS) {
                SafeLogs.d(TAG, "runFolderBackupTask()", "backup error: " + uploadSeafException);
            } else {
                SafeLogs.d(TAG, "runFolderBackupTask()", "backup complete");
            }
        } catch (Exception e) {
            SafeLogs.e(TAG, "runFolderUpload()", e.getMessage());
        }
    }

    /**
     * Schedule the uploader to start after {@link #UPLOAD_START_DELAY_MS} without
     * blocking any thread. The returned future completes when the upload finishes.
     * <p>
     * {@code chain} holds the task future; if the task was cancelled (stopped)
     * during the delay window, the upload is skipped.
     */
    private CompletableFuture<Void> scheduleDelayedUpload(Runnable uploadAction, CompletableFuture<Void>[] chain) {
        SafeLogs.d(TAG, "scheduleDelayedUpload()", "scan finished, schedule uploader to start in " + UPLOAD_START_DELAY_MS + "ms");
        CompletableFuture<Void> uploadFuture = new CompletableFuture<>();
        mainHandler.postDelayed(() -> {
            // The task may have been stopped during the delay window.
            if (chain[0] == null || chain[0].isCancelled()) {
                SafeLogs.d(TAG, "scheduleDelayedUpload()", "task was cancelled during the delay window, skip uploader");
                uploadFuture.complete(null);
                return;
            }
            SafeLogs.d(TAG, "scheduleDelayedUpload()", "delay elapsed, start uploader on executor");
            try {
                _executor.execute(() -> {
                    try {
                        uploadAction.run();
                        SafeLogs.d(TAG, "scheduleDelayedUpload()", "uploader finished");
                        uploadFuture.complete(null);
                    } catch (Throwable t) {
                        SafeLogs.e(TAG, "scheduleDelayedUpload()", t.getMessage());
                        uploadFuture.completeExceptionally(t);
                    }
                });
            } catch (Throwable t) {
                SafeLogs.e(TAG, "scheduleDelayedUpload()", t.getMessage());
                uploadFuture.completeExceptionally(t);
            }
        }, UPLOAD_START_DELAY_MS);
        return uploadFuture;
    }

    public void stopLocalFileUpdate() {
        if (localFileUpdateFuture != null && !localFileUpdateFuture.isDone()) {
            localFileUpdateFuture.cancel(true);
        }
    }

    public void runLocalFileUpdateTask() {
        localFileUpdateFuture = runTask(new Runnable() {
            @Override
            public void run() {
                LocalFileUpdater localFileUpdater = getTransmitter(FeatureDataSource.AUTO_UPDATE_LOCAL_FILE);
                SeafException seafException = localFileUpdater.upload();

                if (seafException != SeafException.SUCCESS) {
                    SafeLogs.d(TAG, "runLocalFileUpdateTask()", "upload error: " + seafException);
                } else {
                    SafeLogs.d(TAG, "runLocalFileUpdateTask()", "upload success");
                }
            }
        }, new Runnable() {
            @Override
            public void run() {
                localFileUpdateFuture = null;
            }
        });
    }

    public void stopShareToSeafileUpload() {
        if (shareFileUploadFuture != null && !shareFileUploadFuture.isDone()) {
            shareFileUploadFuture.cancel(true);
        }
    }

    public void runShareToSeafileUploadTask() {
        shareFileUploadFuture = runTask(new Runnable() {
            @Override
            public void run() {
                ShareToSeafileUploader shareToSeafileUploader = getTransmitter(FeatureDataSource.SHARE_FILE_TO_SEAFILE);
                SeafException seafException = shareToSeafileUploader.upload();

                if (seafException != SeafException.SUCCESS) {
                    SafeLogs.d(TAG, "ShareToSeafile", "upload error: " + seafException);
                } else {
                    SafeLogs.d(TAG, "ShareToSeafile", "upload success");
                }
            }
        }, new Runnable() {
            @Override
            public void run() {
                shareFileUploadFuture = null;
            }
        });
    }

    private CompletableFuture<Void> runTask(Runnable runnable, Runnable onComplete) {
        runningTaskCount.incrementAndGet();
        return CompletableFuture
                .runAsync(runnable, _executor)
                .whenComplete(new BiConsumer<Void, Throwable>() {
                    @Override
                    public void accept(Void unused, Throwable throwable) {
                        if (onComplete != null) onComplete.run();
                        int currentCount = runningTaskCount.decrementAndGet();
                        if (currentCount == 0) {
                            SafeLogs.e(TAG, "all task complete");
                        }
                    }
                });
    }
}
