package com.seafile.seadroid2.framework.service;

import android.content.Context;
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
import com.seafile.seadroid2.framework.worker.BackgroundJobManagerImpl;

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

    public boolean isFolderBackupRunning() {
        return folderBackupFuture != null && !folderBackupFuture.isDone();
    }

    public boolean isAlbumBackupRunning() {
        return albumBackupFuture != null && !albumBackupFuture.isDone();
    }

    public void stopAll() {
        stopAlbumBackup();
        stopFolderBackup();
        stopManualFileUpload();
        stopLocalFileUpdate();
        stopShareToSeafileUpload();
        stopDownload();

        // clear all notification
        notificationDispatcher.clearDelay();
    }

    public void stopById(String modelId, FeatureDataSource dataSource) {
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

        if (BackgroundJobManagerImpl.getInstance().getAlbumModuleRunning()) {
            SafeLogs.e(TAG, "album backup worker is running, please wait");
            return;
        }

        albumBackupFuture = runTask(new Runnable() {
            @Override
            public void run() {
                boolean isEnable = AlbumBackupSharePreferenceHelper.readBackupSwitch();
                if (!isEnable) {
                    SafeLogs.e("album backup is disable");
                    return;
                }

                MediaBackupScanner scanner = new MediaBackupScanner(getApplicationContext(), notificationDispatcher);
                SeafException scanSeafException = scanner.scan(isFullScan);
                if (scanSeafException != SeafException.SUCCESS) {
                    SafeLogs.d(TAG, "runAlbumBackupTask()", "scan error: " + scanSeafException);
                } else {
                    SafeLogs.d(TAG, "runAlbumBackupTask()", "scan success");
                }

                MediaBackupUploader mediaBackupUploader = getTransmitter(FeatureDataSource.ALBUM_BACKUP);
                SeafException uploadSeafException = mediaBackupUploader.upload();
                if (uploadSeafException != SeafException.SUCCESS) {
                    SafeLogs.d(TAG, "runAlbumBackupTask()", "backup error: " + uploadSeafException);
                } else {
                    SafeLogs.d(TAG, "runAlbumBackupTask()", "backup complete");
                }
            }
        }, new Runnable() {
            @Override
            public void run() {
                albumBackupFuture = null;
            }
        });
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

        if (BackgroundJobManagerImpl.getInstance().getFolderModuleRunning()) {
            SafeLogs.e(TAG, "folder backup worker is running, please wait");
            return;
        }

        folderBackupFuture = runTask(new Runnable() {
            @Override
            public void run() {
                boolean isEnable = FolderBackupSharePreferenceHelper.readBackupSwitch();
                if (!isEnable) {
                    SafeLogs.e("folder backup is disable");
                    return;
                }

                FolderBackupScanner scanner = new FolderBackupScanner(getApplicationContext(), notificationDispatcher);
                SeafException scanSeafException = scanner.scan(isFullScan);
                if (scanSeafException != SeafException.SUCCESS) {
                    SafeLogs.d(TAG, "runFolderBackupScanTask()", "scan error: " + scanSeafException);
                } else {
                    SafeLogs.d(TAG, "runFolderBackupScanTask()", "scan success");
                }

                FolderBackupUploader folderBackupUploader = getTransmitter(FeatureDataSource.FOLDER_BACKUP);
                SeafException uploadSeafException = folderBackupUploader.upload();
                if (uploadSeafException != SeafException.SUCCESS) {
                    SafeLogs.d(TAG, "runFolderBackupTask()", "backup error: " + uploadSeafException);
                } else {
                    SafeLogs.d(TAG, "runFolderBackupTask()", "backup complete");
                }
            }
        }, new Runnable() {
            @Override
            public void run() {
                folderBackupFuture = null;
            }
        });
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
