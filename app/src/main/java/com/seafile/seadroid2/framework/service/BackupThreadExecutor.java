package com.seafile.seadroid2.framework.service;

import android.content.Context;

import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.framework.datastore.sp_livedata.AlbumBackupSharePreferenceHelper;
import com.seafile.seadroid2.framework.datastore.sp_livedata.FolderBackupSharePreferenceHelper;
import com.seafile.seadroid2.framework.executor.TaskExecutor;
import com.seafile.seadroid2.framework.service.upload.FolderBackupScanner;
import com.seafile.seadroid2.framework.service.upload.FolderBackupUploader;
import com.seafile.seadroid2.framework.service.upload.LocalFileUpdater;
import com.seafile.seadroid2.framework.service.upload.MediaBackupScanner;
import com.seafile.seadroid2.framework.service.upload.MediaBackupUploader;
import com.seafile.seadroid2.framework.util.SafeLogs;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.BiConsumer;

public class BackupThreadExecutor {
    private final String TAG = "BackupThreadExecutor";

    private static final BackupThreadExecutor INSTANCE = new BackupThreadExecutor();
    private final AtomicInteger runningTaskCount = new AtomicInteger(0);
    private final ThreadPoolExecutor _executor;


    private final BackupTransferNotificationDispatcher notificationDispatcher = new BackupTransferNotificationDispatcher(SeadroidApplication.getAppContext());

    private BackupThreadExecutor() {
        _executor = TaskExecutor.getInstance().getExecutor();
    }

    public static BackupThreadExecutor getInstance() {
        return INSTANCE;
    }

    private Context context;

    public Context getApplicationContext() {
        if (context == null) {
            context = SeadroidApplication.getAppContext();
        }
        return context;
    }

    private CompletableFuture<Void> folderBackupFuture;
    private CompletableFuture<Void> albumBackupFuture;
    private CompletableFuture<Void> localFileUploadFuture;

    public boolean anyBackupRunning() {
        return isFolderBackupRunning() || isAlbumBackupRunning() || isLocalFileUploadRunning();
    }

    public boolean isFolderBackupRunning() {
        return folderBackupFuture != null && !folderBackupFuture.isDone();
    }

    public boolean isAlbumBackupRunning() {
        return albumBackupFuture != null && !albumBackupFuture.isDone();
    }

    public boolean isLocalFileUploadRunning() {
        return localFileUploadFuture != null && !localFileUploadFuture.isDone();
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

        if (albumBackupFuture != null && !albumBackupFuture.isDone()) {
            SafeLogs.e(TAG, "album backup task is running, please wait");
            return;
        }

        SafeLogs.d(TAG, "runAlbumBackupTask()", "isFullScan: " + isFullScan);

        albumBackupFuture = runTask(new Runnable() {
            @Override
            public void run() {
                boolean isEnable = AlbumBackupSharePreferenceHelper.readBackupSwitch();
                if (!isEnable) {
                    SafeLogs.e("album backup is disable");
                    return;
                }

                MediaBackupScanner scanner = new MediaBackupScanner(getApplicationContext());
                SeafException scanSeafException = scanner.scan(isFullScan);
                if (scanSeafException != SeafException.SUCCESS) {
                    SafeLogs.d(TAG, "runAlbumBackupTask()", "scan error: " + scanSeafException);
                } else {
                    SafeLogs.d(TAG, "runAlbumBackupTask()", "scan success");
                }

                MediaBackupUploader mediaBackupUploader = new MediaBackupUploader(getApplicationContext(), notificationDispatcher);
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
        if (folderBackupFuture != null && !folderBackupFuture.isDone()) {
            SafeLogs.e(TAG, "folder backup task is running, please wait");
            return;
        }

        SafeLogs.d(TAG, "runFolderBackupFuture()", "isFullScan: " + isFullScan);

        folderBackupFuture = runTask(new Runnable() {
            @Override
            public void run() {
                boolean isEnable = FolderBackupSharePreferenceHelper.readBackupSwitch();
                if (!isEnable) {
                    SafeLogs.e("folder backup is disable");
                    return;
                }

                FolderBackupScanner scanner = new FolderBackupScanner(getApplicationContext());
                SeafException scanSeafException = scanner.scan(isFullScan);
                if (scanSeafException != SeafException.SUCCESS) {
                    SafeLogs.d(TAG, "runFolderBackupScanTask()", "scan error: " + scanSeafException);
                } else {
                    SafeLogs.d(TAG, "runFolderBackupScanTask()", "scan success");
                }

                FolderBackupUploader folderBackupUploader = new FolderBackupUploader(getApplicationContext(), notificationDispatcher);
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

    public void runLocalFileUpdateTask() {
        localFileUploadFuture = runTask(new Runnable() {
            @Override
            public void run() {
                LocalFileUpdater localFileUpdater = new LocalFileUpdater(getApplicationContext(), notificationDispatcher);
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
                localFileUploadFuture = null;
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
