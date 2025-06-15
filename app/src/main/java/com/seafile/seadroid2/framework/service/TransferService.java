package com.seafile.seadroid2.framework.service;

import android.content.Context;
import android.content.Intent;
import android.content.pm.ServiceInfo;
import android.os.Binder;
import android.os.Build;
import android.os.Bundle;
import android.os.IBinder;
import android.text.TextUtils;

import androidx.annotation.Nullable;
import androidx.core.app.ServiceCompat;
import androidx.core.content.ContextCompat;

import com.blankj.utilcode.util.CollectionUtils;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.enums.FeatureDataSource;
import com.seafile.seadroid2.framework.datastore.sp_livedata.AlbumBackupSharePreferenceHelper;
import com.seafile.seadroid2.framework.datastore.sp_livedata.FolderBackupSharePreferenceHelper;
import com.seafile.seadroid2.framework.notification.NotificationInfo;
import com.seafile.seadroid2.framework.notification.TransferNotificationDispatcher;
import com.seafile.seadroid2.framework.service.download.FileDownloader;
import com.seafile.seadroid2.framework.service.upload.FileUploader;
import com.seafile.seadroid2.framework.service.upload.FolderBackupScanner;
import com.seafile.seadroid2.framework.service.upload.FolderBackupUploader;
import com.seafile.seadroid2.framework.service.upload.LocalFileUpdater;
import com.seafile.seadroid2.framework.service.upload.MediaBackupScanner;
import com.seafile.seadroid2.framework.service.upload.MediaBackupUploader;
import com.seafile.seadroid2.framework.util.SafeLogs;
import com.seafile.seadroid2.framework.worker.queue.TransferModel;
import com.seafile.seadroid2.ui.folder_backup.RepoConfig;

import java.lang.ref.WeakReference;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.BiConsumer;

public class TransferService extends EventService {
    private final String TAG = "TransferService";


    private TransferNotificationDispatcher transferNotificationDispatcher;

    private CompletableFuture<Void> photoUploadFuture;
    private CompletableFuture<Void> folderUploadFuture;
    private CompletableFuture<Void> fileDownloadFuture;
    private CompletableFuture<Void> manualUploadFuture;
    private CompletableFuture<Void> localFileUpdateFuture;

    private final AtomicInteger runningTaskCount = new AtomicInteger(0);
    private final AtomicBoolean foregroundStarted = new AtomicBoolean(false);

    private ExecutorService _executor;

    public static void restartPhotoBackupService(Context context) {
        Bundle bundle = new Bundle();
        bundle.putBoolean("is_force", true);
        bundle.putBoolean("is_restart", true);
        startService(context, "RESTART_PHOTO_BACKUP", bundle);
    }

    public static void stopPhotoBackupService(Context context) {
        startService(context, "STOP_PHOTO_BACKUP");
    }

    public static void startFolderBackupService(Context context) {
        startService(context, "START_FOLDER_BACKUP");
    }

    public static void restartFolderBackupService(Context context) {
        Bundle bundle = new Bundle();
        bundle.putBoolean("is_force", true);
        bundle.putBoolean("is_restart", true);
        startService(context, "START_FOLDER_BACKUP", bundle);
    }

    public static void stopFolderBackupService(Context context) {
        startService(context, "STOP_FOLDER_BACKUP");
    }

    public static void startManualUploadService(Context context) {
        startService(context, "START_MANUAL_UPLOAD");
    }

    public static void startDownloadService(Context context) {
        startService(context, "START_FILE_DOWNLOAD");
    }

    public static void startLocalFileUpdateService(Context context) {
        startService(context, "START_LOCAL_FILE_UPDATE");
    }

    public static void stopTransferService(Context context, TransferModel model) {
        if (model == null) {
            return;
        }

        Bundle bundle = new Bundle();
        bundle.putString("model", model.getId());
        bundle.putString("source", model.data_source.toString());
        startService(context, "STOP_TRANSFER", bundle);
    }

    public static void startService(Context context, String action) {
        startService(context, action, null);
    }

    public static void startService(Context context, String action, Bundle extras) {
        Intent intent = new Intent(context, TransferService.class);
        intent.setAction(action);
        if (extras != null) {
            intent.putExtras(extras);
        }
        ContextCompat.startForegroundService(context, intent);
    }


    @Override
    public void onCreate() {
        super.onCreate();

        transferNotificationDispatcher = new TransferNotificationDispatcher(getApplicationContext());


        _executor = Executors.newFixedThreadPool(5, new ThreadFactory() {
            private final AtomicInteger count = new AtomicInteger(1);

            @Override
            public Thread newThread(Runnable r) {
                return new Thread(r, "TransferService-Task-" + count.getAndIncrement());
            }
        });
    }

    public static class TBinder extends Binder {
        private final WeakReference<TransferService> serviceRef;

        public TBinder(TransferService service) {
            this.serviceRef = new WeakReference<>(service);
        }

        public TransferService getService() {
            return serviceRef.get();
        }
    }

    private final IBinder mBinder = new TransferService.TBinder(this);

    @Nullable
    @Override
    public IBinder onBind(Intent intent) {
        return mBinder;
    }

    @Override
    public int onStartCommand(Intent intent, int flags, int startId) {
        if (intent == null) return START_STICKY;

        String action = intent.getAction();
        if (action == null) {
            return START_STICKY;
        }

        SafeLogs.d(TAG, "onStartCommand()", "action: " + action);

        switch (action) {
            case "RESTART_PHOTO_BACKUP":
                startPhotoBackup(intent);
                break;
            case "STOP_PHOTO_BACKUP":
                stopPhotoBackup();
                break;
            case "START_FOLDER_BACKUP":
                startFolderBackup(intent);
                break;
            case "STOP_FOLDER_BACKUP":
                stopFolderBackup();
                break;
            case "START_MANUAL_UPLOAD":
                startManualUpload();
                break;
            case "START_FILE_DOWNLOAD":
                startDownload();
                break;
            case "START_LOCAL_FILE_UPDATE":
                startLocalFileUpdate();
                break;
            case "STOP_TRANSFER":
                Bundle extras = intent.getExtras();
                stopById(extras);
                break;
        }

        return START_STICKY;
    }

    private void startPhotoBackup(Intent intent) {
        startForegroundNotification(FeatureDataSource.ALBUM_BACKUP);

        boolean isForce = true;
        boolean isRestart = false;

        Bundle bundle = intent.getExtras();
        if (bundle != null) {
            isForce = bundle.getBoolean("is_force", true);
            isRestart = bundle.getBoolean("is_restart", false);
        }

        if (isRestart) {
            // if running, then restart, if not run, start it.
            if (photoUploadFuture != null && !photoUploadFuture.isDone() && mediaBackupUploader != null) {
                //cancel
                mediaBackupUploader.stop();
                photoUploadFuture.cancel(true);

                boolean finalIsForce = isForce;
                photoUploadFuture.whenComplete(new BiConsumer<Void, Throwable>() {
                    @Override
                    public void accept(Void unused, Throwable throwable) {
                        launchAlbumBackup(finalIsForce);
                    }
                });
            } else {
                launchAlbumBackup(isForce);
            }
        } else {
            launchAlbumBackup(isForce);
        }
    }

    private void launchAlbumBackup(boolean isForce) {
        photoUploadFuture = runAlbumBackupTask(isForce, () -> {
            SafeLogs.d(TAG, "startPhotoUpload()", "upload complete");

            transferNotificationDispatcher.clearLater(FeatureDataSource.ALBUM_BACKUP);

            photoUploadFuture = null;
            mediaBackupUploader = null;
        });
    }

    private void stopPhotoBackup() {
        if (mediaBackupUploader != null) {
            mediaBackupUploader.stop();
        }

        startForegroundNotification(FeatureDataSource.ALBUM_BACKUP);

        if (photoUploadFuture != null && !photoUploadFuture.isDone()) {
            photoUploadFuture.cancel(true);
        } else {
            runTask(new Runnable() {
                @Override
                public void run() {
                    SafeLogs.d(TAG, "stopPhotoUpload()", "upload task is not running");
                }
            }, null);
        }
    }

    private void startFolderBackup(Intent intent) {
        startForegroundNotification(FeatureDataSource.FOLDER_BACKUP);

        boolean isForce = true;
        boolean isRestart = false;

        Bundle bundle = intent.getExtras();
        if (bundle != null) {
            isForce = bundle.getBoolean("is_force", true);
            isRestart = bundle.getBoolean("is_restart", false);
        }

        if (isRestart) {
            // if running, then restart, if not run, start it.
            if (folderUploadFuture != null && !folderUploadFuture.isDone() && folderBackupUploader != null) {
                //cancel
                folderBackupUploader.stop();
                folderUploadFuture.cancel(true);

                boolean finalIsForce = isForce;
                folderUploadFuture.whenComplete(new BiConsumer<Void, Throwable>() {
                    @Override
                    public void accept(Void unused, Throwable throwable) {
                        launchFolderBackup(finalIsForce);
                    }
                });

            } else {
                launchFolderBackup(isForce);
            }
        } else {
            //if running, then do nothing.
            if (folderUploadFuture != null && !folderUploadFuture.isDone() && folderBackupUploader != null) {
                return;
            }
            launchFolderBackup(isForce);
        }
    }

    private void launchFolderBackup(boolean isForce) {
        folderUploadFuture = runFolderBackupTask(isForce, () -> {
            SafeLogs.d(TAG, "startFolderBackup()", "upload complete");
            folderBackupUploader = null;
            folderUploadFuture = null;
            transferNotificationDispatcher.releaseAcquire(FeatureDataSource.FOLDER_BACKUP);
        });
    }

    private void stopFolderBackup() {
        if (folderBackupUploader != null) {
            folderBackupUploader.stop();
        }

        startForegroundNotification(FeatureDataSource.FOLDER_BACKUP);

        if (folderUploadFuture != null && !folderUploadFuture.isDone()) {
            folderUploadFuture.cancel(true);
        } else {
            runTask(new Runnable() {
                @Override
                public void run() {
                    SafeLogs.d(TAG, "stopFolderBackup()", "upload task is not running");
                }
            }, null);
        }
    }

    private void startManualUpload() {
        if (manualUploadFuture != null && !manualUploadFuture.isDone()) {
            return;
        }

        startForegroundNotification(FeatureDataSource.MANUAL_FILE_UPLOAD);

        manualUploadFuture = runFileUploadTask(() -> {
            SafeLogs.d(TAG, "startManualUpload()", "upload complete");
            fileUploader = null;
        });
    }

    private void startDownload() {
        if (fileDownloadFuture != null && !fileDownloadFuture.isDone()) {
            return;
        }

        startForegroundNotification(FeatureDataSource.DOWNLOAD);

        fileDownloadFuture = runDownloadTask(() -> {
            downloader = null;
            transferNotificationDispatcher.releaseAcquire(FeatureDataSource.DOWNLOAD);
            SafeLogs.d(TAG, "startFileDownload()", "download complete");
        });

    }

    private void startLocalFileUpdate() {
        if (localFileUpdateFuture != null && !localFileUpdateFuture.isDone()) {
            return;
        }

        startForegroundNotification(FeatureDataSource.AUTOMATIC_UPDATE_FILE_FROM_LOCAL);

        localFileUpdateFuture = runLocalFileUpdateTask(() -> {
            transferNotificationDispatcher.clearLater(FeatureDataSource.AUTOMATIC_UPDATE_FILE_FROM_LOCAL);
            transferNotificationDispatcher.releaseAcquire(FeatureDataSource.AUTOMATIC_UPDATE_FILE_FROM_LOCAL);
            localFileUpdateFuture = null;
            SafeLogs.d(TAG, "startLocalFileUpdate()", "update complete");
        });
    }

    private void startForegroundNotification(FeatureDataSource source) {
        NotificationInfo notificationInfo = transferNotificationDispatcher.getForegroundNotification(source);
        //start foreground service
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.Q) {
            ServiceCompat.startForeground(this, notificationInfo.id, notificationInfo.notification, ServiceInfo.FOREGROUND_SERVICE_TYPE_DATA_SYNC);
        } else {
            startForeground(notificationInfo.id, notificationInfo.notification);
        }
    }


    public void stopById(Bundle extras) {
        if (extras == null) {
            return;
        }

        String modelId = extras.getString("model");
        String dataSource = extras.getString("source");
        if (TextUtils.isEmpty(modelId) || TextUtils.isEmpty(dataSource)) {
            return;
        }

        if (FeatureDataSource.ALBUM_BACKUP.name().equals(dataSource)) {
            if (mediaBackupUploader != null) {
                mediaBackupUploader.stopById(modelId);
            }
        } else if (FeatureDataSource.FOLDER_BACKUP.name().equals(dataSource)) {

            if (folderBackupUploader != null) {
                folderBackupUploader.stopById(modelId);
            }
        } else if (FeatureDataSource.MANUAL_FILE_UPLOAD.name().equals(dataSource)) {
            if (fileUploader != null) {
                fileUploader.stopById(modelId);
            }
        } else if (FeatureDataSource.DOWNLOAD.name().equals(dataSource)) {
            if (downloader != null) {
                downloader.stopById(modelId);
            }
        }
    }

    private FileDownloader downloader;
    private MediaBackupUploader mediaBackupUploader;
    private FolderBackupUploader folderBackupUploader;
    private FileUploader fileUploader;
    private LocalFileUpdater localFileUpdater;

    private CompletableFuture<Void> runAlbumBackupTask(boolean isForce, Runnable onComplete) {
        return runTask(new Runnable() {
            @Override
            public void run() {
                boolean isEnable = AlbumBackupSharePreferenceHelper.readBackupSwitch();
                if (!isEnable) {
                    SafeLogs.e("album backup is disable");
                    return;
                }

                MediaBackupScanner scanner = new MediaBackupScanner(getApplicationContext());
                SeafException scanSeafException = scanner.scan(isForce);
                if (scanSeafException != SeafException.SUCCESS) {
                    SafeLogs.d(TAG, "runAlbumBackupTask()", "scan error: " + scanSeafException);
                } else {
                    SafeLogs.d(TAG, "runAlbumBackupTask()", "scan success");
                }

                mediaBackupUploader = new MediaBackupUploader(getApplicationContext(), transferNotificationDispatcher);
                SeafException uploadSeafException = mediaBackupUploader.upload();
                if (uploadSeafException != SeafException.SUCCESS) {
                    SafeLogs.d(TAG, "runAlbumBackupTask()", "scan error: " + uploadSeafException);
                } else {
                    SafeLogs.d(TAG, "runAlbumBackupTask()", "scan success");
                }
            }
        }, onComplete);
    }

    private CompletableFuture<Void> runFolderBackupTask(boolean isForce, Runnable onComplete) {
        return runTask(new Runnable() {
            @Override
            public void run() {
                boolean isEnable = FolderBackupSharePreferenceHelper.readBackupSwitch();
                if (!isEnable) {
                    SafeLogs.e("folder backup is disable");
                    return;
                }

                FolderBackupScanner scanner = new FolderBackupScanner(getApplicationContext());
                SeafException scanSeafException = scanner.scan(isForce);
                if (scanSeafException != SeafException.SUCCESS) {
                    SafeLogs.d(TAG, "runFolderBackupScanTask()", "scan error: " + scanSeafException);
                } else {
                    SafeLogs.d(TAG, "runFolderBackupScanTask()", "scan success");
                }

                folderBackupUploader = new FolderBackupUploader(getApplicationContext(), transferNotificationDispatcher);
                SeafException uploadSeafException = folderBackupUploader.upload();

                transferNotificationDispatcher.releaseAcquire(FeatureDataSource.FOLDER_BACKUP);
                if (uploadSeafException != SeafException.SUCCESS) {
                    SafeLogs.d(TAG, "runFolderBackupTask()", "upload error: " + uploadSeafException);
                } else {
                    SafeLogs.d(TAG, "runFolderBackupTask()", "upload success");
                }
            }
        }, onComplete);
    }

    private CompletableFuture<Void> runFileUploadTask(Runnable onComplete) {
        return runTask(new Runnable() {
            @Override
            public void run() {
                fileUploader = new FileUploader(getApplicationContext(), transferNotificationDispatcher);
                SeafException seafException = fileUploader.upload();
                transferNotificationDispatcher.releaseAcquire(FeatureDataSource.MANUAL_FILE_UPLOAD);
                if (seafException != SeafException.SUCCESS) {
                    SafeLogs.d(TAG, "runFileUploadTask()", "upload error: " + seafException);
                } else {
                    SafeLogs.d(TAG, "runFileUploadTask()", "upload success");
                }
            }
        }, onComplete);
    }

    private CompletableFuture<Void> runDownloadTask(Runnable onComplete) {
        return runTask(new Runnable() {
            @Override
            public void run() {
                downloader = new FileDownloader(getApplicationContext(), transferNotificationDispatcher);
                SeafException seafException = downloader.download();
                transferNotificationDispatcher.releaseAcquire(FeatureDataSource.DOWNLOAD);
                if (seafException != SeafException.SUCCESS) {
                    SafeLogs.d(TAG, "runDownloadTask()", "download error: " + seafException);
                } else {
                    SafeLogs.d(TAG, "runDownloadTask()", "download success");
                }
            }
        }, onComplete);
    }

    private CompletableFuture<Void> runLocalFileUpdateTask(Runnable onComplete) {
        return runTask(new Runnable() {
            @Override
            public void run() {
                localFileUpdater = new LocalFileUpdater(getApplicationContext(), transferNotificationDispatcher);
                SeafException seafException = localFileUpdater.upload();
                transferNotificationDispatcher.releaseAcquire(FeatureDataSource.AUTOMATIC_UPDATE_FILE_FROM_LOCAL);
                if (seafException != SeafException.SUCCESS) {
                    SafeLogs.d(TAG, "runLocalFileUpdateTask()", "upload error: " + seafException);
                } else {
                    SafeLogs.d(TAG, "runLocalFileUpdateTask()", "upload success");
                }
            }
        }, onComplete);
    }

    private CompletableFuture<Void> runTask(Runnable runnable, Runnable onComplete) {
        runningTaskCount.incrementAndGet();
        return CompletableFuture.runAsync(runnable, _executor).whenComplete(new BiConsumer<Void, Throwable>() {
            @Override
            public void accept(Void unused, Throwable throwable) {
                if (onComplete != null) onComplete.run();
                if (runningTaskCount.decrementAndGet() == 0) {
                    stopMyLife();
                    SafeLogs.e(TAG + " - all task complete");
                }
            }
        });
    }

    private void stopMyLife() {
        SafeLogs.e(TAG, "stopMyLife()");
        stopForeground(true);
        dismissAllNotification();
        stopSelf();
    }

    @Override
    public void onDestroy() {
        SafeLogs.e(TAG, "onDestroy()");
        cancel(photoUploadFuture);
        cancel(folderUploadFuture);
        cancel(fileDownloadFuture);
        cancel(manualUploadFuture);
        cancel(localFileUpdateFuture);

        if (_executor != null) _executor.shutdownNow();

        stopForeground(true);

        super.onDestroy();
    }

    private void dismissAllNotification() {
        SafeLogs.e(TAG, "dismissAllNotification()");
        transferNotificationDispatcher.clearAll(3000);
    }

    private void cancel(CompletableFuture<?> future) {
        if (future != null && !future.isDone()) {
            future.cancel(true);
        }
    }
}
