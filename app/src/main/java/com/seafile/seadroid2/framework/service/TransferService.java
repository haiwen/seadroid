package com.seafile.seadroid2.framework.service;

import android.app.Notification;
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

import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.enums.TransferDataSource;
import com.seafile.seadroid2.framework.datastore.sp_livedata.AlbumBackupSharePreferenceHelper;
import com.seafile.seadroid2.framework.datastore.sp_livedata.FolderBackupSharePreferenceHelper;
import com.seafile.seadroid2.framework.notification.AlbumBackupNotificationHelper;
import com.seafile.seadroid2.framework.notification.DownloadNotificationHelper;
import com.seafile.seadroid2.framework.notification.FileUploadNotificationHelper;
import com.seafile.seadroid2.framework.notification.FolderBackupNotificationHelper;
import com.seafile.seadroid2.framework.notification.LocalFileUpdateNotificationHelper;
import com.seafile.seadroid2.framework.service.download.FileDownloader;
import com.seafile.seadroid2.framework.service.upload.FileUploader;
import com.seafile.seadroid2.framework.service.upload.FolderBackupScanner;
import com.seafile.seadroid2.framework.service.upload.FolderBackupUploader;
import com.seafile.seadroid2.framework.service.upload.LocalFileUpdater;
import com.seafile.seadroid2.framework.service.upload.MediaBackupScanner;
import com.seafile.seadroid2.framework.service.upload.MediaBackupUploader;
import com.seafile.seadroid2.framework.util.SafeLogs;
import com.seafile.seadroid2.framework.worker.queue.TransferModel;

import java.lang.ref.WeakReference;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.BiConsumer;

public class TransferService extends EventService {
    private final String TAG = "TransferService";

    private FileUploadNotificationHelper fileUploadNotificationHelper;
    private FolderBackupNotificationHelper folderBackupNotificationHelper;
    private AlbumBackupNotificationHelper albumBackupNotificationHelper;
    private DownloadNotificationHelper downloadNotificationHelper;
    private LocalFileUpdateNotificationHelper localFileUpdateNotificationHelper;

    private CompletableFuture<Void> photoUploadFuture;
    private CompletableFuture<Void> folderUploadFuture;
    private CompletableFuture<Void> fileDownloadFuture;
    private CompletableFuture<Void> manualUploadFuture;
    private CompletableFuture<Void> localFileUpdateFuture;

    private final AtomicInteger runningTaskCount = new AtomicInteger(0);
    private final AtomicBoolean foregroundStarted = new AtomicBoolean(false);

    private ExecutorService _executor;

    public static void startPhotoBackupService(Context context) {
        startService(context, "START_PHOTO_BACKUP");
    }

    public static void restartPhotoBackupService(Context context) {
        startService(context, "RESTART_PHOTO_BACKUP");
    }

    public static void stopPhotoBackupService(Context context) {
        startService(context, "STOP_PHOTO_BACKUP");
    }

    public static void startFolderBackupService(Context context) {
        startService(context, "START_FOLDER_BACKUP");
    }

    public static void restartFolderBackupService(Context context) {
        startService(context, "RESTART_FOLDER_BACKUP");
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

        initNotificationHelper();


        _executor = Executors.newFixedThreadPool(5, new ThreadFactory() {
            private final AtomicInteger count = new AtomicInteger(1);

            @Override
            public Thread newThread(Runnable r) {
                return new Thread(r, "TransferWorker-" + count.getAndIncrement());
            }
        });
    }

    private void initNotificationHelper() {
        if (fileUploadNotificationHelper != null) {
            return;
        }

        //init notification helper
        fileUploadNotificationHelper = new FileUploadNotificationHelper(this);
        folderBackupNotificationHelper = new FolderBackupNotificationHelper(this);
        albumBackupNotificationHelper = new AlbumBackupNotificationHelper(this);
        downloadNotificationHelper = new DownloadNotificationHelper(this);
        localFileUpdateNotificationHelper = new LocalFileUpdateNotificationHelper(this);
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
            case "START_PHOTO_BACKUP":
                startPhotoBackup(intent);
                break;
            case "RESTART_PHOTO_BACKUP":
                restartPhotoBackup(intent);
                break;
            case "STOP_PHOTO_BACKUP":
                stopPhotoBackup();
                break;
            case "START_FOLDER_BACKUP":
                startFolderBackup(intent);
                break;
            case "RESTART_FOLDER_BACKUP":
                restartFolderBackup(intent);
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
        boolean isEnable = AlbumBackupSharePreferenceHelper.readBackupSwitch();
        if (!isEnable) {
            SafeLogs.e("album backup is disable");
            return;
        }

        if (photoUploadFuture != null && !photoUploadFuture.isDone()) {
            return;
        }

        if (foregroundStarted.compareAndSet(false, true)) {
            dispatchForeground(albumBackupNotificationHelper.getNotificationId(), albumBackupNotificationHelper.getNotification());
        } else {
            albumBackupNotificationHelper.showDefaultNotification();
        }

        boolean isForce = true;
        if (intent != null) {
            Bundle bundle = intent.getExtras();
            if (bundle != null) {
                isForce = bundle.getBoolean("isForce", true);
            }
        }

        photoUploadFuture = runAlbumBackupTask(isForce, () -> {
            SafeLogs.d(TAG, "startPhotoUpload()", "upload complete");
        });
    }

    private void restartPhotoBackup(Intent intent) {
        boolean isForce = true;
        if (intent != null) {
            Bundle bundle = intent.getExtras();
            if (bundle != null) {
                isForce = bundle.getBoolean("isForce", true);
            }
        }


        if (photoUploadFuture == null || photoUploadFuture.isDone()) {
            startPhotoBackup(intent);
        } else {

            if (foregroundStarted.compareAndSet(false, true)) {
                dispatchForeground(albumBackupNotificationHelper.getNotificationId(), albumBackupNotificationHelper.getNotification());
            } else {
                albumBackupNotificationHelper.showDefaultNotification();
            }

            //cancel
            photoUploadFuture.cancel(true);

            boolean finalIsForce = isForce;
            photoUploadFuture.whenComplete(new BiConsumer<Void, Throwable>() {
                @Override
                public void accept(Void unused, Throwable throwable) {
                    photoUploadFuture = runAlbumBackupTask(finalIsForce, () -> {
                        SafeLogs.d(TAG, "startPhotoUpload()", "upload complete");
                        mediaBackupUploader = null;
                    });
                }
            });
        }
    }

    private void stopPhotoBackup() {
        if (mediaBackupUploader != null) {
            mediaBackupUploader.stop();
        }

        if (foregroundStarted.compareAndSet(false, true)) {
            dispatchForeground(albumBackupNotificationHelper.getNotificationId(), albumBackupNotificationHelper.getNotification());
        } else {
            albumBackupNotificationHelper.showDefaultNotification();
        }

        if (photoUploadFuture != null && !photoUploadFuture.isDone()) {
            photoUploadFuture.cancel(true);
            photoUploadFuture.whenComplete(new BiConsumer<Void, Throwable>() {
                @Override
                public void accept(Void unused, Throwable throwable) {
                    photoUploadFuture = null;
                    mediaBackupUploader = null;
                    albumBackupNotificationHelper.cancel();
                }
            });
        }
    }

    private void startFolderBackup(Intent intent) {
        boolean isEnable = FolderBackupSharePreferenceHelper.readBackupSwitch();
        if (!isEnable) {
            SafeLogs.e("folder backup is disable");
            return;
        }

        if (folderUploadFuture != null && !folderUploadFuture.isDone()) {
            return;
        }

        if (foregroundStarted.compareAndSet(false, true)) {
            dispatchForeground(folderBackupNotificationHelper.getNotificationId(), folderBackupNotificationHelper.getNotification());
        } else {
            folderBackupNotificationHelper.showDefaultNotification();
        }

        boolean isForce = true;

        Bundle bundle = intent.getExtras();
        if (bundle != null) {
            isForce = bundle.getBoolean("isForce", true);
        }

        folderUploadFuture = runFolderBackupTask(isForce, () -> {
            SafeLogs.d(TAG, "startFolderBackup()", "upload complete");
            folderBackupUploader = null;
        });
    }

    private void restartFolderBackup(Intent intent) {
        boolean isForce = true;
        Bundle bundle = intent.getExtras();
        if (bundle != null) {
            isForce = bundle.getBoolean("isForce", true);
        }

        if (folderUploadFuture == null || folderUploadFuture.isDone()) {
            startFolderBackup(intent);
        } else {

            if (foregroundStarted.compareAndSet(false, true)) {
                dispatchForeground(folderBackupNotificationHelper.getNotificationId(), folderBackupNotificationHelper.getNotification());
            } else {
                folderBackupNotificationHelper.showDefaultNotification();
            }

            //cancel
            folderUploadFuture.cancel(true);

            boolean finalIsForce = isForce;
            folderUploadFuture.whenComplete(new BiConsumer<Void, Throwable>() {
                @Override
                public void accept(Void unused, Throwable throwable) {
                    folderUploadFuture = runFolderBackupTask(finalIsForce, () -> {
                        SafeLogs.d(TAG, "startFolderBackup()", "upload complete");
                        folderBackupUploader = null;
                    });
                }
            });
        }
    }

    private void stopFolderBackup() {
        if (folderBackupUploader != null) {
            folderBackupUploader.stop();
        }

        if (foregroundStarted.compareAndSet(false, true)) {
            dispatchForeground(folderBackupNotificationHelper.getNotificationId(), folderBackupNotificationHelper.getNotification());
        } else {
            folderBackupNotificationHelper.showDefaultNotification();
        }

        if (folderUploadFuture != null && !folderUploadFuture.isDone()) {
            folderUploadFuture.cancel(true);
            folderUploadFuture.whenComplete(new BiConsumer<Void, Throwable>() {
                @Override
                public void accept(Void unused, Throwable throwable) {
                    folderUploadFuture = null;
                    folderBackupUploader = null;

                    folderBackupNotificationHelper.cancel();
                }
            });
        }
    }

    private void startManualUpload() {
        if (manualUploadFuture != null && !manualUploadFuture.isDone()) {
            return;
        }

        if (foregroundStarted.compareAndSet(false, true)) {
            dispatchForeground(fileUploadNotificationHelper.getNotificationId(), fileUploadNotificationHelper.getNotification());
        } else {
            fileUploadNotificationHelper.showDefaultNotification();
        }

        manualUploadFuture = runFileUploadTask(() -> {
            SafeLogs.d(TAG, "startManualUpload()", "upload complete");
            fileUploader = null;
        });
    }

    private void startDownload() {
        if (fileDownloadFuture != null && !fileDownloadFuture.isDone()) {
            return;
        }

        if (foregroundStarted.compareAndSet(false, true)) {
            dispatchForeground(downloadNotificationHelper.getNotificationId(), downloadNotificationHelper.getNotification());
        } else {
            downloadNotificationHelper.showDefaultNotification();
        }
        fileDownloadFuture = runDownloadTask(() -> {
            downloader = null;
            SafeLogs.d(TAG, "startFileDownload()", "download complete");
        });

    }

    private void startLocalFileUpdate() {
        if (localFileUpdateFuture != null && !localFileUpdateFuture.isDone()) {
            return;
        }
        if (foregroundStarted.compareAndSet(false, true)) {
            dispatchForeground(localFileUpdateNotificationHelper.getNotificationId(), localFileUpdateNotificationHelper.getNotification());
        } else {
            localFileUpdateNotificationHelper.showDefaultNotification();
        }
        localFileUpdateFuture = runLocalFileUpdateTask(() -> {
            localFileUpdateNotificationHelper.cancel();
            localFileUpdateFuture = null;
            SafeLogs.d(TAG, "startLocalFileUpdate()", "update complete");
        });
    }

    private void dispatchForeground(int id, Notification notification) {
        //start foreground service
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.Q) {
            ServiceCompat.startForeground(this, id, notification, ServiceInfo.FOREGROUND_SERVICE_TYPE_DATA_SYNC);
        } else {
            startForeground(id, notification);
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

        if (TransferDataSource.ALBUM_BACKUP.name().equals(dataSource)) {
            if (mediaBackupUploader != null) {
                mediaBackupUploader.stopById(modelId);
            }
        } else if (TransferDataSource.FOLDER_BACKUP.name().equals(dataSource)) {

            if (folderBackupUploader != null) {
                folderBackupUploader.stopById(modelId);
            }
        } else if (TransferDataSource.FILE_BACKUP.name().equals(dataSource)) {
            if (fileUploader != null) {
                fileUploader.stopById(modelId);
            }
        } else if (TransferDataSource.DOWNLOAD.name().equals(dataSource)) {
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
        CompletableFuture<Void> scanFuture = runAlbumBackupScanTask(isForce, () -> {
            SafeLogs.d(TAG, "runAlbumBackupTask()", "scan complete");
        });
        Runnable uploadRunnable = getAlbumBackupUploadRunnable();
        return scanFuture.thenRun(uploadRunnable).whenComplete(new BiConsumer<Void, Throwable>() {
            @Override
            public void accept(Void unused, Throwable throwable) {
                SafeLogs.d(TAG, "runAlbumBackupTask()", "upload complete");
                if (onComplete != null) onComplete.run();
            }
        });
    }

    private CompletableFuture<Void> runAlbumBackupScanTask(boolean isForce, Runnable onComplete) {
        return runTask(new Runnable() {
            @Override
            public void run() {
                MediaBackupScanner scanner = new MediaBackupScanner(getApplicationContext(), albumBackupNotificationHelper);
                SeafException seafException = scanner.scan(isForce);
                if (seafException != SeafException.SUCCESS) {
                    SafeLogs.d(TAG, "runAlbumBackupTask()", "scan error: " + seafException);
                } else {
                    SafeLogs.d(TAG, "runAlbumBackupTask()", "scan success");
                }
            }
        }, onComplete);
    }

    private Runnable getAlbumBackupUploadRunnable() {
        return new Runnable() {
            @Override
            public void run() {
                mediaBackupUploader = new MediaBackupUploader(getApplicationContext(), albumBackupNotificationHelper);
                SeafException seafException = mediaBackupUploader.upload();
                if (seafException != SeafException.SUCCESS) {
                    albumBackupNotificationHelper.cancel();
                    SafeLogs.d(TAG, "runAlbumBackupTask()", "upload error: " + seafException);
                } else {
                    SafeLogs.d(TAG, "runAlbumBackupTask()", "upload success");
                }
            }
        };
    }

    private CompletableFuture<Void> runFolderBackupTask(boolean isForce, Runnable onComplete) {
        CompletableFuture<Void> scanFuture = runFolderBackupScanTask(isForce, () -> {
            SafeLogs.d(TAG, "runFolderBackupTask()", "scan complete");
        });
        Runnable uploadRunnable = getFolderBackupUploadRunnable();
        return scanFuture.thenRun(uploadRunnable).whenComplete(new BiConsumer<Void, Throwable>() {
            @Override
            public void accept(Void unused, Throwable throwable) {
                SafeLogs.d(TAG, "runFolderBackupTask()", "upload complete");
                if (onComplete != null) onComplete.run();
            }
        });

    }

    private CompletableFuture<Void> runFolderBackupScanTask(boolean isForce, Runnable onComplete) {
        return runTask(new Runnable() {
            @Override
            public void run() {
                FolderBackupScanner scanner = new FolderBackupScanner(getApplicationContext(), folderBackupNotificationHelper);
                SeafException scanSeafException = scanner.scan(isForce);
                if (scanSeafException != SeafException.SUCCESS) {
                    SafeLogs.d(TAG, "runFolderBackupScanTask()", "scan error: " + scanSeafException);
                } else {
                    SafeLogs.d(TAG, "runFolderBackupScanTask()", "scan success");
                }
            }
        }, onComplete);
    }

    private Runnable getFolderBackupUploadRunnable() {
        return new Runnable() {
            @Override
            public void run() {
                folderBackupUploader = new FolderBackupUploader(getApplicationContext(), folderBackupNotificationHelper);
                SeafException uploadSeafException = folderBackupUploader.upload();
                if (uploadSeafException != SeafException.SUCCESS) {
                    SafeLogs.d(TAG, "runFolderBackupTask()", "upload error: " + uploadSeafException);
                } else {
                    SafeLogs.d(TAG, "runFolderBackupTask()", "upload success");
                }
            }
        };
    }

    private CompletableFuture<Void> runFileUploadTask(Runnable onComplete) {
        return runTask(new Runnable() {
            @Override
            public void run() {
                fileUploader = new FileUploader(getApplicationContext(), fileUploadNotificationHelper);
                SeafException seafException = fileUploader.upload();
                if (seafException != SeafException.SUCCESS) {
                    fileUploadNotificationHelper.cancel();
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
                downloader = new FileDownloader(getApplicationContext(), downloadNotificationHelper);
                SeafException seafException = downloader.download();
                if (seafException != SeafException.SUCCESS) {
                    downloadNotificationHelper.cancel();
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
                localFileUpdater = new LocalFileUpdater(getApplicationContext(), localFileUpdateNotificationHelper);
                SeafException seafException = localFileUpdater.upload();
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
        return CompletableFuture.runAsync(runnable, _executor).thenRun(new Runnable() {
            @Override
            public void run() {
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
        int delay = 1000;
        albumBackupNotificationHelper.cancel(delay);
        folderBackupNotificationHelper.cancel(delay);
        fileUploadNotificationHelper.cancel(delay);
        downloadNotificationHelper.cancel(delay);
        localFileUpdateNotificationHelper.cancel(delay);
    }

    private void cancel(CompletableFuture<?> future) {
        if (future != null && !future.isDone()) {
            future.cancel(true);
        }
    }
}
