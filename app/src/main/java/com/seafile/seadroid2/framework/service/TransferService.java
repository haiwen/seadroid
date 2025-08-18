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

import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.enums.FeatureDataSource;
import com.seafile.seadroid2.enums.ServiceActionEnum;
import com.seafile.seadroid2.framework.executor.TaskExecutor;
import com.seafile.seadroid2.framework.notification.NotificationInfo;
import com.seafile.seadroid2.framework.service.download.FileDownloader;
import com.seafile.seadroid2.framework.service.upload.FileUploader;
import com.seafile.seadroid2.framework.service.upload.FolderBackupUploader;
import com.seafile.seadroid2.framework.service.upload.LocalFileUpdater;
import com.seafile.seadroid2.framework.service.upload.MediaBackupUploader;
import com.seafile.seadroid2.framework.service.upload.ShareToSeafileUploader;
import com.seafile.seadroid2.framework.util.SafeLogs;
import com.seafile.seadroid2.framework.worker.GlobalTransferCacheList;
import com.seafile.seadroid2.framework.worker.queue.TransferModel;

import java.lang.ref.WeakReference;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.BiConsumer;

@Deprecated
public class TransferService extends EventService {
    private static final String TAG = "TransferService";

    private ForegroundServiceNotificationDispatcher transferNotificationDispatcher;

    private static final ConcurrentHashMap<FeatureDataSource, CompletableFuture<Void>> _activeTasks = new ConcurrentHashMap<>();
    private static final AtomicBoolean _isRunning = new AtomicBoolean(false);
    private final AtomicInteger runningTaskCount = new AtomicInteger(0);

    public static ConcurrentHashMap<FeatureDataSource, CompletableFuture<Void>> getActiveTasks() {
        return _activeTasks;
    }

    private ThreadPoolExecutor _executor;


    public static void startManualUploadService(Context context) {
        startService(context, ServiceActionEnum.START_MANUAL_UPLOAD.name());
    }

    public static void startDownloadService(Context context) {
        startService(context, ServiceActionEnum.START_FILE_DOWNLOAD.name());
    }

    public static void stopDownloadService(Context context) {
        startService(context, ServiceActionEnum.STOP_FILE_DOWNLOAD.name());
    }

    public static void stopTransfer(Context context, TransferModel model) {
        if (model == null) {
            throw new IllegalArgumentException("model is null");
        }

        Bundle bundle = new Bundle();
        bundle.putString("model", model.getId());
        bundle.putString("source", model.data_source.toString());
        startService(context, ServiceActionEnum.STOP_TRANSFER.name(), bundle);
    }

    public static void stopService(Context context) {
        if (context == null) {
            throw new IllegalArgumentException("context is null");
        }

        if (!_isRunning.get()) {
            SafeLogs.e(TAG, "stopService()", "service is not running");
            return;
        }

        startService(context, ServiceActionEnum.STOP_SERVICE.name());
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

        transferNotificationDispatcher = new ForegroundServiceNotificationDispatcher(getApplicationContext());

        _executor = TaskExecutor.getInstance().getExecutor();
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

        _isRunning.set(true);

        if (intent == null) return START_STICKY;

        String action = intent.getAction();
        if (action == null) {
            return START_STICKY;
        }

        ServiceActionEnum actionEnum = ServiceActionEnum.valueOf(action);
        SafeLogs.d(TAG, "onStartCommand()", "action: " + action);

        switch (actionEnum) {
            case STOP_SERVICE:
                stopAll();
                break;

            case START_MANUAL_UPLOAD:
                manageTask(FeatureDataSource.MANUAL_FILE_UPLOAD, intent.getExtras());

                break;
            case START_FILE_DOWNLOAD:
                manageTask(FeatureDataSource.DOWNLOAD, intent.getExtras());

                break;
            case STOP_FILE_DOWNLOAD:
                stopDownload();

                break;
            case STOP_TRANSFER:
                stopById(intent.getExtras());
                break;
        }

        return START_STICKY;
    }

    private void stopDownload() {
        startForegroundNotification(FeatureDataSource.DOWNLOAD);

        CompletableFuture<Void> future = getActiveTasks().get(FeatureDataSource.DOWNLOAD);
        if (future != null && !future.isDone() && downloader != null) {
            downloader.stop();

            future.cancel(true);
        } else {
            runTask(new Runnable() {
                @Override
                public void run() {
                    SafeLogs.d(TAG, "stopDownload()", "download task is not running");
                }
            }, null);
        }
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
            throw new RuntimeException("You must provide a valid bundle.");
        }

        String modelId = extras.getString("model");
        String dataSource = extras.getString("source");

        if (TextUtils.isEmpty(modelId) || TextUtils.isEmpty(dataSource)) {
            throw new RuntimeException("You must provide a valid parameter.");
        }

        if (FeatureDataSource.ALBUM_BACKUP.name().equals(dataSource)) {
            startForegroundNotification(FeatureDataSource.ALBUM_BACKUP);

            if (mediaBackupUploader != null) {
                mediaBackupUploader.stopById(modelId);
            } else {
                GlobalTransferCacheList.ALBUM_BACKUP_QUEUE.remove(modelId);
            }

        } else if (FeatureDataSource.FOLDER_BACKUP.name().equals(dataSource)) {
            startForegroundNotification(FeatureDataSource.FOLDER_BACKUP);

            if (folderBackupUploader != null) {
                folderBackupUploader.stopById(modelId);
            } else {
                GlobalTransferCacheList.FOLDER_BACKUP_QUEUE.remove(modelId);
            }

        } else if (FeatureDataSource.MANUAL_FILE_UPLOAD.name().equals(dataSource)) {
            startForegroundNotification(FeatureDataSource.MANUAL_FILE_UPLOAD);

            if (fileUploader != null) {
                fileUploader.stopById(modelId);
            } else {
                GlobalTransferCacheList.FILE_UPLOAD_QUEUE.remove(modelId);
            }

        } else if (FeatureDataSource.DOWNLOAD.name().equals(dataSource)) {
            startForegroundNotification(FeatureDataSource.DOWNLOAD);

            if (downloader != null) {
                downloader.stopById(modelId);
            } else {
                GlobalTransferCacheList.DOWNLOAD_QUEUE.remove(modelId);
            }
        } else {
            throw new RuntimeException("You must provide a valid data source.");
        }

        SafeLogs.d(TAG, "stopById()", "stopped: " + modelId);
    }

    private FileDownloader downloader;
    private MediaBackupUploader mediaBackupUploader;
    private FolderBackupUploader folderBackupUploader;
    private FileUploader fileUploader;
    private LocalFileUpdater localFileUpdater;
    private ShareToSeafileUploader shareToSeafileUploader;


    private void manageTask(FeatureDataSource source, Bundle extras) {
        startForegroundNotification(source);

        CompletableFuture<Void> existingTask = getActiveTasks().get(source);
        if (existingTask != null && !existingTask.isDone()) {
            SafeLogs.d(TAG, "manageTask()", source.name(), "task is already running");
            return;
        }

        if (source == FeatureDataSource.MANUAL_FILE_UPLOAD) {
            runFileUploadTask();
        } else if (source == FeatureDataSource.DOWNLOAD) {
            runDownloadTask();
        }
    }

    private void runFileUploadTask() {
        CompletableFuture<Void> completableFuture = runTask(new Runnable() {
            @Override
            public void run() {
                fileUploader = new FileUploader(getApplicationContext(), transferNotificationDispatcher);
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
                transferNotificationDispatcher.releaseAcquire(FeatureDataSource.MANUAL_FILE_UPLOAD);

            }
        });
        getActiveTasks().put(FeatureDataSource.MANUAL_FILE_UPLOAD, completableFuture);

    }

    private void runDownloadTask() {
        CompletableFuture<Void> completableFuture = runTask(new Runnable() {
            @Override
            public void run() {
                downloader = new FileDownloader(getApplicationContext(), transferNotificationDispatcher);
                SeafException seafException = downloader.download();

                if (seafException != SeafException.SUCCESS) {
                    SafeLogs.d(TAG, "runDownloadTask()", "download error: " + seafException);
                } else {
                    SafeLogs.d(TAG, "runDownloadTask()", "download success");
                }
            }
        }, new Runnable() {
            @Override
            public void run() {
                downloader = null;
                transferNotificationDispatcher.releaseAcquire(FeatureDataSource.DOWNLOAD);
                getActiveTasks().remove(FeatureDataSource.DOWNLOAD);
            }
        });
        getActiveTasks().put(FeatureDataSource.DOWNLOAD, completableFuture);
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
                            stopThisService();
                            SafeLogs.e(TAG + " - all task complete");
                        }
                    }
                });
    }

    @Override
    public void onTimeout(int startId, int fgsType) {
        super.onTimeout(startId, fgsType);
        SafeLogs.e(TAG, "onTimeout()", "startId: " + startId + ", fgsType: " + fgsType);
        stopThisService();
    }

    private void stopAll() {
        //any one data source
        startForegroundNotification(FeatureDataSource.SHARE_FILE_TO_SEAFILE);

        getActiveTasks().forEach(new BiConsumer<FeatureDataSource, CompletableFuture<Void>>() {
            @Override
            public void accept(FeatureDataSource featureDataSource, CompletableFuture<Void> voidCompletableFuture) {
                if (voidCompletableFuture != null && !voidCompletableFuture.isDone()) {
                    voidCompletableFuture.cancel(true);
                }
            }
        });
    }

    private void stopThisService() {
        SafeLogs.e(TAG, "stopThisService()");
        stopForeground(true);
        dismissAllNotification();
        stopSelf();
    }

    @Override
    public void onDestroy() {
        SafeLogs.e(TAG, "onDestroy()");

        getActiveTasks().forEach((key, value) -> {
            cancel(value);
        });

        stopForeground(true);

        _isRunning.set(false);

        super.onDestroy();
    }

    private void dismissAllNotification() {
        SafeLogs.e(TAG, "dismissAllNotification()");
        transferNotificationDispatcher.clearDelay();
    }

    private void cancel(CompletableFuture<?> future) {
        if (future != null && !future.isDone()) {
            future.cancel(true);
        }
    }
}
