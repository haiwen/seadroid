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

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.enums.FeatureDataSource;
import com.seafile.seadroid2.enums.ServiceActionEnum;
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
import com.seafile.seadroid2.framework.service.upload.ShareToSeafileUploader;
import com.seafile.seadroid2.framework.util.SafeLogs;
import com.seafile.seadroid2.framework.worker.queue.TransferModel;

import java.lang.ref.WeakReference;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.BiConsumer;

public class TransferService extends EventService {
    private final String TAG = "TransferService";

    private TransferNotificationDispatcher transferNotificationDispatcher;

    private static final ConcurrentHashMap<FeatureDataSource, CompletableFuture<Void>> _activeTasks = new ConcurrentHashMap<>();
    private static final AtomicBoolean _isRunning = new AtomicBoolean(false);
    private final AtomicInteger runningTaskCount = new AtomicInteger(0);

    public static boolean getServiceRunning() {
        return _isRunning.get();
    }

    public static ConcurrentHashMap<FeatureDataSource, CompletableFuture<Void>> getActiveTasks() {
        return _activeTasks;
    }

    private ThreadPoolExecutor _executor;
    // 使用线程池工厂创建可配置的线程池
    private static final int CORE_POOL_SIZE = Runtime.getRuntime().availableProcessors();
    private static final int MAX_POOL_SIZE = CORE_POOL_SIZE * 2;
    private static final long KEEP_ALIVE_TIME = 30L;

    public static void restartPhotoBackupService(Context context) {
        Bundle bundle = new Bundle();
        bundle.putBoolean("is_force", true);
        bundle.putBoolean("is_restart", true);
        startService(context, ServiceActionEnum.RESTART_PHOTO_BACKUP.name(), bundle);
    }

    public static void stopPhotoBackupService(Context context) {
        startService(context, ServiceActionEnum.STOP_PHOTO_BACKUP.name());
    }

    public static void restartFolderBackupService(Context context, boolean isForce) {
        Bundle bundle = new Bundle();
        bundle.putBoolean("is_force", isForce);
        bundle.putBoolean("is_restart", true);
        startService(context, ServiceActionEnum.RESTART_FOLDER_BACKUP.name(), bundle);
    }

    public static void stopFolderBackupService(Context context) {
        startService(context, ServiceActionEnum.STOP_FOLDER_BACKUP.name());
    }

    public static void startManualUploadService(Context context) {
        startService(context, ServiceActionEnum.START_MANUAL_UPLOAD.name());
    }

    public static void startShareToSeafileUploadService(Context context) {
        startService(context, ServiceActionEnum.START_SHARE_TO_SEAFILE_UPLOAD.name());
    }

    public static void startDownloadService(Context context) {
        startService(context, ServiceActionEnum.START_FILE_DOWNLOAD.name());
    }

    public static void startLocalFileUpdateService(Context context) {
        startService(context, ServiceActionEnum.START_LOCAL_FILE_UPDATE.name());
    }

    public static void stopTransfer(Context context, TransferModel model) {
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


        _executor = new ThreadPoolExecutor(
                CORE_POOL_SIZE,
                MAX_POOL_SIZE,
                KEEP_ALIVE_TIME,
                TimeUnit.SECONDS,
                new LinkedBlockingQueue<>(20), // 限制队列长度
                new ThreadFactory() {
                    private final AtomicInteger count = new AtomicInteger(1);

                    @Override
                    public Thread newThread(Runnable r) {
                        Thread thread = new Thread(r, "TransferService-Task-" + count.getAndIncrement());
                        // 设置线程优先级避免影响UI
                        thread.setPriority(Thread.MIN_PRIORITY + (Thread.NORM_PRIORITY - Thread.MIN_PRIORITY) / 2);
                        return thread;
                    }
                }
        );
        _executor.allowCoreThreadTimeOut(true);
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
            case RESTART_PHOTO_BACKUP:
                startPhotoBackup(intent);

                break;
            case STOP_PHOTO_BACKUP:
                stopPhotoBackup();

                break;
            case RESTART_FOLDER_BACKUP:
                startFolderBackup(intent);

                break;
            case STOP_FOLDER_BACKUP:
                stopFolderBackup();

                break;
            case START_MANUAL_UPLOAD:
                manageTask(FeatureDataSource.MANUAL_FILE_UPLOAD, intent.getExtras());

                break;
            case START_SHARE_TO_SEAFILE_UPLOAD:
                manageTask(FeatureDataSource.SHARE_FILE_TO_SEAFILE, intent.getExtras());

                break;
            case START_FILE_DOWNLOAD:
                manageTask(FeatureDataSource.DOWNLOAD, intent.getExtras());

                break;
            case START_LOCAL_FILE_UPDATE:
                manageTask(FeatureDataSource.AUTO_UPDATE_LOCAL_FILE, intent.getExtras());

                break;
            case STOP_TRANSFER:
                Bundle extras = intent.getExtras();
                stopById(extras);
                break;
        }

        return START_STICKY;
    }

    private void startPhotoBackup(Intent intent) {

        boolean isRestart = false;
        Bundle extras = intent.getExtras();
        if (extras != null) {
            isRestart = extras.getBoolean("is_restart", false);
        }

        if (isRestart) {
            // if running, then restart, if not run, start it.
            CompletableFuture<Void> future = getActiveTasks().get(FeatureDataSource.ALBUM_BACKUP);
            if (future != null && !future.isDone() && mediaBackupUploader != null) {
                //cancel
                mediaBackupUploader.stop();
                future.cancel(true);

                future.whenComplete(new BiConsumer<Void, Throwable>() {
                    @Override
                    public void accept(Void unused, Throwable throwable) {
                        launchAlbumBackup(extras);
                    }
                });
            } else {
                launchAlbumBackup(extras);
            }
        } else {
            launchAlbumBackup(extras);
        }
    }

    private void launchAlbumBackup(Bundle extras) {
        manageTask(FeatureDataSource.ALBUM_BACKUP, extras);
    }

    private void stopPhotoBackup() {
        startForegroundNotification(FeatureDataSource.ALBUM_BACKUP);

        CompletableFuture<Void> future = getActiveTasks().get(FeatureDataSource.ALBUM_BACKUP);
        if (future != null && !future.isDone() && mediaBackupUploader != null) {
            mediaBackupUploader.stop();
            future.cancel(true);
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
        boolean isRestart = false;

        Bundle extras = intent.getExtras();
        if (extras != null) {
            isRestart = extras.getBoolean("is_restart", false);
        }

        if (isRestart) {
            // if running, then restart, if not run, start it.
            CompletableFuture<Void> folderUploadFuture = getActiveTasks().get(FeatureDataSource.FOLDER_BACKUP);
            if (folderUploadFuture != null && !folderUploadFuture.isDone() && folderBackupUploader != null) {
                //cancel
                folderBackupUploader.stop();
                folderUploadFuture.cancel(true);

                folderUploadFuture.whenComplete(new BiConsumer<Void, Throwable>() {
                    @Override
                    public void accept(Void unused, Throwable throwable) {
                        launchFolderBackup(extras);
                    }
                });

            } else {
                launchFolderBackup(extras);
            }
        } else {
            launchFolderBackup(extras);
        }
    }

    private void launchFolderBackup(Bundle extras) {
        manageTask(FeatureDataSource.FOLDER_BACKUP, extras);
    }

    private void stopFolderBackup() {
        startForegroundNotification(FeatureDataSource.FOLDER_BACKUP);

        CompletableFuture<Void> future = getActiveTasks().get(FeatureDataSource.FOLDER_BACKUP);
        if (future != null && !future.isDone() && folderBackupUploader != null) {
            folderBackupUploader.stop();

            future.cancel(true);
        } else {
            runTask(new Runnable() {
                @Override
                public void run() {
                    SafeLogs.d(TAG, "stopFolderBackup()", "upload task is not running");
                }
            }, null);
        }
    }


    private void startForegroundNotification(FeatureDataSource source) {
        String scanning = getApplicationContext().getString(R.string.is_scanning);
        NotificationInfo notificationInfo = transferNotificationDispatcher.getForegroundNotification(source, scanning);
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

        if (source == FeatureDataSource.ALBUM_BACKUP) {
            runAlbumBackupTask(extras);
        } else if (source == FeatureDataSource.FOLDER_BACKUP) {
            runFolderBackupTask(extras);
        } else if (source == FeatureDataSource.MANUAL_FILE_UPLOAD) {
            runFileUploadTask();
        } else if (source == FeatureDataSource.DOWNLOAD) {
            runDownloadTask();
        } else if (source == FeatureDataSource.AUTO_UPDATE_LOCAL_FILE) {
            runLocalFileUpdateTask();
        } else if (source == FeatureDataSource.SHARE_FILE_TO_SEAFILE) {
            runShareToSeafileUploadTask();
        }
    }

    private void runAlbumBackupTask(Bundle extras) {

        boolean isForce = true;
        if (extras != null) {
            isForce = extras.getBoolean("is_force", true);
        }

        boolean finalIsForce = isForce;
        CompletableFuture<Void> completableFuture = runTask(new Runnable() {
            @Override
            public void run() {
                boolean isEnable = AlbumBackupSharePreferenceHelper.readBackupSwitch();
                if (!isEnable) {
                    SafeLogs.e("album backup is disable");
                    return;
                }

                MediaBackupScanner scanner = new MediaBackupScanner(getApplicationContext());
                SeafException scanSeafException = scanner.scan(finalIsForce);
                if (scanSeafException != SeafException.SUCCESS) {
                    SafeLogs.d(TAG, "runAlbumBackupTask()", "scan error: " + scanSeafException);
                } else {
                    SafeLogs.d(TAG, "runAlbumBackupTask()", "scan success");
                }

                mediaBackupUploader = new MediaBackupUploader(getApplicationContext(), transferNotificationDispatcher);
                SeafException uploadSeafException = mediaBackupUploader.upload();
                if (uploadSeafException != SeafException.SUCCESS) {
                    SafeLogs.d(TAG, "runAlbumBackupTask()", "upload error: " + uploadSeafException);
                } else {
                    SafeLogs.d(TAG, "runAlbumBackupTask()", "upload success");
                }
            }
        }, new Runnable() {
            @Override
            public void run() {
                mediaBackupUploader = null;
                transferNotificationDispatcher.releaseAcquire(FeatureDataSource.ALBUM_BACKUP);
                getActiveTasks().remove(FeatureDataSource.ALBUM_BACKUP);
            }
        });

        getActiveTasks().put(FeatureDataSource.ALBUM_BACKUP, completableFuture);

    }

    private void runFolderBackupTask(Bundle extras) {
        boolean isForce = true;
        if (extras != null) {
            isForce = extras.getBoolean("is_force", true);
        }

        boolean finalIsForce = isForce;
        CompletableFuture<Void> completableFuture = runTask(new Runnable() {
            @Override
            public void run() {
                boolean isEnable = FolderBackupSharePreferenceHelper.readBackupSwitch();
                if (!isEnable) {
                    SafeLogs.e("folder backup is disable");
                    return;
                }

                FolderBackupScanner scanner = new FolderBackupScanner(getApplicationContext());
                SeafException scanSeafException = scanner.scan(finalIsForce);
                if (scanSeafException != SeafException.SUCCESS) {
                    SafeLogs.d(TAG, "runFolderBackupScanTask()", "scan error: " + scanSeafException);
                } else {
                    SafeLogs.d(TAG, "runFolderBackupScanTask()", "scan success");
                }

                folderBackupUploader = new FolderBackupUploader(getApplicationContext(), transferNotificationDispatcher);
                SeafException uploadSeafException = folderBackupUploader.upload();

                if (uploadSeafException != SeafException.SUCCESS) {
                    SafeLogs.d(TAG, "runFolderBackupTask()", "upload error: " + uploadSeafException);
                } else {
                    SafeLogs.d(TAG, "runFolderBackupTask()", "upload success");
                }
            }
        }, new Runnable() {
            @Override
            public void run() {
                folderBackupUploader = null;
                transferNotificationDispatcher.releaseAcquire(FeatureDataSource.FOLDER_BACKUP);
                getActiveTasks().remove(FeatureDataSource.FOLDER_BACKUP);
            }
        });

        getActiveTasks().put(FeatureDataSource.FOLDER_BACKUP, completableFuture);
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

    private void runLocalFileUpdateTask() {
        CompletableFuture<Void> completableFuture = runTask(new Runnable() {
            @Override
            public void run() {
                localFileUpdater = new LocalFileUpdater(getApplicationContext(), transferNotificationDispatcher);
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
                localFileUpdater = null;
                transferNotificationDispatcher.releaseAcquire(FeatureDataSource.AUTO_UPDATE_LOCAL_FILE);
                getActiveTasks().remove(FeatureDataSource.AUTO_UPDATE_LOCAL_FILE);
            }
        });
        getActiveTasks().put(FeatureDataSource.AUTO_UPDATE_LOCAL_FILE, completableFuture);
    }

    private void runShareToSeafileUploadTask() {
        CompletableFuture<Void> completableFuture = runTask(new Runnable() {
            @Override
            public void run() {

                shareToSeafileUploader = new ShareToSeafileUploader(getApplicationContext(), transferNotificationDispatcher);
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
                shareToSeafileUploader = null;
                transferNotificationDispatcher.releaseAcquire(FeatureDataSource.SHARE_FILE_TO_SEAFILE);
                getActiveTasks().remove(FeatureDataSource.SHARE_FILE_TO_SEAFILE);
            }
        });

        getActiveTasks().put(FeatureDataSource.SHARE_FILE_TO_SEAFILE, completableFuture);
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

        if (_executor != null) {
            _executor.shutdown();
            try {
                if (!_executor.awaitTermination(5, TimeUnit.SECONDS)) {
                    List<Runnable> droppedTasks = _executor.shutdownNow(); // 强制关闭
                    SafeLogs.d(TAG, "Cancelled " + droppedTasks.size() + " pending tasks.");
                }
            } catch (InterruptedException e) {
                _executor.shutdownNow();
                Thread.currentThread().interrupt(); // 恢复中断标志
            }
        }

        _isRunning.set(false);

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
