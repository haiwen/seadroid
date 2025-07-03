package com.seafile.seadroid2.framework.service.runner;

import android.content.Context;
import android.os.Bundle;

import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.enums.FeatureDataSource;
import com.seafile.seadroid2.framework.datastore.sp_livedata.FolderBackupSharePreferenceHelper;
import com.seafile.seadroid2.framework.service.upload.FolderBackupScanner;
import com.seafile.seadroid2.framework.service.upload.FolderBackupUploader;
import com.seafile.seadroid2.framework.util.SafeLogs;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.BiConsumer;

public class BackupThreadExecutor {
    private final String TAG = "BackupThreadExecutor";

    private static final BackupThreadExecutor INSTANCE = new BackupThreadExecutor();
    private final AtomicInteger runningTaskCount = new AtomicInteger(0);
    private final ThreadPoolExecutor _executor;

    private static final int CORE_POOL_SIZE = Runtime.getRuntime().availableProcessors();
    private static final int MAX_POOL_SIZE = CORE_POOL_SIZE * 2;
    private static final long KEEP_ALIVE_TIME = 30L;

    private final BackupTransferNotificationDispatcher transferNotificationDispatcher = new BackupTransferNotificationDispatcher(SeadroidApplication.getAppContext());

    private BackupThreadExecutor() {
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

    CompletableFuture<Void> folderBackupFuture;

    public void stopFolderBackup() {
        SafeLogs.d(TAG, "stopFolderBackup()");
        if (folderBackupFuture != null && !folderBackupFuture.isDone()) {
            folderBackupFuture.cancel(true);
        }
    }

    public void runFolderBackupFuture(boolean isForce) {
        folderBackupFuture = runTask(new Runnable() {
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

                FolderBackupUploader folderBackupUploader = new FolderBackupUploader(getApplicationContext(), transferNotificationDispatcher);
                SeafException uploadSeafException = folderBackupUploader.upload();
                folderBackupUploader.stop();
                if (uploadSeafException != SeafException.SUCCESS) {
                    SafeLogs.d(TAG, "runFolderBackupTask()", "upload error: " + uploadSeafException);
                } else {
                    SafeLogs.d(TAG, "runFolderBackupTask()", "upload success");
                }
            }
        }, new Runnable() {
            @Override
            public void run() {
                folderBackupFuture = null;
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
                            SafeLogs.e(TAG + " - all task complete");
                        }
                    }
                });
    }
}
