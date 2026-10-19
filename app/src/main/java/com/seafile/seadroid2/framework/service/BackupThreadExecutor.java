package com.seafile.seadroid2.framework.service;

import android.content.Context;

import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.enums.FeatureDataSource;
import com.seafile.seadroid2.framework.executor.TaskExecutor;
import com.seafile.seadroid2.framework.notification.NotificationKey;
import com.seafile.seadroid2.framework.notification.TransferNotifications;
import com.seafile.seadroid2.framework.service.download.FileDownloader;
import com.seafile.seadroid2.framework.service.upload.*;

import java.util.EnumMap;
import java.util.Map;
import java.util.concurrent.CompletableFuture;

/**
 * One real execution chain per business, including scan, delayed preparation and synchronous IO.
 */
public final class BackupThreadExecutor {
    private static volatile BackupThreadExecutor singleton;
    private final Map<FeatureDataSource, Run> running = new EnumMap<>(FeatureDataSource.class);
    private final Context context = SeadroidApplication.getAppContext();

    private static final class Run {
        final CompletableFuture<Void> completion = new CompletableFuture<>();
        final ExecutionCancellation cancellation = new ExecutionCancellation();
        final ParentEventTransfer transfer;
        boolean discardQueueOnCompletion;
        private boolean cancelled, repeat, fullScan, completed;
        private TransferNotifications.ResultState result = TransferNotifications.ResultState.SUCCESS;

        Run(ParentEventTransfer transfer) {
            this.transfer = transfer;
        }

        synchronized void trigger(boolean full) {
            if (cancelled || completed) return;
            repeat = true;
            fullScan |= full;
        }

        synchronized boolean takeRepeat() {
            if (cancelled || completed || !repeat) return false;
            repeat = false;
            return true;
        }

        synchronized boolean isFullScan() {
            return fullScan;
        }

        synchronized boolean isCancelled() {
            return cancelled;
        }

        synchronized void cancel() {
            if (!completed) {
                cancelled = true;
                repeat = false;
            }
        }

        synchronized void record(TransferNotifications.ResultState result) {
            if (completed) return;
            if (result == TransferNotifications.ResultState.FAILED || this.result == TransferNotifications.ResultState.SUCCESS) {
                this.result = result;
            }
        }

        synchronized TransferNotifications.ResultState result() {
            return cancelled ? TransferNotifications.ResultState.CANCELLED : result;
        }

        synchronized void complete() {
            if (completed) return;
            completed = true;
            completion.complete(null);
        }
    }

    private BackupThreadExecutor() {
    }

    public static BackupThreadExecutor getInstance() {
        if (singleton == null) synchronized (BackupThreadExecutor.class) {
            if (singleton == null) singleton = new BackupThreadExecutor();
        }
        return singleton;
    }

    public Context getApplicationContext() {
        return context;
    }

    private ParentEventTransfer create(FeatureDataSource source) {
        // A new instance per scope prevents old network callbacks from observing a later session.
        ITransferNotification silent = new ITransferNotification() {
            public void showTransferNotification(FeatureDataSource s, String text) {
            }

            public void showProgress(FeatureDataSource s, String name, int percent) {
            }

            public void clearDelay(FeatureDataSource... s) {
            }
        };
        return switch (source) {
            case MANUAL_FILE_UPLOAD -> new FileUploader(context, silent);
            case ALBUM_BACKUP -> new MediaBackupUploader(context, silent);
            case FOLDER_BACKUP -> new FolderBackupUploader(context, silent);
            case SHARE_FILE_TO_SEAFILE -> new ShareToSeafileUploader(context, silent);
            case AUTO_UPDATE_LOCAL_FILE -> new LocalFileUpdater(context, silent);
            case DOWNLOAD -> new FileDownloader(context, silent);
        };
    }

    public synchronized boolean isFolderBackupRunning() {
        return running.containsKey(FeatureDataSource.FOLDER_BACKUP);
    }

    public synchronized boolean isAlbumBackupRunning() {
        return running.containsKey(FeatureDataSource.ALBUM_BACKUP);
    }

    public synchronized boolean isDownloading() {
        return running.containsKey(FeatureDataSource.DOWNLOAD);
    }

    public synchronized boolean isUploading() {
        for (FeatureDataSource source : running.keySet()) if (source != FeatureDataSource.DOWNLOAD) return true;
        return false;
    }

    public synchronized boolean isTransferring() {
        return !running.isEmpty();
    }

    /**
     * The returned completion is not a cancellation handle. Execution ownership ends only in finish().
     */
    private synchronized CompletableFuture<Void> start(FeatureDataSource source, boolean scan, boolean full) {
        Run existing = running.get(source);
        if (existing != null) {
            // Merge triggers, including the narrow queue-drained/completion race.
            existing.trigger(full);
            return existing.completion;
        }
        ParentEventTransfer transfer = create(source);
        Run run = new Run(transfer);
        run.trigger(full);
        run.takeRepeat();
        running.put(source, run);
        submit(source, run, scan);
        return run.completion;
    }

    private void submit(FeatureDataSource source, Run run, boolean scan) {
        TaskExecutor.getInstance().getExecutor().execute(() -> {
            if (run.isCancelled()) {
                finish(source, run, TransferNotifications.ResultState.CANCELLED);
                return;
            }

            ExecutionCancellation.enter(run.cancellation);
            final NotificationKey key = NotificationKey.from(source);
            try {
                TransferNotifications.get().begin(key);
                boolean scanNext = scan;
                while (true) {
                    TransferNotifications.ResultState result = TransferNotifications.ResultState.SUCCESS;
                    try {
                        if (scanNext) {
                            TransferNotifications.get().scanning(key);
                            SeafException scanned = source == FeatureDataSource.ALBUM_BACKUP
                                    ? new MediaBackupScanner(context, null).scan(run.isFullScan())
                                    : new FolderBackupScanner(context, null).scan(run.isFullScan());
                            result = outcome(scanned);
                            TransferNotifications.get().prepared(key);
                        }
                        if (!run.isCancelled() && result == TransferNotifications.ResultState.SUCCESS) {
                            SeafException transferred = switch (source) {
                                case ALBUM_BACKUP -> ((MediaBackupUploader) run.transfer).upload();
                                case FOLDER_BACKUP -> ((FolderBackupUploader) run.transfer).upload();
                                case MANUAL_FILE_UPLOAD -> ((FileUploader) run.transfer).upload();
                                case SHARE_FILE_TO_SEAFILE -> ((ShareToSeafileUploader) run.transfer).upload();
                                case AUTO_UPDATE_LOCAL_FILE -> ((LocalFileUpdater) run.transfer).upload();
                                case DOWNLOAD -> ((FileDownloader) run.transfer).download();
                            };
                            result = outcome(transferred);
                        }
                    } catch (Throwable error) {
                        android.util.Log.w("BackupThreadExecutor", "Transfer chain failed: " + source, error);
                        result = TransferNotifications.ResultState.FAILED;
                    }
                    synchronized (BackupThreadExecutor.this) {
                        run.record(result);
                        if (!run.takeRepeat()) {
                            finish(source, run, result);
                            break;
                        }
                        scanNext = source == FeatureDataSource.ALBUM_BACKUP || source == FeatureDataSource.FOLDER_BACKUP;
                    }
                }
            } catch (Throwable error) {
                android.util.Log.w("BackupThreadExecutor", "Execution failed: " + source, error);
                run.record(TransferNotifications.ResultState.FAILED);
            } finally {
                ExecutionCancellation.leave();
                // Also release ownership if setup or notification attachment failed.
                finish(source, run, run.result());
            }
        });
    }

    private void cancelIO(Run run) {
        run.cancel();
        run.cancellation.cancel();
        run.transfer.requestNotificationCancellation();
        if (run.transfer instanceof ParentEventUploader) ((ParentEventUploader) run.transfer).stopThis();
        if (run.transfer instanceof ParentEventDownloader) ((ParentEventDownloader) run.transfer).stopThis();
    }

    private TransferNotifications.ResultState outcome(SeafException result) {
        if (result == SeafException.SUCCESS) return TransferNotifications.ResultState.SUCCESS;
        return result == SeafException.USER_CANCELLED_EXCEPTION
                ? TransferNotifications.ResultState.CANCELLED : TransferNotifications.ResultState.FAILED;
    }

    private synchronized void finish(FeatureDataSource source, Run run, TransferNotifications.ResultState result) {
        if (running.get(source) != run) return;
        run.record(result);
        if (run.discardQueueOnCompletion) clearQueue(source);
        try {
            TransferNotifications.get().finish(NotificationKey.from(source), run.result());
        } finally {
            // Notification failures must not strand ownership or block an explicit restart.
            running.remove(source);
            run.complete();
        }
    }

    private synchronized void stop(FeatureDataSource source, boolean rerun) {
        Run run = running.get(source);
        if (run == null) {
            if (rerun) start(source, true, true);
            return;
        }
        cancelIO(run);
        if (rerun) run.completion.thenRun(() -> start(source, true, true));
        // Never cancel the future: that does not terminate synchronous Retrofit/OkHttp execution.
    }

    public synchronized void stopAll() {
        // Preserve legacy stop(): clear pending models AND stop owned IO, even without an active run.
        for (FeatureDataSource source : FeatureDataSource.values()) {
            stop(source, false);
            clearQueue(source);
            Run run = running.get(source);
            if (run != null) run.discardQueueOnCompletion = true;
        }
    }

    private void clearQueue(FeatureDataSource source) {
        switch (source) {
            case ALBUM_BACKUP -> com.seafile.seadroid2.framework.worker.GlobalTransferCacheList.ALBUM_BACKUP_QUEUE.clear();
            case FOLDER_BACKUP -> com.seafile.seadroid2.framework.worker.GlobalTransferCacheList.FOLDER_BACKUP_QUEUE.clear();
            case MANUAL_FILE_UPLOAD -> com.seafile.seadroid2.framework.worker.GlobalTransferCacheList.FILE_UPLOAD_QUEUE.clear();
            case SHARE_FILE_TO_SEAFILE -> com.seafile.seadroid2.framework.worker.GlobalTransferCacheList.SHARE_FILE_TO_SEAFILE_QUEUE.clear();
            case AUTO_UPDATE_LOCAL_FILE -> com.seafile.seadroid2.framework.worker.GlobalTransferCacheList.LOCAL_FILE_MONITOR_QUEUE.clear();
            case DOWNLOAD -> com.seafile.seadroid2.framework.worker.GlobalTransferCacheList.DOWNLOAD_QUEUE.clear();
        }
    }

    public synchronized void stopSpecialTransmitter(String modelId, FeatureDataSource source) {
        Run run = running.get(source);
        if (run == null) return;
        switch (source) {
            case ALBUM_BACKUP -> ((MediaBackupUploader) run.transfer).stopById(modelId);
            case FOLDER_BACKUP -> ((FolderBackupUploader) run.transfer).stopById(modelId);
            case MANUAL_FILE_UPLOAD -> ((FileUploader) run.transfer).stopById(modelId);
            case SHARE_FILE_TO_SEAFILE -> ((ShareToSeafileUploader) run.transfer).stopById(modelId);
            case DOWNLOAD -> ((FileDownloader) run.transfer).stopById(modelId);
            case AUTO_UPDATE_LOCAL_FILE -> ((LocalFileUpdater) run.transfer).stopThis();
        }
    }
    // Only this direct thread-pool path owns these scopes; legacy Workers are independent.

    public void runDownloadTask() {
        start(FeatureDataSource.DOWNLOAD, false, false);
    }

    public void runManualFileUploadTask() {
        start(FeatureDataSource.MANUAL_FILE_UPLOAD, false, false);
    }

    public void runLocalFileUpdateTask() {
        start(FeatureDataSource.AUTO_UPDATE_LOCAL_FILE, false, false);
    }

    public void runShareToSeafileUploadTask() {
        start(FeatureDataSource.SHARE_FILE_TO_SEAFILE, false, false);
    }

    public void runAlbumBackupTask(boolean full) {
        start(FeatureDataSource.ALBUM_BACKUP, true, full);
    }

    public void runFolderBackupFuture(boolean full) {
        start(FeatureDataSource.FOLDER_BACKUP, true, full);
    }

    public void stopDownload() {
        stop(FeatureDataSource.DOWNLOAD, false);
    }

    public void stopManualFileUpload() {
        stop(FeatureDataSource.MANUAL_FILE_UPLOAD, false);
    }

    public void stopLocalFileUpdate() {
        stop(FeatureDataSource.AUTO_UPDATE_LOCAL_FILE, false);
    }

    public void stopShareToSeafileUpload() {
        stop(FeatureDataSource.SHARE_FILE_TO_SEAFILE, false);
    }

    public void stopAlbumBackup() {
        stopAlbumBackup(false);
    }

    public void stopAlbumBackup(boolean rerun) {
        stop(FeatureDataSource.ALBUM_BACKUP, rerun);
    }

    public void stopFolderBackup() {
        stopFolderBackup(false);
    }

    public void stopFolderBackup(boolean rerun) {
        stop(FeatureDataSource.FOLDER_BACKUP, rerun);
    }
}
