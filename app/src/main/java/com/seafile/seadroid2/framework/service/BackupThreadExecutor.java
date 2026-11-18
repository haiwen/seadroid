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
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.worker.GlobalTransferCacheList;

import java.util.EnumMap;
import java.util.Map;
import java.util.concurrent.CompletableFuture;

/**
 * One real execution chain per business, including scan, delayed preparation and synchronous IO.
 */
public final class BackupThreadExecutor {
    private static volatile BackupThreadExecutor singleton;
    private final Map<FeatureDataSource, Run> running = new EnumMap<>(FeatureDataSource.class);
    /**
     * Each service can retain at most one request waiting to be started;
     * running tasks are restarted by finish() after completion.
     */
    private final Map<FeatureDataSource, StartRequest> pending = new EnumMap<>(FeatureDataSource.class);
    private final Context context = SeadroidApplication.getAppContext();

    private static final class Run {
        final CompletableFuture<Void> completion = new CompletableFuture<>();
        final ExecutionCancellation cancellation = new ExecutionCancellation();
        final ParentEventTransfer transfer;
        private final boolean fullScan;
        private boolean cancelled;
        private boolean completed;
        private TransferNotifications.ResultState result = TransferNotifications.ResultState.SUCCESS;

        Run(ParentEventTransfer transfer, boolean fullScan) {
            this.transfer = transfer;
            this.fullScan = fullScan;
        }

        boolean isFullScan() {
            return fullScan;
        }

        synchronized boolean isCancelled() {
            return cancelled;
        }

        synchronized void cancel() {
            if (!completed) cancelled = true;
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
        return switch (source) {
            case MANUAL_FILE_UPLOAD -> new FileUploader(context, null);
            case ALBUM_BACKUP -> new MediaBackupUploader(context, null);
            case FOLDER_BACKUP -> new FolderBackupUploader(context, null);
            case SHARE_FILE_TO_SEAFILE -> new ShareToSeafileUploader(context, null);
            case AUTO_UPDATE_LOCAL_FILE -> new LocalFileUpdater(context, null);
            case DOWNLOAD -> new FileDownloader(context, null);
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
     * For the same service, there can be at most one running Run and one request waiting to start.
     *
     * @param replaceRunning true：Cancel the current run; this request will start immediately after the old run exits.
     * @param resetModels    Should the pending models be cleared during generational switch? (This only applies to rescanable services)
     *                       <p>
     *                       The returned completion is not a cancellation handle. Execution ownership ends only in finish().
     */
    private synchronized CompletableFuture<Void> start(FeatureDataSource source, boolean scan, boolean full,
                                                       boolean replaceRunning, boolean resetModels) {
        boolean rescan = isScanSource(source);
        StartRequest request = new StartRequest(full, resetModels && rescan);

        Run active = running.get(source);
        if (active == null) {
            return begin(source, scan, request);
        }

        // When there are already waiting requests, merge them by field instead of overwriting the entire request, thus avoiding the loss of full requests or the semantics of clearing the queue.
        StartRequest waiting = pending.get(source);
        if (waiting == null) {
            pending.put(source, request);
        } else {
            waiting.merge(request);
        }

        // Continuous triggering only updates waiting requests; canceled Runs will not be canceled again.
        if (replaceRunning && rescan && !active.isCancelled()) {
            cancelIO(active);
        }
        return active.completion;
    }

    private CompletableFuture<Void> begin(FeatureDataSource source, boolean scan, StartRequest request) {
        // The old run has exited and the new scan has not yet been committed; this is the only safe place to clear the cache.
        if (request.isResetModels()) {
            clearQueue(source);
        }
        Run run = new Run(create(source), request.isFullScan());
        running.put(source, run);
        submit(source, run, scan);
        return run.completion;
    }

    private static boolean isScanSource(FeatureDataSource source) {
        return source == FeatureDataSource.ALBUM_BACKUP || source == FeatureDataSource.FOLDER_BACKUP;
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
                TransferNotifications.ResultState result = TransferNotifications.ResultState.SUCCESS;
                try {
                    if (scan) {
                        TransferNotifications.get().scanning(key);
                        SeafException scanned = source == FeatureDataSource.ALBUM_BACKUP
                                ? new MediaBackupScanner(context, null).scan(run.isFullScan())
                                : new FolderBackupScanner(context, null).scan(run.isFullScan());
                        result = outcome(scanned);
                        TransferNotifications.get().prepared(key);
                        if (result == TransferNotifications.ResultState.SUCCESS) {
                            ExecutionCancellation.await(1000);
                        }
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
                    if (!run.isCancelled()) android.util.Log.w("BackupThreadExecutor", "Transfer chain failed: " + source, error);
                    result = run.isCancelled() ? TransferNotifications.ResultState.CANCELLED
                            : TransferNotifications.ResultState.FAILED;
                }

                // The next startup is initiated as a new Run by the pending request in finish().
                run.record(result);
            } catch (Throwable error) {
                SLogs.e("BackupThreadExecutor", "Execution failed: " + source, error);
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
        StartRequest next = pending.remove(source);
        try {
            // When there are subsequent requests, keep the notifications continuous, and let the new Run's begin() take over to avoid flashing "cancelled".
            if (next == null) {
                TransferNotifications.get().finish(NotificationKey.from(source), run.result());
            }
        } finally {
            // Notification failures must not strand ownership or block an explicit restart.
            running.remove(source);
            run.complete();
            if (next != null) {
                begin(source, isScanSource(source), next);
            }
        }
    }

    private synchronized void stop(FeatureDataSource source) {
        pending.remove(source);
        Run run = running.get(source);
        if (run != null) cancelIO(run);
        // Never cancel the future: that does not terminate synchronous Retrofit/OkHttp execution.
    }

    public synchronized void stopAll() {
        // Preserve legacy stop(): clear pending models AND stop owned IO, even without an active run.
        for (FeatureDataSource source : FeatureDataSource.values()) {
            stop(source);
            clearQueue(source);
        }
    }

    private void clearQueue(FeatureDataSource source) {
        switch (source) {
            case ALBUM_BACKUP -> GlobalTransferCacheList.ALBUM_BACKUP_QUEUE.clear();
            case FOLDER_BACKUP -> GlobalTransferCacheList.FOLDER_BACKUP_QUEUE.clear();
            case MANUAL_FILE_UPLOAD -> GlobalTransferCacheList.FILE_UPLOAD_QUEUE.clear();
            case SHARE_FILE_TO_SEAFILE -> GlobalTransferCacheList.SHARE_FILE_TO_SEAFILE_QUEUE.clear();
            case AUTO_UPDATE_LOCAL_FILE -> GlobalTransferCacheList.LOCAL_FILE_MONITOR_QUEUE.clear();
            case DOWNLOAD -> GlobalTransferCacheList.DOWNLOAD_QUEUE.clear();
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
        start(FeatureDataSource.DOWNLOAD, false, false, false, false);
    }

    public void runManualFileUploadTask() {
        start(FeatureDataSource.MANUAL_FILE_UPLOAD, false, false, false, false);
    }

    public void runLocalFileUpdateTask() {
        start(FeatureDataSource.AUTO_UPDATE_LOCAL_FILE, false, false, false, false);
    }

    public void runShareToSeafileUploadTask() {
        start(FeatureDataSource.SHARE_FILE_TO_SEAFILE, false, false, false, false);
    }

    public void runAlbumBackupTask(boolean full) {
        start(FeatureDataSource.ALBUM_BACKUP, true, full, false, false);
    }

    public void runFolderBackupFuture(boolean full) {
        start(FeatureDataSource.FOLDER_BACKUP, true, full, false, false);
    }

    /**
     * Restart immediately after setting changes: Cancel the current task, clear the pending models and rescan the entire dataset after the old task exits.
     */
    public void restartAlbumBackup() {
        start(FeatureDataSource.ALBUM_BACKUP, true, true, true, true);
    }

    public void restartFolderBackup() {
        start(FeatureDataSource.FOLDER_BACKUP, true, true, true, true);
    }

    public void stopDownload() {
        stop(FeatureDataSource.DOWNLOAD);
    }

    public void stopManualFileUpload() {
        stop(FeatureDataSource.MANUAL_FILE_UPLOAD);
    }

    public void stopLocalFileUpdate() {
        stop(FeatureDataSource.AUTO_UPDATE_LOCAL_FILE);
    }

    public void stopShareToSeafileUpload() {
        stop(FeatureDataSource.SHARE_FILE_TO_SEAFILE);
    }

    public void stopAlbumBackup() {
        stop(FeatureDataSource.ALBUM_BACKUP);
    }

    public void stopFolderBackup() {
        stop(FeatureDataSource.FOLDER_BACKUP);
    }
}
