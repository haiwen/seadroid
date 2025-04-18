package com.seafile.seadroid2.provider;

import android.content.Context;
import android.util.Log;

import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.annotation.Todo;
import com.seafile.seadroid2.enums.SaveTo;
import com.seafile.seadroid2.enums.TransferDataSource;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.framework.worker.BackgroundJobManagerImpl;
import com.seafile.seadroid2.framework.worker.ExistingFileStrategy;
import com.seafile.seadroid2.framework.worker.GlobalTransferCacheList;
import com.seafile.seadroid2.framework.worker.queue.TransferModel;

import java.io.File;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

@Todo("unstable feature, will be refactored/removed in the future")
public class OpenDocumentWriteWatcher {
    private static final long POLL_INTERVAL_MS = 500;
    private static final long MAX_WAIT_TIME_MS = 60_000;
    private static final ExecutorService executor = Executors.newCachedThreadPool();

    /**
     * Start a background thread, wait for the file to be written, and then submit the upload task to WorkManager.
     */
    public static void scheduleUploadAfterClose(Context context, Account account, String repoId, String repoName, String targetPath, File file, String documentId) {
        executor.execute(() -> {
            try {
                Log.d("UploadWatcher", "Watching file write: " + file.getName());

                boolean completed = waitForFileStable(file);
                if (!completed) {
                    SLogs.w("UploadWatcher", "File write timeout, skipping upload: " + file.getName());
                    return;
                }

                Log.d("UploadWatcher", "File ready, scheduling upload: " + file.getAbsolutePath());

                enqueueUploadWork(context, account, repoId, repoName, targetPath, file, documentId);

            } catch (Exception e) {
                Log.e("UploadWatcher", "Failed to watch file: " + file.getName(), e);
            }
        });
    }

    /**
     * Check whether the file is written: Check whether the file size is stable within a certain period of time.
     */
    private static boolean waitForFileStable(File file) throws InterruptedException {
        long start = System.currentTimeMillis();
        long lastSize = -1;

        while (System.currentTimeMillis() - start < MAX_WAIT_TIME_MS) {
            long currentSize = file.length();

            if (currentSize > 0 && currentSize == lastSize) {
                return true; // The size is stable and the write is considered complete
            }

            lastSize = currentSize;
            Thread.sleep(POLL_INTERVAL_MS);
        }

        return false;
    }

    /**
     * 提交 WorkManager 上传任务
     */
    private static void enqueueUploadWork(Context context, Account account, String repoId, String repoName, String targetPath, File file, String documentId) {

        TransferModel model = new TransferModel();
        model.created_at = System.currentTimeMillis();
        model.full_path = file.getAbsolutePath();
        model.file_name = file.getName();
        model.file_size = file.length();
        model.save_to = SaveTo.DELETE;
        model.data_source = TransferDataSource.FILE_BACKUP;
        model.related_account = account.getSignature();
        model.repo_id = repoId;
        model.repo_name = repoName;
        model.target_path = targetPath;
        model.setParentPath(Utils.getParentPath(targetPath));
        model.transfer_strategy = ExistingFileStrategy.REPLACE;
        model.setId(model.genStableId());

        GlobalTransferCacheList.FILE_UPLOAD_QUEUE.put(model);

        BackgroundJobManagerImpl.getInstance().startFileUploadWorker();
    }
}
