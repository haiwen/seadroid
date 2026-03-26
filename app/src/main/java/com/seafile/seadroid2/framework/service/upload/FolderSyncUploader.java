package com.seafile.seadroid2.framework.service.upload;

import android.content.Context;
import android.text.TextUtils;

import com.blankj.utilcode.util.NetworkUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.enums.FeatureDataSource;
import com.seafile.seadroid2.framework.datastore.SyncRule;
import com.seafile.seadroid2.framework.datastore.SyncRuleManager;
import com.seafile.seadroid2.framework.datastore.SyncStateManager;
import com.seafile.seadroid2.framework.service.ITransferNotification;
import com.seafile.seadroid2.framework.service.ParentEventUploader;
import com.seafile.seadroid2.framework.util.SafeLogs;
import com.seafile.seadroid2.framework.util.Toasts;
import com.seafile.seadroid2.framework.worker.GlobalTransferCacheList;
import com.seafile.seadroid2.framework.worker.TransferEvent;
import com.seafile.seadroid2.framework.worker.queue.TransferModel;

/**
 * Handles uploading of local-only files to Seafile as part of folder sync.
 * Picks transfer models from the FOLDER_SYNC_QUEUE.
 */
public class FolderSyncUploader extends ParentEventUploader {
    private final String TAG = "FolderSyncUploader";

    public FolderSyncUploader(Context context, ITransferNotification i) {
        super(context, i);
    }

    @Override
    public FeatureDataSource getFeatureDataSource() {
        return FeatureDataSource.FOLDER_SYNC;
    }

    public void stop() {
        SafeLogs.d(TAG, "stop()");
        GlobalTransferCacheList.FOLDER_SYNC_QUEUE.clear();
        stopThis();
        send(FeatureDataSource.FOLDER_SYNC, TransferEvent.EVENT_TRANSFER_TASK_CANCELLED);
    }

    public void stopById(String modelId) {
        SafeLogs.d(TAG, "stopById()", "stop upload by id: " + modelId);
        TransferModel transferModel = getCurrentTransferringModel();
        if (transferModel == null) return;
        if (!TextUtils.equals(modelId, transferModel.getId())) {
            GlobalTransferCacheList.FOLDER_SYNC_QUEUE.remove(modelId);
            return;
        }
        stopThis();
    }

    public SeafException upload() {
        SafeLogs.d(TAG, "start folder sync upload");
        send(FeatureDataSource.FOLDER_SYNC, TransferEvent.EVENT_TRANSFER_TASK_START);

        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        if (account == null) {
            SafeLogs.d(TAG, "account is null");
            return returnSuccess();
        }

        if (!NetworkUtils.isConnected()) {
            SafeLogs.d(TAG, "network is not connected");
            return returnSuccess();
        }

        int totalPendingCount = GlobalTransferCacheList.FOLDER_SYNC_QUEUE.getPendingCount();
        if (totalPendingCount <= 0) {
            SafeLogs.d(TAG, "folder sync upload queue is empty");
            return returnSuccess();
        }

        SafeLogs.d(TAG, "pending count: " + totalPendingCount);

        SeafException resultException = SeafException.SUCCESS;
        SeafException interruptException = SeafException.SUCCESS;
        SeafException lastException = SeafException.SUCCESS;

        while (true) {
            TransferModel transferModel = GlobalTransferCacheList.FOLDER_SYNC_QUEUE.pick();
            if (transferModel == null) break;

            try {
                transfer(account, transferModel);

                // Upload succeeded — record in sync state immediately
                SyncRule rule = SyncRuleManager.findMatch(
                        transferModel.repo_id,
                        transferModel.target_path);
                if (rule != null) {
                    String relPath = rule.getRelativePath(
                            transferModel.target_path);
                    SyncStateManager.addSyncedFile(rule.id, relPath);
                }
            } catch (SeafException seafException) {
                boolean isInterrupt = isInterrupt(seafException);
                if (isInterrupt) {
                    SafeLogs.e("Folder sync upload interrupted");
                    notifyError(seafException);
                    interruptException = seafException;
                    break;
                } else {
                    lastException = seafException;
                    SafeLogs.e("Folder sync upload error, continuing next");
                }
            }
        }

        getTransferNotificationDispatcher().clearDelay();

        if (interruptException != SeafException.SUCCESS) {
            resultException = interruptException;
        } else if (totalPendingCount == 1 && lastException != SeafException.SUCCESS) {
            resultException = lastException;
        }

        sendCompleteEvent(FeatureDataSource.FOLDER_SYNC, 
                resultException != SeafException.SUCCESS ? resultException.getMessage() : null,
                totalPendingCount);
        return resultException;
    }

    protected SeafException returnSuccess() {
        send(FeatureDataSource.FOLDER_SYNC, TransferEvent.EVENT_TRANSFER_TASK_COMPLETE);
        return SeafException.SUCCESS;
    }
}
