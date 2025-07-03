package com.seafile.seadroid2.framework.service.upload;

import android.content.Context;
import android.os.Bundle;
import android.text.TextUtils;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.NetworkUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.enums.FeatureDataSource;
import com.seafile.seadroid2.enums.TransferDataSource;
import com.seafile.seadroid2.framework.datastore.sp_livedata.FolderBackupSharePreferenceHelper;
import com.seafile.seadroid2.framework.helper.ITransferNotification;
import com.seafile.seadroid2.framework.notification.TransferNotificationDispatcher;
import com.seafile.seadroid2.framework.service.ParentEventUploader;
import com.seafile.seadroid2.framework.util.SafeLogs;
import com.seafile.seadroid2.framework.util.Toasts;
import com.seafile.seadroid2.framework.worker.GlobalTransferCacheList;
import com.seafile.seadroid2.framework.worker.TransferEvent;
import com.seafile.seadroid2.framework.worker.TransferWorker;
import com.seafile.seadroid2.framework.worker.queue.TransferModel;
import com.seafile.seadroid2.ui.folder_backup.RepoConfig;

import java.util.List;

public class FolderBackupUploader extends ParentEventUploader {
    private final String TAG = "FolderBackupUploader";

    public FolderBackupUploader(Context context, ITransferNotification iTransferNotificationDispatcher) {
        super(context, iTransferNotificationDispatcher);
    }

    @Override
    public FeatureDataSource getFeatureDataSource() {
        return FeatureDataSource.FOLDER_BACKUP;
    }

    public void stop() {
        SafeLogs.d(TAG, "stop()");

        //clear
        GlobalTransferCacheList.FOLDER_BACKUP_QUEUE.clear();

        //stop
        stopThis();

        send(FeatureDataSource.FOLDER_BACKUP, TransferEvent.EVENT_TRANSFER_TASK_CANCELLED);

    }

    public void stopById(String modelId) {
        SafeLogs.d(TAG, "stopById()", "stop download by id: " + modelId);

        TransferModel transferModel = getCurrentTransferringModel();
        if (transferModel == null) {
            return;
        }

        if (!TextUtils.equals(modelId, transferModel.getId())) {
            GlobalTransferCacheList.FOLDER_BACKUP_QUEUE.remove(modelId);
            return;
        }

        //stop
        stopThis();
    }

    public SeafException upload() {

        SafeLogs.d(TAG, "start upload");
        //send a start event
        send(FeatureDataSource.FOLDER_BACKUP, TransferEvent.EVENT_TRANSFER_TASK_START);

        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        if (account == null) {
            SafeLogs.d(TAG, "account is null");
            return returnSuccess();
        }

        boolean isTurnOn = FolderBackupSharePreferenceHelper.readBackupSwitch();
        if (!isTurnOn) {
            SafeLogs.d(TAG, "backup switch is off");
            return returnSuccess();
        }

        List<String> backupPaths = FolderBackupSharePreferenceHelper.readBackupPathsAsList();
        if (CollectionUtils.isEmpty(backupPaths)) {
            SafeLogs.d(TAG, "backup paths is empty");
            return returnSuccess();
        }

        RepoConfig repoConfig = FolderBackupSharePreferenceHelper.readRepoConfig();
        if (repoConfig == null) {
            SafeLogs.d(TAG, "repo config is null");
            return returnSuccess();
        }


        if (!NetworkUtils.isConnected()) {
            SafeLogs.d(TAG, "network is not connected");
            return returnSuccess();
        }

        boolean isAllowDataPlan = FolderBackupSharePreferenceHelper.readDataPlanAllowed();
        if (!isAllowDataPlan) {
            if (NetworkUtils.isMobileData()) {
                SafeLogs.e(TAG, "data plan is not allowed", "current network type", NetworkUtils.getNetworkType().name());
                return returnSuccess();
            }

            SafeLogs.e(TAG, "data plan is not allowed", "current network type", NetworkUtils.getNetworkType().name());
        } else {
            SafeLogs.e(TAG, "data plan is allowed", "current network type", NetworkUtils.getNetworkType().name());
        }

        //
        int totalPendingCount = GlobalTransferCacheList.FOLDER_BACKUP_QUEUE.getPendingCount();
        if (totalPendingCount <= 0) {
            SafeLogs.e(TAG, "backup queue is empty");
            return returnSuccess();
        }

        SafeLogs.e(TAG, "pending count: " + totalPendingCount);
        SeafException resultSeafException = SeafException.SUCCESS;

        while (true) {
            TransferModel transferModel = GlobalTransferCacheList.FOLDER_BACKUP_QUEUE.pick();
            if (transferModel == null) {
                break;
            }

            try {

                transfer(account, transferModel);

            } catch (SeafException seafException) {
                // In some cases, the transmission needs to be interrupted
                boolean isInterrupt = isInterrupt(seafException);
                if (isInterrupt) {
                    SafeLogs.e("An exception occurred and the transmission has been interrupted");
                    notifyError(seafException);

                    resultSeafException = seafException;
                    break;
                } else {
                    SafeLogs.e("An exception occurred and the next transfer will continue");
                }
            }
        }

        String errorMsg = null;
        if (resultSeafException != SeafException.SUCCESS) {
            errorMsg = resultSeafException.getMessage();
            SafeLogs.d(TAG, "all completed", "error msg: " + errorMsg);
            Toasts.show(R.string.backup_finished);
        } else {
            Toasts.show(R.string.backup_completed);
            SafeLogs.d(TAG, "all completed");
        }
        sendCompleteEvent(FeatureDataSource.FOLDER_BACKUP, errorMsg, totalPendingCount);
        return resultSeafException;
    }

    protected SeafException returnSuccess() {
        send(FeatureDataSource.FOLDER_BACKUP, TransferEvent.EVENT_TRANSFER_TASK_COMPLETE);
        return SeafException.SUCCESS;
    }
}
