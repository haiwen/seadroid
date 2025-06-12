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
import com.seafile.seadroid2.enums.TransferDataSource;
import com.seafile.seadroid2.framework.datastore.sp_livedata.FolderBackupSharePreferenceHelper;
import com.seafile.seadroid2.framework.notification.FolderBackupNotificationHelper;
import com.seafile.seadroid2.framework.notification.base.BaseTransferNotificationHelper;
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
    private final FolderBackupNotificationHelper notificationManager;

    public FolderBackupUploader(Context context, FolderBackupNotificationHelper notificationManager) {
        super(context);
        this.notificationManager = notificationManager;
    }

    @Override
    public BaseTransferNotificationHelper getNotificationHelper() {
        return notificationManager;
    }

    public void stop() {
        SafeLogs.d(TAG, "stop()");

        //clear
        GlobalTransferCacheList.FOLDER_BACKUP_QUEUE.clear();

        //stop
        stopThis();
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
        sendWorkerEvent(TransferDataSource.FOLDER_BACKUP, TransferEvent.EVENT_TRANSFER_TASK_START);

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
                SafeLogs.d(TAG, "data plan is not allowed", "current network type: ", NetworkUtils.getNetworkType().name());
                return returnSuccess();
            }

            SafeLogs.d(TAG, "data plan is not allowed", "current network type: ", NetworkUtils.getNetworkType().name());
        } else {
            SafeLogs.d(TAG, "data plan is allowed", "current network type: ", NetworkUtils.getNetworkType().name());
        }


        //
        int totalPendingCount = GlobalTransferCacheList.FOLDER_BACKUP_QUEUE.getPendingCount();
        if (totalPendingCount <= 0) {
            SafeLogs.d(TAG, "backup queue is empty");
            return returnSuccess();
        }

        SafeLogs.d(TAG, "pending count: " + totalPendingCount);
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

        Toasts.show(R.string.upload_finished);
        SafeLogs.d(TAG, "all completed");

        //
        Bundle b = new Bundle();
        if (resultSeafException != SeafException.SUCCESS) {
            b.putString(TransferWorker.KEY_DATA_RESULT, resultSeafException.getMessage());
        }
        b.putInt(TransferWorker.KEY_TRANSFER_COUNT, totalPendingCount);
        sendWorkerEvent(TransferDataSource.FOLDER_BACKUP, TransferEvent.EVENT_TRANSFER_TASK_COMPLETE, b);

        return resultSeafException;
    }

    protected SeafException returnSuccess() {
        sendWorkerEvent(TransferDataSource.FOLDER_BACKUP, TransferEvent.EVENT_TRANSFER_TASK_COMPLETE);
        return SeafException.SUCCESS;
    }
}
