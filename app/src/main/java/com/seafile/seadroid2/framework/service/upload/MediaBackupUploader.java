package com.seafile.seadroid2.framework.service.upload;

import android.content.Context;
import android.os.Bundle;
import android.text.TextUtils;

import com.blankj.utilcode.util.NetworkUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.enums.FeatureDataSource;
import com.seafile.seadroid2.enums.TransferDataSource;
import com.seafile.seadroid2.framework.datastore.sp_livedata.AlbumBackupSharePreferenceHelper;
import com.seafile.seadroid2.framework.notification.TransferNotificationDispatcher;
import com.seafile.seadroid2.framework.notification.base.BaseTransferNotificationHelper;
import com.seafile.seadroid2.framework.service.ParentEventUploader;
import com.seafile.seadroid2.framework.util.SafeLogs;
import com.seafile.seadroid2.framework.util.Toasts;
import com.seafile.seadroid2.framework.worker.GlobalTransferCacheList;
import com.seafile.seadroid2.framework.worker.TransferEvent;
import com.seafile.seadroid2.framework.worker.TransferWorker;
import com.seafile.seadroid2.framework.worker.queue.TransferModel;
import com.seafile.seadroid2.ui.folder_backup.RepoConfig;

public class MediaBackupUploader extends ParentEventUploader {
    private final String TAG = "MediaBackupUploader";

    public MediaBackupUploader(Context context, TransferNotificationDispatcher transferNotificationDispatcher) {
        super(context, transferNotificationDispatcher);
    }

    @Override
    public FeatureDataSource getFeatureDataSource() {
        return FeatureDataSource.ALBUM_BACKUP;
    }

    public void stop() {
        SafeLogs.d(TAG, "stop()");

        //clear
        GlobalTransferCacheList.ALBUM_BACKUP_QUEUE.clear();

        //stop
        stopThis();

        send(FeatureDataSource.ALBUM_BACKUP, TransferEvent.EVENT_TRANSFER_TASK_CANCELLED);

    }

    public void stopById(String modelId) {
        SafeLogs.d(TAG, "stopById()", "stop download by id: " + modelId);

        TransferModel transferModel = getCurrentTransferringModel();
        if (transferModel == null) {
            return;
        }

        if (!TextUtils.equals(modelId, transferModel.getId())) {
            GlobalTransferCacheList.ALBUM_BACKUP_QUEUE.remove(modelId);
            return;
        }

        //stop
        stopThis();
    }

    public SeafException upload() {
        SafeLogs.d(TAG, "start upload");
        //send a start event
        send(FeatureDataSource.ALBUM_BACKUP, TransferEvent.EVENT_TRANSFER_TASK_START);

        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        if (account == null) {
            SafeLogs.d(TAG, "account is null");
            return returnSuccess();
        }
        boolean isTurnOn = AlbumBackupSharePreferenceHelper.readBackupSwitch();
        if (!isTurnOn) {
            SafeLogs.d(TAG, "backup switch is off");
            return returnSuccess();
        }

        RepoConfig repoConfig = AlbumBackupSharePreferenceHelper.readRepoConfig();
        if (repoConfig == null) {
            SafeLogs.d(TAG, "repo config is null");
            return returnSuccess();
        }

        if (TextUtils.isEmpty(repoConfig.getRepoId())) {
            SafeLogs.d(TAG, "repo id is empty");
            return returnSuccess();
        }

        if (!NetworkUtils.isConnected()) {
            SafeLogs.d(TAG, "network is not connected");
            return returnSuccess();
        }

        boolean isAllowDataPlan = AlbumBackupSharePreferenceHelper.readAllowDataPlanSwitch();
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
        int totalPendingCount = GlobalTransferCacheList.ALBUM_BACKUP_QUEUE.getPendingCount();
        if (totalPendingCount <= 0) {
            SafeLogs.e(TAG, "backup queue is empty");
            return returnSuccess();
        }
        SafeLogs.e(TAG, "pending count: " + totalPendingCount);

        SeafException resultSeafException = SeafException.SUCCESS;

        while (true) {
            TransferModel transferModel = GlobalTransferCacheList.ALBUM_BACKUP_QUEUE.pick();
            if (transferModel == null) {
                break;
            }

            transferModel.related_account = account.getSignature();
            transferModel.repo_id = repoConfig.getRepoId();
            transferModel.repo_name = repoConfig.getRepoName();

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

        //
        String errorMsg = null;
        if (resultSeafException != SeafException.SUCCESS) {
            errorMsg = resultSeafException.getMessage();

            SafeLogs.d(TAG, "all completed", "error msg: " + errorMsg);
            Toasts.show(R.string.backup_finished);
        } else {
            SafeLogs.d(TAG, "all completed");
            Toasts.show(R.string.backup_completed);
        }
        sendCompleteEvent(FeatureDataSource.ALBUM_BACKUP, errorMsg, totalPendingCount);
        return resultSeafException;
    }

    protected SeafException returnSuccess() {
        send(FeatureDataSource.ALBUM_BACKUP, TransferEvent.EVENT_TRANSFER_TASK_COMPLETE);
        return SeafException.SUCCESS;
    }

}
