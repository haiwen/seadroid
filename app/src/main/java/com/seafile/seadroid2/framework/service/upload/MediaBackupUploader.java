package com.seafile.seadroid2.framework.service.upload;

import android.content.Context;
import android.os.Bundle;
import android.text.TextUtils;

import com.blankj.utilcode.util.NetworkUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.enums.TransferDataSource;
import com.seafile.seadroid2.framework.datastore.sp_livedata.AlbumBackupSharePreferenceHelper;
import com.seafile.seadroid2.framework.notification.AlbumBackupNotificationHelper;
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
    private AlbumBackupNotificationHelper notificationManager;

    public MediaBackupUploader(Context context, AlbumBackupNotificationHelper notificationManager) {
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
        GlobalTransferCacheList.ALBUM_BACKUP_QUEUE.clear();

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
            GlobalTransferCacheList.ALBUM_BACKUP_QUEUE.remove(modelId);
            return;
        }

        //stop
        stopThis();
    }

    public SeafException upload() {
        SafeLogs.d(TAG, "start upload");
        //send a start event
        sendWorkerEvent(TransferDataSource.ALBUM_BACKUP, TransferEvent.EVENT_TRANSFER_TASK_START);

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
                SafeLogs.d(TAG, "data plan is not allowed", "current network type: ", NetworkUtils.getNetworkType().name());
                return returnSuccess();
            }

            SafeLogs.d(TAG, "data plan is not allowed", "current network type: ", NetworkUtils.getNetworkType().name());
        } else {
            SafeLogs.d(TAG, "data plan is allowed", "current network type: ", NetworkUtils.getNetworkType().name());
        }


        //
        int totalPendingCount = GlobalTransferCacheList.ALBUM_BACKUP_QUEUE.getPendingCount();
        if (totalPendingCount <= 0) {
            SafeLogs.d(TAG, "backup queue is empty");
            return returnSuccess();
        }
        SafeLogs.d(TAG, "pending count: " + totalPendingCount);

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


        Toasts.show(R.string.upload_finished);
        SafeLogs.d(TAG, "all completed");

        //
        Bundle b = new Bundle();
        if (resultSeafException != SeafException.SUCCESS) {
            b.putString(TransferWorker.KEY_DATA_RESULT, resultSeafException.getMessage());
        }
        b.putInt(TransferWorker.KEY_TRANSFER_COUNT, totalPendingCount);
        sendWorkerEvent(TransferDataSource.ALBUM_BACKUP, TransferEvent.EVENT_TRANSFER_TASK_COMPLETE, b);
        return resultSeafException;
    }

    protected SeafException returnSuccess() {
        sendWorkerEvent(TransferDataSource.ALBUM_BACKUP, TransferEvent.EVENT_TRANSFER_TASK_COMPLETE);
        return SeafException.SUCCESS;
    }

}
