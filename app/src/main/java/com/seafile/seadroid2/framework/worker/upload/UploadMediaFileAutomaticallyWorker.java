package com.seafile.seadroid2.framework.worker.upload;

import android.content.Context;
import android.os.Build;
import android.text.TextUtils;

import androidx.annotation.NonNull;
import androidx.work.Data;
import androidx.work.ForegroundInfo;
import androidx.work.ListenableWorker;
import androidx.work.WorkInfo;
import androidx.work.WorkerParameters;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.ToastUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.framework.data.db.AppDatabase;
import com.seafile.seadroid2.framework.data.db.entities.FileTransferEntity;
import com.seafile.seadroid2.enums.TransferAction;
import com.seafile.seadroid2.enums.TransferDataSource;
import com.seafile.seadroid2.enums.TransferResult;
import com.seafile.seadroid2.framework.datastore.sp_livedata.AlbumBackupSharePreferenceHelper;
import com.seafile.seadroid2.framework.datastore.sp_livedata.FolderBackupSharePreferenceHelper;
import com.seafile.seadroid2.framework.notification.AlbumBackupNotificationHelper;
import com.seafile.seadroid2.framework.notification.base.BaseTransferNotificationHelper;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.worker.BackgroundJobManagerImpl;
import com.seafile.seadroid2.framework.worker.TransferEvent;
import com.seafile.seadroid2.framework.worker.TransferWorker;
import com.seafile.seadroid2.ui.folder_backup.RepoConfig;

import java.util.List;
import java.util.UUID;

/**
 * Worker Tag:
 *
 * @see BackgroundJobManagerImpl#TAG_ALL
 * @see BackgroundJobManagerImpl#TAG_TRANSFER
 */
public class UploadMediaFileAutomaticallyWorker extends BaseUploadWorker {
    public static final UUID UID = UUID.nameUUIDFromBytes(UploadMediaFileAutomaticallyWorker.class.getSimpleName().getBytes());

    private final AlbumBackupNotificationHelper albumNotificationHelper;

    public UploadMediaFileAutomaticallyWorker(@NonNull Context context, @NonNull WorkerParameters workerParams) {
        super(context, workerParams);

        albumNotificationHelper = new AlbumBackupNotificationHelper(context);
    }

    @Override
    public BaseTransferNotificationHelper getNotification() {
        return albumNotificationHelper;
    }

    boolean isFirstShow = true;
    private void startShowNotification(){
        if (!isFirstShow) {
            return;
        }

        //send start transfer event
        sendEvent(TransferEvent.EVENT_TRANSFERRING, TransferDataSource.ALBUM_BACKUP);

        // show foreground notification
        ForegroundInfo foregroundInfo = albumNotificationHelper.getForegroundNotification();
        showForegroundAsync(foregroundInfo);
    }

    @NonNull
    @Override
    public ListenableWorker.Result doWork() {
        SLogs.d("start upload media worker");

        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        if (account == null) {

            notifyError(TransferResult.ACCOUNT_NOT_FOUND);

            return ListenableWorker.Result.success();
        }

        boolean canExec = can();
        if (!canExec) {
            return Result.success();
        }

        if (repoConfig == null) {
            return Result.success();
        }

        //
        String finishFlagEvent = null;
        boolean isUploaded = false;
        while (true) {
            if (isStopped()) {
                break;
            }

            List<FileTransferEntity> transferList = AppDatabase
                    .getInstance()
                    .fileTransferDAO()
                    .getOnePendingTransferSync(account.getSignature(),
                            TransferAction.UPLOAD,
                            TransferDataSource.ALBUM_BACKUP
                    );
            if (CollectionUtils.isEmpty(transferList)) {
                break;
            }

            startShowNotification();

            isUploaded = true;

            FileTransferEntity transferEntity = transferList.get(0);
            transferEntity.repo_id = repoConfig.getRepoID();
            transferEntity.repo_name = repoConfig.getRepoName();

            try {
                transferFile(account, transferEntity);

                sendTransferEvent(transferEntity, true);
            } catch (Exception e) {
                SLogs.e("upload media file failed: ", e);
                isUploaded = false;

                TransferResult transferResult = onException(transferEntity, e);

                if (!isStopped()) {
                    notifyError(transferResult);

                    sendTransferEvent(transferEntity, false);
                }

                String finishFlag = isInterrupt(transferResult);
                if (!TextUtils.isEmpty(finishFlag)) {
                    finishFlagEvent = finishFlag;
                    break;
                }
            }
        }

        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.S) {
            if (getStopReason() >= WorkInfo.STOP_REASON_CANCELLED_BY_APP) {
                isUploaded = false;
            }
        }

        //
        if (isUploaded) {
            ToastUtils.showLong(R.string.upload_finished);
        }

        SLogs.e("UploadMediaFileAutomaticallyWorker all task run");
        if (finishFlagEvent == null) {
            finishFlagEvent = TransferEvent.EVENT_FINISH;
        }

//        int pendingCount = AppDatabase
//                .getInstance()
//                .fileTransferDAO()
//                .countPendingTransferSync(account.getSignature(),
//                        TransferAction.UPLOAD,
//                        TransferDataSource.ALBUM_BACKUP
//                );

        Data outputData = new Data.Builder()
                .putString(TransferWorker.KEY_DATA_EVENT, finishFlagEvent)
                .putBoolean(TransferWorker.KEY_DATA_PARAM, isUploaded)
                .putString(TransferWorker.KEY_DATA_TYPE, String.valueOf(TransferDataSource.ALBUM_BACKUP))
                .build();
        return Result.success(outputData);
    }

    private RepoConfig repoConfig;

    private boolean can() {
        boolean isTurnOn = AlbumBackupSharePreferenceHelper.readBackupSwitch();
        if (!isTurnOn) {
            return false;
        }

        repoConfig = AlbumBackupSharePreferenceHelper.readRepoConfig();
        if (repoConfig == null) {
            return false;
        }

        return true;
    }
}
