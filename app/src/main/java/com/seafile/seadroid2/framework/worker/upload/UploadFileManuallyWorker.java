package com.seafile.seadroid2.framework.worker.upload;

import android.content.Context;
import android.text.TextUtils;

import androidx.annotation.NonNull;
import androidx.work.Data;
import androidx.work.ForegroundInfo;
import androidx.work.ListenableWorker;
import androidx.work.WorkerParameters;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.FileUtils;
import com.blankj.utilcode.util.ToastUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.framework.data.db.AppDatabase;
import com.seafile.seadroid2.framework.data.db.entities.FileTransferEntity;
import com.seafile.seadroid2.enums.TransferAction;
import com.seafile.seadroid2.enums.TransferDataSource;
import com.seafile.seadroid2.enums.TransferResult;
import com.seafile.seadroid2.framework.notification.FileBackupNotificationHelper;
import com.seafile.seadroid2.framework.notification.base.BaseTransferNotificationHelper;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.worker.BackgroundJobManagerImpl;
import com.seafile.seadroid2.framework.worker.TransferEvent;
import com.seafile.seadroid2.framework.worker.TransferWorker;

import java.util.List;
import java.util.UUID;


/**
 * Worker Tag:
 *
 * @see BackgroundJobManagerImpl#TAG_ALL
 * @see BackgroundJobManagerImpl#TAG_TRANSFER
 */
public class UploadFileManuallyWorker extends BaseUploadWorker {
    public static final UUID UID = UUID.nameUUIDFromBytes(UploadFileManuallyWorker.class.getSimpleName().getBytes());

    private final FileBackupNotificationHelper notificationManager;

    public UploadFileManuallyWorker(@NonNull Context context, @NonNull WorkerParameters workerParams) {
        super(context, workerParams);

        notificationManager = new FileBackupNotificationHelper(context);
    }

    @Override
    public BaseTransferNotificationHelper getNotification() {
        return notificationManager;
    }

    @NonNull
    @Override
    public ListenableWorker.Result doWork() {
        return start();
    }

    /**
     * The task here may not be from the current account
     */
    private ListenableWorker.Result start() {

        ForegroundInfo foregroundInfo = notificationManager.getForegroundNotification();
        showForegroundAsync(foregroundInfo);

//        notificationManager.cancel();
//        notificationManager.showNotification();

        boolean isUploaded = false;
        String finishFlagEvent = null;

        while (true) {
            SLogs.d("start upload file worker");
            if (isStopped()) {
                return ListenableWorker.Result.success();
            }

            List<FileTransferEntity> transferList = AppDatabase
                    .getInstance().fileTransferDAO()
                    .getOnePendingTransferAllAccountSync(
                            TransferAction.UPLOAD,
                            TransferDataSource.FILE_BACKUP
                    );
            if (CollectionUtils.isEmpty(transferList)) {
                break;
            }

            FileTransferEntity transferEntity = transferList.get(0);

            try {
                boolean isAmple = calcQuota(CollectionUtils.newArrayList(transferEntity));
                if (!isAmple) {
                    getGeneralNotificationHelper().showErrorNotification(R.string.above_quota, R.string.settings_folder_backup_info_title);
                    AppDatabase.getInstance().fileTransferDAO().cancelWithFileBackup(transferEntity.related_account, TransferResult.OUT_OF_QUOTA);

                    finishFlagEvent = TransferEvent.EVENT_CANCEL_WITH_OUT_OF_QUOTA;
                    break;
                }
            } catch (Exception e) {
                e.printStackTrace();
                break;
            }

            isUploaded = true;

            try {

                String relatedAccount = transferEntity.related_account;
                Account account = SupportAccountManager.getInstance().getSpecialAccount(relatedAccount);
                if (account == null) {
                    SLogs.d("account is null : " + relatedAccount);
                    throw SeafException.notFoundUserException;
                } else if (TextUtils.isEmpty(account.token)) {
                    SLogs.d("account is not logged in : " + relatedAccount);
                    throw SeafException.notLoggedInException;
                }

                //update modified_at field
                transferEntity.modified_at = System.currentTimeMillis();
                AppDatabase.getInstance().fileTransferDAO().update(transferEntity);

                //transfer
                transferFile(account, transferEntity);

                sendTransferEvent(transferEntity, true);
            } catch (Exception e) {
                SLogs.e("upload file file failed: ", e);
                isUploaded = false;

                TransferResult transferResult = onException(transferEntity, e);

                notifyError(transferResult);

                sendTransferEvent(transferEntity, false);

                String finishFlag = isInterrupt(transferResult);
                if (!TextUtils.isEmpty(finishFlag)) {
                    finishFlagEvent = finishFlag;
                    break;
                }

            } finally {
                // After the user selects the file and completes the upload,
                // the APP will no longer cache the file in ".../android/Media/Seafile/..."
                FileUtils.delete(transferEntity.full_path);
            }
        }

        if (isUploaded) {
            ToastUtils.showLong(R.string.upload_finished);
        }

        SLogs.e("UploadFileManuallyWorker all task run");

        if (finishFlagEvent == null) {
            finishFlagEvent = TransferEvent.EVENT_FINISH;
        }

//        Account account = SupportAccountManager.getInstance().getCurrentAccount();
//        int pendingCount = AppDatabase
//                .getInstance()
//                .fileTransferDAO()
//                .countPendingTransferSync(account.getSignature(),
//                        TransferAction.UPLOAD,
//                        TransferDataSource.FILE_BACKUP
//                );

        Data outputData = new Data.Builder()
                .putString(TransferWorker.KEY_DATA_EVENT, finishFlagEvent)
                .putBoolean(TransferWorker.KEY_DATA_PARAM, isUploaded)
                .putString(TransferWorker.KEY_DATA_TYPE, String.valueOf(TransferDataSource.FILE_BACKUP))
                .build();
        return ListenableWorker.Result.success(outputData);
    }

}
