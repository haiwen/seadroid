package com.seafile.seadroid2.framework.worker;

import android.content.Context;
import android.text.TextUtils;

import androidx.annotation.NonNull;
import androidx.work.Data;
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
import com.seafile.seadroid2.framework.data.model.enums.TransferAction;
import com.seafile.seadroid2.framework.data.model.enums.TransferDataSource;
import com.seafile.seadroid2.framework.data.model.enums.TransferResult;
import com.seafile.seadroid2.framework.notification.FileBackupNotificationHelper;
import com.seafile.seadroid2.framework.notification.base.BaseNotification;
import com.seafile.seadroid2.framework.util.SLogs;

import java.util.List;
import java.util.UUID;


/**
 * Worker Tag:
 *
 * @see BackgroundJobManagerImpl#TAG_ALL
 * @see BackgroundJobManagerImpl#TAG_TRANSFER
 */
public class UploadFileManuallyWorker extends BaseUploadFileWorker {
    public static final UUID UID = UUID.nameUUIDFromBytes(UploadFileManuallyWorker.class.getSimpleName().getBytes());

    private final FileBackupNotificationHelper notificationManager;

    public UploadFileManuallyWorker(@NonNull Context context, @NonNull WorkerParameters workerParams) {
        super(context, workerParams);

        notificationManager = new FileBackupNotificationHelper(context);
    }

    @Override
    public BaseNotification getNotification() {
        return notificationManager;
    }

    @NonNull
    @Override
    public Result doWork() {
        return start();
    }

    /**
     * The task here may not be from the current account
     */
    private Result start() {

        notificationManager.cancel();
        notificationManager.showNotification();

        boolean isUploaded = false;
        String outEvent = null;

        while (true) {
            SLogs.d("start upload file worker");
            if (isStopped()) {
                return Result.success();
            }

            List<FileTransferEntity> transferList = AppDatabase.getInstance().fileTransferDAO()
                    .getOnePendingTransferAllAccountSync(
                            TransferAction.UPLOAD,
                            TransferDataSource.FILE_BACKUP);
            if (CollectionUtils.isEmpty(transferList)) {
                break;
            }

            FileTransferEntity transfer = transferList.get(0);

            try {
                boolean isAmple = calcQuota(CollectionUtils.newArrayList(transfer));
                if (!isAmple) {
                    getGeneralNotificationHelper().showErrorNotification(R.string.above_quota, R.string.settings_folder_backup_info_title);
                    AppDatabase.getInstance().fileTransferDAO().cancelWithFileBackup(TransferResult.OUT_OF_QUOTA);

                    outEvent = TransferEvent.EVENT_CANCEL_OUT_OF_QUOTA;
                    break;
                }
            } catch (Exception e) {
                e.printStackTrace();
                break;
            }

            isUploaded = true;
            try {

                String relatedAccount = transfer.related_account;
                Account account = SupportAccountManager.getInstance().getSpecialAccount(relatedAccount);
                if (account == null) {
                    SLogs.d("account is null : " + relatedAccount);
                    throw SeafException.notFoundUserException;
                } else if (TextUtils.isEmpty(account.token)) {
                    SLogs.d("account is not logged in : " + relatedAccount);
                    throw SeafException.notLoggedInException;
                }

                transferFile(account, transfer);

            } catch (Exception e) {
                SLogs.e(e);
                catchExceptionAndUpdateDB(transfer, e);
            } finally {
                // After the user selects the file and completes the upload,
                // the APP will no longer cache the file in ".../android/Media/Seafile/..."
                FileUtils.delete(transfer.full_path);
            }
        }


        if (isUploaded) {
            ToastUtils.showLong(R.string.upload_finished);
            SLogs.d("UploadFileManuallyWorker all task run");
            if (outEvent == null) {
                outEvent = TransferEvent.EVENT_TRANSFERRED_WITH_DATA;
            }
        } else {
            SLogs.d("UploadFileManuallyWorker nothing to run");
            if (outEvent == null) {
                outEvent = TransferEvent.EVENT_TRANSFERRED_WITHOUT_DATA;
            }
        }

        notificationManager.cancel();

        //Send a completion event
        Data data = new Data.Builder()
                .putString(TransferWorker.KEY_DATA_EVENT, outEvent)
                .putString(TransferWorker.KEY_DATA_TYPE, String.valueOf(TransferDataSource.FILE_BACKUP))
                .build();
        return Result.success(data);
    }
}
