package com.seafile.seadroid2.framework.worker;

import android.content.Context;
import android.os.Build;

import androidx.annotation.NonNull;
import androidx.work.Data;
import androidx.work.WorkInfo;
import androidx.work.WorkerParameters;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.ToastUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.framework.data.model.enums.TransferDataSource;
import com.seafile.seadroid2.framework.data.db.AppDatabase;
import com.seafile.seadroid2.framework.data.db.entities.FileTransferEntity;
import com.seafile.seadroid2.framework.data.model.enums.TransferAction;
import com.seafile.seadroid2.framework.data.model.enums.TransferResult;
import com.seafile.seadroid2.framework.datastore.sp.FolderBackupManager;
import com.seafile.seadroid2.framework.notification.base.BaseNotification;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.notification.FolderBackupNotificationHelper;

import java.util.List;
import java.util.UUID;


/**
 * Worker Tag:
 *
 * @see BackgroundJobManagerImpl#TAG_ALL
 * @see BackgroundJobManagerImpl#TAG_TRANSFER
 * @see BackgroundJobManagerImpl#TAG_TRANSFER_UPLOAD
 * @see BackgroundJobManagerImpl#TAG_TRANSFER_UPLOAD_FOLDER_BACKUP_WORKER
 */
public class UploadFolderFileAutomaticallyWorker extends BaseUploadFileWorker {
    public static final UUID UID = UUID.nameUUIDFromBytes(UploadFolderFileAutomaticallyWorker.class.getSimpleName().getBytes());

    private final FolderBackupNotificationHelper notificationManager;

    public UploadFolderFileAutomaticallyWorker(@NonNull Context context, @NonNull WorkerParameters workerParams) {
        super(context, workerParams);

        notificationManager = new FolderBackupNotificationHelper(context);
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

    private Result start() {
        notificationManager.cancel();

        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        if (account == null) {
            return Result.success();
        }

        boolean isEnable = FolderBackupManager.readBackupSwitch();
        if (!isEnable) {
            return Result.success();
        }

        boolean isUploaded = false;

        notificationManager.showNotification();

        String outEvent = null;

        while (true) {
            if (isStopped()) {
                return Result.success();
            }

            SLogs.d("start upload file worker");

            //check
            List<FileTransferEntity> transferList = AppDatabase.getInstance().fileTransferDAO()
                    .getOnePendingTransferSync(account.getSignature(),
                            TransferAction.UPLOAD, TransferDataSource.FOLDER_BACKUP);

            if (CollectionUtils.isEmpty(transferList)) {
                break;
            }

            FileTransferEntity transfer = transferList.get(0);

            try {
                boolean isAmple = calcQuota(CollectionUtils.newArrayList(transfer));
                if (!isAmple) {
                    getGeneralNotificationHelper().showErrorNotification(R.string.above_quota, R.string.settings_folder_backup_info_title);
                    //
                    AppDatabase.getInstance().fileTransferDAO().cancel(account.getSignature(), TransferDataSource.FOLDER_BACKUP, TransferResult.OUT_OF_QUOTA);

                    outEvent = TransferEvent.EVENT_CANCEL_OUT_OF_QUOTA;

                    break;
                }
            } catch (Exception e) {
                e.printStackTrace();
                break;
            }

            isUploaded = true;

            try {

                transferFile(account, transfer);

            } catch (Exception e) {
                SLogs.e(e);
                catchExceptionAndUpdateDB(transfer, e);

                ToastUtils.showLong(R.string.upload_failed);
                isUploaded = false;
            }
        }

        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.S) {
            if (getStopReason() == WorkInfo.STOP_REASON_CANCELLED_BY_APP) {
                isUploaded = false;
            }
        }

        if (isUploaded) {
            ToastUtils.showLong(R.string.upload_finished);
            SLogs.d("UploadFolderFileAutomaticallyWorker all task run");
            if (outEvent == null) {
                outEvent = TransferEvent.EVENT_TRANSFERRED_WITH_DATA;
            }
        } else {
            SLogs.d("UploadFolderFileAutomaticallyWorker nothing to run");
            if (outEvent == null) {
                outEvent = TransferEvent.EVENT_TRANSFERRED_WITHOUT_DATA;
            }
        }

        notificationManager.cancel();

        //Send a completion event
        Data data = new Data.Builder()
                .putString(TransferWorker.KEY_DATA_EVENT, outEvent)
                .putString(TransferWorker.KEY_DATA_TYPE, String.valueOf(TransferDataSource.FOLDER_BACKUP))
                .build();
        return Result.success(data);
    }


}
