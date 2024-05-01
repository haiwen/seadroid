package com.seafile.seadroid2.framework.worker;

import android.content.Context;

import androidx.annotation.NonNull;
import androidx.work.Data;
import androidx.work.WorkerParameters;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.ToastUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.framework.data.db.AppDatabase;
import com.seafile.seadroid2.framework.data.db.entities.FileTransferEntity;
import com.seafile.seadroid2.framework.data.model.enums.TransferAction;
import com.seafile.seadroid2.framework.data.model.enums.TransferDataSource;
import com.seafile.seadroid2.framework.data.model.enums.TransferResult;
import com.seafile.seadroid2.framework.datastore.sp.FolderBackupManager;
import com.seafile.seadroid2.framework.notification.FolderBackupNotificationHelper;
import com.seafile.seadroid2.framework.notification.UploadNotificationHelper;
import com.seafile.seadroid2.framework.util.SLogs;

import java.util.List;
import java.util.UUID;

@Deprecated
public class UploadWorker extends BaseUploadFileWorker {
    public static final UUID UID = UUID.randomUUID();

    private final UploadNotificationHelper notificationManager;

    public UploadWorker(@NonNull Context context, @NonNull WorkerParameters workerParams) {
        super(context, workerParams);

        notificationManager = new UploadNotificationHelper(context);
    }

    @Override
    public FolderBackupNotificationHelper getNotification() {
        return null;
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
            return Result.failure();
        }

        boolean isEnable = FolderBackupManager.readBackupSwitch();
        if (!isEnable) {
            return Result.success();
        }

        boolean isUploaded = false;

        notificationManager.showNotification();

        String outEvent = TransferEvent.EVENT_TRANSFERRED_WITH_DATA;

        while (true) {
            SLogs.d("start upload file worker");

            if (isStopped()) {
                return Result.success();
            }

            SLogs.e(account.getSignature());

            //get last 10 pending transfer record
            List<FileTransferEntity> transferList = AppDatabase.getInstance().fileTransferDAO()
                    .getListPendingTransferSync(account.getSignature(), TransferAction.UPLOAD, 10);

            if (CollectionUtils.isEmpty(transferList)) {
                break;
            }

            try {
                boolean isAmple = calculateQuota(transferList);
                if (!isAmple) {
                    getGeneralNotificationHelper().showErrorNotification(R.string.out_of_quota, R.string.settings_folder_backup_info_title);
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
            for (FileTransferEntity fileTransferEntity : transferList) {
                try {
                    transferFile(account, fileTransferEntity);
                } catch (Exception e) {
                    SLogs.e(e);
                    catchExceptionAndUpdateDB(fileTransferEntity, e);
                }
            }
        }

        if (isUploaded) {
            ToastUtils.showLong(R.string.upload_finished);
            SLogs.d("all task run");
        } else {
            SLogs.d("nothing to run");
        }

        notificationManager.cancel();

        //Send a completion event
        Data data = new Data.Builder()
                .putString(TransferWorker.KEY_DATA_EVENT, outEvent)
                .build();
        return Result.success(data);
    }

}
