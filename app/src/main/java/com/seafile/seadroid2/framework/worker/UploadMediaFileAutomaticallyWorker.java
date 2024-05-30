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
import com.seafile.seadroid2.framework.data.db.AppDatabase;
import com.seafile.seadroid2.framework.data.db.entities.FileTransferEntity;
import com.seafile.seadroid2.framework.data.model.enums.TransferAction;
import com.seafile.seadroid2.framework.data.model.enums.TransferDataSource;
import com.seafile.seadroid2.framework.data.model.enums.TransferResult;
import com.seafile.seadroid2.framework.datastore.sp.AlbumBackupManager;
import com.seafile.seadroid2.framework.notification.base.BaseNotification;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.notification.AlbumBackupNotificationHelper;

import java.util.List;
import java.util.UUID;

/**
 * Worker Tag:
 *
 * @see BackgroundJobManagerImpl#TAG_ALL
 * @see BackgroundJobManagerImpl#TAG_TRANSFER
 */
public class UploadMediaFileAutomaticallyWorker extends BaseUploadFileWorker {
    public static final UUID UID = UUID.nameUUIDFromBytes(UploadMediaFileAutomaticallyWorker.class.getSimpleName().getBytes());

    private final AlbumBackupNotificationHelper albumNotificationHelper;

    public UploadMediaFileAutomaticallyWorker(@NonNull Context context, @NonNull WorkerParameters workerParams) {
        super(context, workerParams);

        albumNotificationHelper = new AlbumBackupNotificationHelper(context);
    }

    @Override
    public BaseNotification getNotification() {
        return albumNotificationHelper;
    }

    @NonNull
    @Override
    public Result doWork() {

        albumNotificationHelper.cancel();

        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        if (account == null) {
            return Result.failure();
        }

        boolean isEnable = AlbumBackupManager.readBackupSwitch();
        if (!isEnable) {
            return Result.success();
        }

        String outEvent = null;

        boolean isUploaded = false;
        while (true) {
            if (isStopped()) {
                break;
            }

            SLogs.d("start upload media worker");

            List<FileTransferEntity> transferList = AppDatabase.getInstance().fileTransferDAO()
                    .getOnePendingTransferSync(account.getSignature(),
                            TransferAction.UPLOAD,
                            TransferDataSource.ALBUM_BACKUP);
            if (CollectionUtils.isEmpty(transferList)) {
                break;
            }
            FileTransferEntity transfer = transferList.get(0);

            try {
                boolean isAmple = calcQuota(CollectionUtils.newArrayList(transfer));
                if (!isAmple) {
                    getGeneralNotificationHelper().showErrorNotification(R.string.above_quota, R.string.settings_camera_upload_info_title);
                    AppDatabase.getInstance().fileTransferDAO().cancel(account.getSignature(), TransferDataSource.ALBUM_BACKUP, TransferResult.OUT_OF_QUOTA);

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
                SLogs.e("upload media file failed: ", e);
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

        //
        if (isUploaded) {
            ToastUtils.showLong(R.string.upload_finished);
            SLogs.d("UploadMediaFileAutomaticallyWorker all task run");
            if (outEvent == null) {
                outEvent = TransferEvent.EVENT_TRANSFERRED_WITH_DATA;
            }
        } else {
            if (outEvent == null) {
                outEvent = TransferEvent.EVENT_TRANSFERRED_WITHOUT_DATA;
            }
            SLogs.d("UploadMediaFileAutomaticallyWorker nothing to run");
        }

        albumNotificationHelper.cancel();

        //Send a completion event
        Data data = new Data.Builder()
                .putString(TransferWorker.KEY_DATA_EVENT, outEvent)
                .putString(TransferWorker.KEY_DATA_TYPE, String.valueOf(TransferDataSource.ALBUM_BACKUP))
                .build();
        return Result.success(data);
    }
}
