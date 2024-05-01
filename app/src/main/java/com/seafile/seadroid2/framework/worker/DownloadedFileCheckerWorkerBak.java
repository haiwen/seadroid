package com.seafile.seadroid2.framework.worker;

import android.content.Context;

import androidx.annotation.NonNull;
import androidx.work.Data;
import androidx.work.WorkerParameters;

import com.blankj.utilcode.util.CloneUtils;
import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.FileUtils;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.framework.data.db.AppDatabase;
import com.seafile.seadroid2.framework.data.db.entities.DirentModel;
import com.seafile.seadroid2.framework.data.db.entities.FileTransferEntity;
import com.seafile.seadroid2.framework.data.model.dirents.DirentFileModel;
import com.seafile.seadroid2.framework.data.model.enums.TransferAction;
import com.seafile.seadroid2.framework.data.model.enums.TransferDataSource;
import com.seafile.seadroid2.framework.data.model.enums.TransferResult;
import com.seafile.seadroid2.framework.data.model.enums.TransferStatus;
import com.seafile.seadroid2.framework.notification.FolderBackupNotificationHelper;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.util.Utils;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.UUID;

/**
 * Check the change status of the downloaded file
 */
@Deprecated
public class DownloadedFileCheckerWorkerBak extends BaseUploadFileWorker {
    public static final UUID UID = UUID.randomUUID();

    private final FolderBackupNotificationHelper notificationManager;

    public DownloadedFileCheckerWorkerBak(@NonNull Context context, @NonNull WorkerParameters workerParams) {
        super(context, workerParams);

        notificationManager = new FolderBackupNotificationHelper(context);
    }

    @Override
    public FolderBackupNotificationHelper getNotification() {
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

        String outEvent = TransferEvent.EVENT_TRANSFERRED_WITH_DATA;

        //get last 10 pending transfer record
        List<FileTransferEntity> transferedList = AppDatabase.getInstance().fileTransferDAO().getDownloadListSync(account.getSignature());
        if (CollectionUtils.isEmpty(transferedList)) {
            return Result.success();
        }

        notificationManager.showNotification();

        //Send a completion event
        Data data = new Data.Builder()
                .putString(TransferWorker.KEY_DATA_EVENT, outEvent)
                .build();

        for (FileTransferEntity fileTransferEntity : transferedList) {
            try {
                checkFile(account, fileTransferEntity);
            } catch (IOException | SeafException e) {
                throw new RuntimeException(e);
            }
        }

        notificationManager.cancel();

        return Result.success(data);
    }

    private void checkFile(Account account, FileTransferEntity transferEntity) throws IOException, SeafException {
        if (transferEntity.transfer_status != TransferStatus.SUCCEEDED) {
            SLogs.d("transfer_status is not success: " + transferEntity.target_path);
            return;
        }

        File file = new File(transferEntity.target_path);
        if (!file.exists()) {
            SLogs.d("file is not exists: " + transferEntity.target_path);
            return;
        }

        List<DirentModel> direntList = AppDatabase.getInstance().direntDao().getByFullPathSync(transferEntity.repo_id, transferEntity.full_path);
        if (CollectionUtils.isEmpty(direntList)) {
            // db not exist
            SLogs.d("db is not exists: " + transferEntity.target_path);
            return;
        }
        DirentModel direntModel = direntList.get(0);

        //More judgment conditions may be required

        if (direntModel.last_modified_at == transferEntity.modified_at) {
            SLogs.d("modified_at is same: " + transferEntity.target_path);

            return;
        }

        if (direntModel.size == transferEntity.file_size) {
            SLogs.d("file size is same: " + transferEntity.target_path);

            return;
        }

        DirentFileModel fileModel = getRemoteFile(transferEntity.repo_id, transferEntity.full_path);
        if (fileModel == null) {
            //remote not exists, delete local
            SLogs.d("remote file is not exists: " + transferEntity.target_path);
            return;
        }


        FileTransferEntity newTransferEntity = CloneUtils.deepClone(transferEntity, FileTransferEntity.class);
        newTransferEntity.full_path = transferEntity.target_path;
        newTransferEntity.target_path = transferEntity.full_path;
        newTransferEntity.setParent_path(Utils.getParentPath(newTransferEntity.target_path));
        newTransferEntity.file_id = null;
        newTransferEntity.transfer_action = TransferAction.UPLOAD;
        newTransferEntity.transferred_size = 0;
        newTransferEntity.transfer_result = TransferResult.NO_RESULT;
        newTransferEntity.transfer_status = TransferStatus.WAITING;
        newTransferEntity.file_size = FileUtils.getFileLength(newTransferEntity.full_path);
        newTransferEntity.file_strategy = ExistingFileStrategy.REPLACE;
        newTransferEntity.is_copy_to_local = true;
        newTransferEntity.is_auto_transfer = true;
        newTransferEntity.data_source = TransferDataSource.FILE_BACKUP;
        newTransferEntity.action_end_at = 0;
        newTransferEntity.created_at = System.currentTimeMillis();
        newTransferEntity.modified_at = newTransferEntity.created_at;
        newTransferEntity.file_original_modified_at = file.lastModified();
        newTransferEntity.file_md5 = FileUtils.getFileMD5ToString(newTransferEntity.full_path).toLowerCase();

        newTransferEntity.uid = newTransferEntity.getUID();

        AppDatabase.getInstance().fileTransferDAO().insert(newTransferEntity);


        BackgroundJobManagerImpl.getInstance().startFileUploadWorker();
    }
}
