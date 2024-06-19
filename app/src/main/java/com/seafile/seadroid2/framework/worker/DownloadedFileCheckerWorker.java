package com.seafile.seadroid2.framework.worker;

import android.annotation.SuppressLint;
import android.content.Context;
import android.text.TextUtils;

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
public class DownloadedFileCheckerWorker extends BaseUploadFileWorker {
    public static final UUID UID = UUID.nameUUIDFromBytes(DownloadedFileCheckerWorker.class.getSimpleName().getBytes());


    public static final String FILE_CHANGE_KEY = "download_file_change_key";
    private final FolderBackupNotificationHelper notificationManager;

    public DownloadedFileCheckerWorker(@NonNull Context context, @NonNull WorkerParameters workerParams) {
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
        SLogs.d("DownloadCheckerWorker start");

        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        if (account == null) {
            return Result.success();
        }

        String filePath = getInputData().getString(FILE_CHANGE_KEY);
        SLogs.d("DownloadCheckerWorker filePath: " + filePath);

        if (TextUtils.isEmpty(filePath)) {
            return Result.success();
        }

        if (!FileUtils.isFileExists(filePath)) {
            return Result.success();
        }

        notificationManager.cancel();

        String outEvent = TransferEvent.EVENT_TRANSFERRED_WITH_DATA;

        //
        List<FileTransferEntity> transferEntityList = AppDatabase
                .getInstance()
                .fileTransferDAO()
                .getByTargetPathSync(account.getSignature(), TransferAction.DOWNLOAD, filePath);

        if (CollectionUtils.isEmpty(transferEntityList)) {
            return Result.success();
        }

        notificationManager.showNotification();

        try {
            checkFile(account, transferEntityList.get(0));
        } catch (IOException | SeafException e) {
            throw new RuntimeException(e);
        }

        notificationManager.cancel();

        //Send a completion event
        Data data = new Data.Builder()
                .putString(TransferWorker.KEY_DATA_EVENT, outEvent)
                .build();
        return Result.success(data);
    }

    private void checkFile(Account account, FileTransferEntity downloadTransferEntity) throws IOException, SeafException {
        if (downloadTransferEntity.transfer_status != TransferStatus.SUCCEEDED) {
            SLogs.d("transfer_status is not success: " + downloadTransferEntity.target_path);
            return;
        }

        File file = new File(downloadTransferEntity.target_path);
        if (!file.exists()) {
            SLogs.d("file is not exists: " + downloadTransferEntity.target_path);
            return;
        }

        List<DirentModel> direntList = AppDatabase.getInstance().direntDao().getListByFullPathSync(downloadTransferEntity.repo_id, downloadTransferEntity.full_path);
        if (CollectionUtils.isEmpty(direntList)) {
            // db not exist
            SLogs.d("db is not exists: " + downloadTransferEntity.target_path);
            return;
        }

        //More judgment conditions may be required

        DirentFileModel fileModel = getRemoteFile(downloadTransferEntity.repo_id, downloadTransferEntity.full_path);
        if (fileModel == null) {
            //remote not exists, delete local
            SLogs.d("remote file is not exists: " + downloadTransferEntity.target_path);
            return;
        }

        //target_path is Absolute Path
        List<FileTransferEntity> transferEntityList = AppDatabase
                .getInstance()
                .fileTransferDAO()
                .getListByFullPathSync(account.getSignature(), TransferAction.UPLOAD, downloadTransferEntity.target_path);

        FileTransferEntity transferEntity = null;
        if (CollectionUtils.isEmpty(transferEntityList)) {
            transferEntity = CloneUtils.deepClone(downloadTransferEntity, FileTransferEntity.class);
            transferEntity.full_path = downloadTransferEntity.target_path;
            transferEntity.target_path = downloadTransferEntity.full_path;
            transferEntity.setParent_path(Utils.getParentPath(transferEntity.target_path));
            transferEntity.file_id = null;
            transferEntity.transfer_action = TransferAction.UPLOAD;
            transferEntity.transferred_size = 0;
            transferEntity.transfer_result = TransferResult.NO_RESULT;
            transferEntity.transfer_status = TransferStatus.WAITING;
            transferEntity.file_size = FileUtils.getFileLength(transferEntity.full_path);
            transferEntity.file_strategy = ExistingFileStrategy.REPLACE;
            transferEntity.is_copy_to_local = true;
            transferEntity.is_auto_transfer = true;
            transferEntity.data_source = TransferDataSource.FILE_BACKUP;
            transferEntity.action_end_at = 0;
            transferEntity.created_at = System.currentTimeMillis();
            transferEntity.modified_at = transferEntity.created_at;
            transferEntity.file_original_modified_at = file.lastModified();
            transferEntity.file_md5 = FileUtils.getFileMD5ToString(transferEntity.full_path).toLowerCase();

            transferEntity.uid = transferEntity.getUID();
        } else {
            transferEntity.file_md5 = FileUtils.getFileMD5ToString(transferEntity.full_path).toLowerCase();
            transferEntity.file_size = FileUtils.getFileLength(transferEntity.full_path);
            transferEntity.file_strategy = ExistingFileStrategy.REPLACE;
            transferEntity.file_original_modified_at = file.lastModified();

            transferEntity.transferred_size = 0;
            transferEntity.transfer_result = TransferResult.NO_RESULT;
            transferEntity.transfer_status = TransferStatus.WAITING;
        }


        AppDatabase.getInstance().fileTransferDAO().insert(transferEntity);

        //start
        BackgroundJobManagerImpl.getInstance().startFileUploadWorker();
    }
}
