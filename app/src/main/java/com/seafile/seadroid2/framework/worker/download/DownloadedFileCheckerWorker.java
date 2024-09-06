package com.seafile.seadroid2.framework.worker.download;

import android.content.Context;
import android.text.TextUtils;

import androidx.annotation.NonNull;
import androidx.work.Data;
import androidx.work.ForegroundInfo;
import androidx.work.WorkerParameters;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.FileUtils;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.framework.data.db.AppDatabase;
import com.seafile.seadroid2.framework.data.db.entities.DirentModel;
import com.seafile.seadroid2.framework.data.db.entities.FileTransferEntity;
import com.seafile.seadroid2.framework.data.model.dirents.DirentFileModel;
import com.seafile.seadroid2.enums.TransferAction;
import com.seafile.seadroid2.enums.TransferDataSource;
import com.seafile.seadroid2.enums.TransferResult;
import com.seafile.seadroid2.enums.TransferStatus;
import com.seafile.seadroid2.framework.notification.FolderBackupNotificationHelper;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.framework.worker.upload.BaseUploadWorker;
import com.seafile.seadroid2.framework.worker.ExistingFileStrategy;
import com.seafile.seadroid2.framework.worker.TransferEvent;
import com.seafile.seadroid2.framework.worker.TransferWorker;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.UUID;

/**
 * Check the change status of the downloaded file
 */
public class DownloadedFileCheckerWorker extends BaseUploadWorker {
    public static final UUID UID = UUID.nameUUIDFromBytes(DownloadedFileCheckerWorker.class.getSimpleName().getBytes());


    public static final String FILE_CHANGE_KEY = "download_file_change_key";
    private final FolderBackupNotificationHelper notificationHelper;

    public DownloadedFileCheckerWorker(@NonNull Context context, @NonNull WorkerParameters workerParams) {
        super(context, workerParams);

        notificationHelper = new FolderBackupNotificationHelper(context);
    }

    @Override
    public FolderBackupNotificationHelper getNotification() {
        return notificationHelper;
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


        String outEvent = TransferEvent.EVENT_FINISH;

        //
        List<FileTransferEntity> transferEntityList = AppDatabase
                .getInstance()
                .fileTransferDAO()
                .getByTargetPathSync(account.getSignature(), TransferAction.DOWNLOAD, filePath);

        if (CollectionUtils.isEmpty(transferEntityList)) {
            return Result.success();
        }

        ForegroundInfo foregroundInfo = notificationHelper.getForegroundNotification();
        showForegroundAsync(foregroundInfo);

        try {
            checkFile(account, transferEntityList.get(0));
        } catch (IOException | SeafException e) {
            return Result.failure();
        }

        //Send a completion event
        Data data = new Data.Builder()
                .putString(TransferWorker.KEY_DATA_EVENT, outEvent)
                .build();
        return Result.success(data);
    }

    private void checkFile(Account account, FileTransferEntity downloadedTransferEntity) throws IOException, SeafException {
        if (downloadedTransferEntity.transfer_status != TransferStatus.SUCCEEDED) {
            SLogs.e("transfer_status is not success: " + downloadedTransferEntity.target_path);
            return;
        }

        File file = new File(downloadedTransferEntity.target_path);
        if (!file.exists()) {
            SLogs.e("file is not exists: " + downloadedTransferEntity.target_path);
            return;
        }

        List<DirentModel> direntList = AppDatabase.getInstance().direntDao().getListByFullPathSync(downloadedTransferEntity.repo_id, downloadedTransferEntity.full_path);
        if (CollectionUtils.isEmpty(direntList)) {
            // db not exist
            SLogs.e("db is not exists: " + downloadedTransferEntity.target_path);
            return;
        }

        //More judgment conditions may be required

        DirentFileModel fileModel = getRemoteFile(downloadedTransferEntity.repo_id, downloadedTransferEntity.full_path);
        if (fileModel == null) {
            //remote not exists, delete local
            SLogs.e("remote file is not exists: " + downloadedTransferEntity.target_path);
            return;
        }

        //insert upload entity
        FileTransferEntity transferEntity = new FileTransferEntity();

        //user attribute
        transferEntity.repo_id = downloadedTransferEntity.repo_id;
        transferEntity.repo_name = downloadedTransferEntity.repo_name;
        transferEntity.related_account = downloadedTransferEntity.related_account;

        //file
        transferEntity.file_format = downloadedTransferEntity.file_format;
        transferEntity.file_name = downloadedTransferEntity.file_name;
        transferEntity.file_md5 = FileUtils.getFileMD5ToString(downloadedTransferEntity.target_path).toLowerCase();
        transferEntity.file_id = null;
        transferEntity.file_size = FileUtils.getFileLength(downloadedTransferEntity.target_path);
        transferEntity.file_original_modified_at = file.lastModified();
        transferEntity.file_strategy = ExistingFileStrategy.REPLACE;
        transferEntity.mime_type = downloadedTransferEntity.mime_type;

        //data
        transferEntity.data_source = TransferDataSource.FILE_BACKUP;
        transferEntity.transfer_result = TransferResult.NO_RESULT;
        transferEntity.transfer_status = TransferStatus.WAITING;
        transferEntity.full_path = downloadedTransferEntity.target_path;
        transferEntity.target_path = downloadedTransferEntity.full_path;
        transferEntity.setParent_path(Utils.getParentPath(downloadedTransferEntity.full_path));

        //tranfer
        transferEntity.transfer_action = TransferAction.UPLOAD;
        transferEntity.transferred_size = 0;
        transferEntity.is_copy_to_local = true;
        transferEntity.is_auto_transfer = true;
        transferEntity.action_end_at = 0;
        transferEntity.created_at = System.currentTimeMillis();
        transferEntity.modified_at = transferEntity.created_at;

        //uid
        transferEntity.uid = transferEntity.getUID();

        AppDatabase.getInstance().fileTransferDAO().insert(transferEntity);
    }
}
