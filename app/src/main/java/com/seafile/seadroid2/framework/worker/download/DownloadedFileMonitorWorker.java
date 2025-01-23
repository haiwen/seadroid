package com.seafile.seadroid2.framework.worker.download;

import android.app.ForegroundServiceStartNotAllowedException;
import android.content.Context;
import android.os.Build;
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
import com.seafile.seadroid2.framework.data.db.entities.FileTransferEntity;
import com.seafile.seadroid2.enums.TransferAction;
import com.seafile.seadroid2.enums.TransferDataSource;
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
public class DownloadedFileMonitorWorker extends BaseUploadWorker {
    public static final UUID UID = UUID.nameUUIDFromBytes(DownloadedFileMonitorWorker.class.getSimpleName().getBytes());


    public static final String FILE_CHANGE_KEY = "download_file_change_key";
    private final FolderBackupNotificationHelper notificationManager;

    public DownloadedFileMonitorWorker(@NonNull Context context, @NonNull WorkerParameters workerParams) {
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

        showNotification();

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

        ForegroundInfo foregroundInfo = notificationManager.getForegroundNotification();
        showForegroundAsync(foregroundInfo);

        try {
            checkFile(account, transferEntityList.get(0), filePath);
        } catch (IOException | SeafException e) {
            return Result.failure();
        }

        //Send a completion event
        Data data = new Data.Builder()
                .putString(TransferWorker.KEY_DATA_STATUS, outEvent)
                .build();
        return Result.success(data);
    }

    private void showNotification(){
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.S) {
            try {
                ForegroundInfo foregroundInfo = notificationManager.getForegroundNotification();
                showForegroundAsync(foregroundInfo);
            } catch (ForegroundServiceStartNotAllowedException e) {
                SLogs.e(e.getMessage());
            }
        } else {
            ForegroundInfo foregroundInfo = notificationManager.getForegroundNotification();
            showForegroundAsync(foregroundInfo);
        }
    }

    private void checkFile(Account account, FileTransferEntity downloadedEntity, String changedFilePath) throws IOException, SeafException {
        if (downloadedEntity.transfer_status != TransferStatus.SUCCEEDED) {
            SLogs.e("transfer_status is not success: " + downloadedEntity.target_path);
            return;
        }

        File file = new File(changedFilePath);
        if (!file.exists()) {
            SLogs.e("DownloadedFileMonitorWorker -> file is not exists: " + downloadedEntity.target_path);
            return;
        }

        //compare the local database data with the md5 value of the file if it is the same
        String localMd5 = FileUtils.getFileMD5ToString(changedFilePath).toLowerCase();
        if (TextUtils.equals(downloadedEntity.file_md5, localMd5)) {
            return;
        }

        //if file is not TextFile and file size not changed, do not update
        boolean isTextFile = Utils.isTextFile(file);
        if (file.length() == downloadedEntity.file_size && !isTextFile) {
            return;
        }

//        //More judgment conditions may be required
//        DirentFileModel fileModel = getRemoteFile(downloadedEntity.repo_id, downloadedEntity.full_path);
//        if (fileModel == null) {
//            //remote not exists, delete local
//            SLogs.e("remote file is not exists: " + downloadedEntity.target_path);
//            return;
//        }

        //insert upload entity
        FileTransferEntity transferEntity = new FileTransferEntity();

        //user attribute
        transferEntity.repo_id = downloadedEntity.repo_id;
        transferEntity.repo_name = downloadedEntity.repo_name;
        transferEntity.related_account = downloadedEntity.related_account;

        //file
        transferEntity.file_format = downloadedEntity.file_format;
        transferEntity.file_name = downloadedEntity.file_name;
        transferEntity.file_md5 = localMd5;
        transferEntity.file_id = null;
        transferEntity.file_size = FileUtils.getFileLength(downloadedEntity.target_path);
        transferEntity.file_original_modified_at = file.lastModified();
        transferEntity.file_strategy = ExistingFileStrategy.REPLACE;
        transferEntity.mime_type = downloadedEntity.mime_type;

        //data
        transferEntity.data_source = TransferDataSource.FILE_BACKUP;
        transferEntity.result = null;
        transferEntity.transfer_status = TransferStatus.WAITING;
        //notice here
        transferEntity.full_path = downloadedEntity.target_path;
        transferEntity.target_path = downloadedEntity.full_path;
        transferEntity.setParent_path(Utils.getParentPath(downloadedEntity.full_path));

        //transfer: update remote file
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
