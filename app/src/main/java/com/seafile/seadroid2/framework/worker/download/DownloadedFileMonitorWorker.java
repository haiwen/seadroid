package com.seafile.seadroid2.framework.worker.download;

import android.content.Context;
import android.text.TextUtils;

import androidx.annotation.NonNull;
import androidx.work.Data;
import androidx.work.WorkerParameters;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.FileUtils;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.enums.SaveTo;
import com.seafile.seadroid2.enums.TransferDataSource;
import com.seafile.seadroid2.enums.TransferStatus;
import com.seafile.seadroid2.framework.data.db.AppDatabase;
import com.seafile.seadroid2.framework.data.db.entities.FileCacheStatusEntity;
import com.seafile.seadroid2.framework.data.model.dirents.DirentFileModel;
import com.seafile.seadroid2.framework.worker.queue.TransferModel;
import com.seafile.seadroid2.framework.http.HttpIO;
import com.seafile.seadroid2.framework.notification.FolderBackupNotificationHelper;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.worker.ExistingFileStrategy;
import com.seafile.seadroid2.framework.worker.GlobalTransferCacheList;
import com.seafile.seadroid2.framework.worker.TransferEvent;
import com.seafile.seadroid2.framework.worker.TransferWorker;
import com.seafile.seadroid2.framework.worker.upload.BaseUploadWorker;
import com.seafile.seadroid2.ui.file.FileService;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.UUID;

import retrofit2.Call;

/**
 * Check the change status of the downloaded file
 */
public class DownloadedFileMonitorWorker extends BaseUploadWorker {
    public static final UUID UID = UUID.nameUUIDFromBytes(DownloadedFileMonitorWorker.class.getSimpleName().getBytes());

    public DownloadedFileMonitorWorker(@NonNull Context context, @NonNull WorkerParameters workerParams) {
        super(context, workerParams);
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
        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        if (account == null) {
            return Result.success();
        }

        boolean isEmpty = GlobalTransferCacheList.CHANGED_FILE_MONITOR_QUEUE.isQueueEmpty();
        if (isEmpty) {
            return Result.success();
        }

        while (true) {
            TransferModel transferModel = GlobalTransferCacheList.CHANGED_FILE_MONITOR_QUEUE.pick(true);
            if (transferModel == null) {
                break;
            }

            File file = new File(transferModel.full_path);
            SLogs.d("DownloadedFileMonitorWorker filePath: " + transferModel.full_path);
            if (!FileUtils.isFileExists(file)) {
                return Result.success();
            }

            List<FileCacheStatusEntity> cacheList = AppDatabase
                    .getInstance()
                    .fileCacheStatusDAO()
                    .getByTargetPathSync(account.getSignature(), file.getAbsolutePath());

            if (CollectionUtils.isEmpty(cacheList)) {
                continue;
            }

            checkFile(account, cacheList.get(0), file.getAbsolutePath());
        }

        //Send a completion event
        Data data = new Data.Builder()
                .putString(TransferWorker.KEY_DATA_STATUS, TransferEvent.EVENT_TRANSFER_FINISH)
                .build();
        return Result.success(data);
    }

    private void checkFile(Account account, FileCacheStatusEntity downloadedEntity, String localPath) {
        File file = new File(localPath);
        if (!file.exists()) {
            SLogs.e("DownloadedFileMonitorWorker -> local file is not exists: " + localPath);
            return;
        }

        //compare the local database data with the md5 value of the file if it is the same
        String localMd5 = FileUtils.getFileMD5ToString(localPath).toLowerCase();
        if (TextUtils.equals(downloadedEntity.file_md5, localMd5)) {
            return;
        }

        try {
            DirentFileModel fileModel = getDirentDetail(downloadedEntity.repo_id, downloadedEntity.full_path);
            //if not exists in the remote, stop it, no need to upload again
            if (fileModel == null) {
                SLogs.e("DownloadedFileMonitorWorker -> file is not exists in remote: " + localPath);
                return;
            }
        } catch (IOException e) {
            return;
        }


        TransferModel transferModel = new TransferModel();
        transferModel.save_to = SaveTo.DB;
        transferModel.created_at = System.nanoTime();
        transferModel.related_account = downloadedEntity.related_account;
        transferModel.repo_id = downloadedEntity.repo_id;
        transferModel.repo_name = downloadedEntity.repo_name;
        transferModel.file_name = downloadedEntity.file_name;
        //
        transferModel.target_path = downloadedEntity.full_path;
        transferModel.full_path = downloadedEntity.target_path;
        transferModel.setParentPath(downloadedEntity.getParent_path());
        transferModel.file_size = file.length();
        transferModel.data_source = TransferDataSource.DOWNLOAD;
        transferModel.transfer_strategy = ExistingFileStrategy.REPLACE;
        transferModel.transfer_status = TransferStatus.WAITING;
        transferModel.setId(transferModel.genStableId());
        GlobalTransferCacheList.FILE_UPLOAD_QUEUE.put(transferModel);
        SLogs.d(DownloadedFileMonitorWorker.class.getSimpleName() + " -> add to FILE_UPLOAD_QUEUE : " + transferModel.toString());
    }

    private DirentFileModel getDirentDetail(String repoId, String path) throws IOException {
        //get parent dirent list from remote
        Call<DirentFileModel> fileDetailCall = HttpIO.getCurrentInstance().execute(FileService.class).getFileDetailCall(repoId, path);
        retrofit2.Response<DirentFileModel> res = fileDetailCall.execute();
        if (!res.isSuccessful()) {
            SLogs.e(DownloadedFileMonitorWorker.class.getSimpleName() + " -> getDirRecursiveFileCall() -> request dirents failed");
            return null;
        }

        DirentFileModel t = res.body();
        if (t == null) {
            SLogs.e(DownloadedFileMonitorWorker.class.getSimpleName() + " -> getDirRecursiveFileCall() -> request dirents is null");
            return null;
        }

        return t;
    }
}
