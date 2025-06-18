package com.seafile.seadroid2.framework.worker.reupoad;

import android.app.ForegroundServiceStartNotAllowedException;
import android.content.Context;
import android.os.Build;
import android.os.Bundle;
import android.text.TextUtils;

import androidx.annotation.NonNull;
import androidx.work.ForegroundInfo;
import androidx.work.WorkInfo;
import androidx.work.WorkerParameters;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.FileUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.enums.FeatureDataSource;
import com.seafile.seadroid2.enums.SaveTo;
import com.seafile.seadroid2.enums.TransferDataSource;
import com.seafile.seadroid2.enums.TransferStatus;
import com.seafile.seadroid2.framework.db.AppDatabase;
import com.seafile.seadroid2.framework.db.entities.FileCacheStatusEntity;
import com.seafile.seadroid2.framework.http.HttpIO;
import com.seafile.seadroid2.framework.model.dirents.DirentFileModel;
import com.seafile.seadroid2.framework.notification.FileUploadNotificationHelper;
import com.seafile.seadroid2.framework.notification.FolderBackupNotificationHelper;
import com.seafile.seadroid2.framework.util.ExceptionUtils;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.worker.ExistingFileStrategy;
import com.seafile.seadroid2.framework.worker.GlobalTransferCacheList;
import com.seafile.seadroid2.framework.worker.TransferEvent;
import com.seafile.seadroid2.framework.worker.TransferWorker;
import com.seafile.seadroid2.framework.worker.queue.TransferModel;
import com.seafile.seadroid2.framework.worker.upload.BaseUploadWorker;
import com.seafile.seadroid2.ui.file.FileService;

import java.io.File;
import java.io.IOException;
import java.util.List;

import retrofit2.Call;

/**
 * Check the change status of the downloaded file
 */
public class DownloadedFileMonitorWorker extends BaseUploadWorker {
    private final String TAG = "DownloadedFileMonitorWorker";
    private final FileUploadNotificationHelper notificationManager;

    public DownloadedFileMonitorWorker(@NonNull Context context, @NonNull WorkerParameters workerParams) {
        super(context, workerParams);
        notificationManager = new FileUploadNotificationHelper(context);
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

    private void showNotification() {
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

    private Result start() {
        SLogs.d(TAG, "start()", "started execution");

        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        if (account == null) {
            return Result.success();
        }

        int totalPendingCount = GlobalTransferCacheList.LOCAL_FILE_MONITOR_QUEUE.getPendingCount();

        SLogs.d(TAG, "start()", "pending count: " + totalPendingCount);
        if (totalPendingCount <= 0) {
            return Result.success();
        }

        SLogs.d(TAG, "start()", "start transfer");
        showNotification();

        String interruptibleExceptionMsg = null;
        boolean wasThereSuccessfulUploaded = false;
        while (true) {
            TransferModel missFieldDataTransferModel = GlobalTransferCacheList.LOCAL_FILE_MONITOR_QUEUE.pick(true);
            if (missFieldDataTransferModel == null) {
                break;
            }

            SLogs.d(TAG, "start()", "downloaded file path: " + missFieldDataTransferModel.full_path);
            File file = new File(missFieldDataTransferModel.full_path);
            if (!FileUtils.isFileExists(file)) {
                continue;
            }

            List<FileCacheStatusEntity> cacheList = AppDatabase
                    .getInstance()
                    .fileCacheStatusDAO()
                    .getByTargetPathSync(account.getSignature(), file.getAbsolutePath());

            //if the file is not in the database, it means that the app has been deleted.
            //this data is useless
            if (CollectionUtils.isEmpty(cacheList)) {
                SLogs.d(TAG, "start()", "file is not in the database: " + file.getName());

                continue;
            }

            try {
                try {
                    TransferModel tm = parseFile(account, cacheList.get(0), file.getAbsolutePath());
                    if (tm == null) {
                        continue;
                    }

                    transfer(account, tm);
                    SLogs.d(TAG, "start()", "transfer complete");
                    wasThereSuccessfulUploaded = true;
                } catch (Exception e) {
                    SeafException seafException = ExceptionUtils.parseByThrowable(e);
                    //Is there an interruption in the transmission in some cases?
                    boolean isInterrupt = isInterrupt(seafException);
                    if (isInterrupt) {
                        SLogs.e("An exception occurred and the transmission has been interrupted");
                        notifyError(seafException);

                        // notice this, see BaseUploadWorker#isInterrupt()
                        throw e;
                    } else {
                        SLogs.e("An exception occurred and the next transfer will continue");
                    }
                }
            } catch (Exception e) {
                SLogs.e("upload file file failed: ", e);
                interruptibleExceptionMsg = e.getMessage();

                break;
            }
        }

        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.S) {
            if (getStopReason() >= WorkInfo.STOP_REASON_CANCELLED_BY_APP) {
                interruptibleExceptionMsg = SeafException.USER_CANCELLED_EXCEPTION.getMessage();
            }
        }

        if (wasThereSuccessfulUploaded) {
            showToast(R.string.updated);
        }

        SLogs.d(TAG, "start()", "downloaded file monitor: complete, upload successful? -> " + wasThereSuccessfulUploaded);
        //
        Bundle b = new Bundle();
        b.putString(TransferWorker.KEY_DATA_RESULT, interruptibleExceptionMsg);
        b.putInt(TransferWorker.KEY_TRANSFER_COUNT, totalPendingCount);
        sendWorkerEvent(FeatureDataSource.AUTO_UPDATE_LOCAL_FILE, TransferEvent.EVENT_TRANSFER_TASK_COMPLETE, b);

        return Result.success();
    }


    private TransferModel parseFile(Account account, FileCacheStatusEntity downloadedEntity, String localPath) throws IOException {
        File file = new File(localPath);
        if (!file.exists()) {
            SLogs.d(TAG, "parseFile()", "local file is not exists: " + localPath);

            return null;
        }

        //compare the local database data with the md5 value of the file if it is the same
        String localMd5 = FileUtils.getFileMD5ToString(localPath).toLowerCase();
        if (TextUtils.equals(downloadedEntity.file_md5, localMd5)) {
            return null;
        }

        try {
            DirentFileModel fileModel = getDirentDetail(downloadedEntity.repo_id, downloadedEntity.full_path);
            //if not exists in the remote, stop it, no need to upload again
            if (fileModel == null) {
                SLogs.d(TAG, "parseFile()", "file is not exists in remote: " + localPath);
                return null;
            }
        } catch (IOException e) {
            throw e;
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
        transferModel.data_source = FeatureDataSource.AUTO_UPDATE_LOCAL_FILE;
        transferModel.transfer_strategy = ExistingFileStrategy.REPLACE;
        transferModel.transfer_status = TransferStatus.WAITING;
        transferModel.setId(transferModel.genStableId());

        SLogs.d(TAG, transferModel.toString());
        return transferModel;
    }

    private DirentFileModel getDirentDetail(String repoId, String path) throws IOException {
        //get parent dirent list from remote
        Call<DirentFileModel> fileDetailCall = HttpIO.getCurrentInstance().execute(FileService.class).getFileDetailCall(repoId, path);
        retrofit2.Response<DirentFileModel> res = fileDetailCall.execute();
        if (!res.isSuccessful()) {
            SLogs.d(TAG, "request dirents failed");
            return null;
        }

        DirentFileModel t = res.body();
        if (t == null) {
            SLogs.d(TAG, "request dirents is null");
            return null;
        }

        return t;
    }
}
