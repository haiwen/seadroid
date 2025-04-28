package com.seafile.seadroid2.framework.worker.reupoad;

import static com.blankj.utilcode.util.ThreadUtils.runOnUiThread;

import android.app.ForegroundServiceStartNotAllowedException;
import android.content.Context;
import android.os.Build;
import android.os.Bundle;
import android.os.Handler;
import android.os.Looper;
import android.text.TextUtils;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.work.Data;
import androidx.work.ForegroundInfo;
import androidx.work.WorkInfo;
import androidx.work.WorkerParameters;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.FileUtils;
import com.blankj.utilcode.util.ToastUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.enums.SaveTo;
import com.seafile.seadroid2.enums.TransferDataSource;
import com.seafile.seadroid2.enums.TransferStatus;
import com.seafile.seadroid2.framework.db.AppDatabase;
import com.seafile.seadroid2.framework.db.entities.FileCacheStatusEntity;
import com.seafile.seadroid2.framework.model.dirents.DirentFileModel;
import com.seafile.seadroid2.framework.notification.FileBackupNotificationHelper;
import com.seafile.seadroid2.framework.util.ExceptionUtils;
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

import retrofit2.Call;

/**
 * Check the change status of the downloaded file
 */
public class DownloadedFileMonitorWorker extends BaseUploadWorker {
    private final FileBackupNotificationHelper notificationManager;

    public DownloadedFileMonitorWorker(@NonNull Context context, @NonNull WorkerParameters workerParams) {
        super(context, workerParams);
        notificationManager = new FileBackupNotificationHelper(context);
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
        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        if (account == null) {
            return Result.success();
        }

        int totalPendingCount = GlobalTransferCacheList.CHANGED_FILE_MONITOR_QUEUE.getPendingCount();
        if (totalPendingCount <= 0) {
            return Result.success();
        }

        showNotification();
        String interruptibleExceptionMsg = null;
        while (true) {
            TransferModel missFieldDataTransferModel = GlobalTransferCacheList.CHANGED_FILE_MONITOR_QUEUE.pick(true);
            if (missFieldDataTransferModel == null) {
                break;
            }

            File file = new File(missFieldDataTransferModel.full_path);
            SLogs.d("DownloadedFileMonitorWorker filePath: " + missFieldDataTransferModel.full_path);
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


            try {
                try {

                    TransferModel tm = parseFile(account, cacheList.get(0), file.getAbsolutePath());
                    transfer(account, tm);

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

        showToast(R.string.updated);
        SLogs.e("downloaded file monitor: complete");

        //
        Bundle b = new Bundle();
        b.putString(TransferWorker.KEY_DATA_RESULT, interruptibleExceptionMsg);
        b.putInt(TransferWorker.KEY_TRANSFER_COUNT, totalPendingCount);
        sendWorkerEvent(TransferDataSource.DOWNLOAD, TransferEvent.EVENT_TRANSFER_FINISH, b);

        return Result.success();
    }


    private TransferModel parseFile(Account account, FileCacheStatusEntity downloadedEntity, String localPath) throws IOException {
        File file = new File(localPath);
        if (!file.exists()) {
            SLogs.e("DownloadedFileMonitorWorker -> local file is not exists: " + localPath);
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
                SLogs.e("DownloadedFileMonitorWorker -> file is not exists in remote: " + localPath);
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
        transferModel.data_source = TransferDataSource.DOWNLOAD;
        transferModel.transfer_strategy = ExistingFileStrategy.REPLACE;
        transferModel.transfer_status = TransferStatus.WAITING;
        transferModel.setId(transferModel.genStableId());

        SLogs.d(DownloadedFileMonitorWorker.class.getSimpleName() + " -> add to FILE_UPLOAD_QUEUE : " + transferModel.toString());

        return transferModel;
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
