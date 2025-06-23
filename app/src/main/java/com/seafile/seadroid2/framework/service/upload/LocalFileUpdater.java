package com.seafile.seadroid2.framework.service.upload;

import android.content.Context;
import android.text.TextUtils;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.FileUtils;
import com.blankj.utilcode.util.NetworkUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.enums.FeatureDataSource;
import com.seafile.seadroid2.enums.SaveTo;
import com.seafile.seadroid2.enums.TransferStatus;
import com.seafile.seadroid2.framework.db.AppDatabase;
import com.seafile.seadroid2.framework.db.entities.FileCacheStatusEntity;
import com.seafile.seadroid2.framework.http.HttpIO;
import com.seafile.seadroid2.framework.model.dirents.DirentFileModel;
import com.seafile.seadroid2.framework.notification.TransferNotificationDispatcher;
import com.seafile.seadroid2.framework.service.ParentEventUploader;
import com.seafile.seadroid2.framework.util.SafeLogs;
import com.seafile.seadroid2.framework.util.Toasts;
import com.seafile.seadroid2.framework.worker.ExistingFileStrategy;
import com.seafile.seadroid2.framework.worker.GlobalTransferCacheList;
import com.seafile.seadroid2.framework.worker.TransferEvent;
import com.seafile.seadroid2.framework.worker.queue.TransferModel;
import com.seafile.seadroid2.ui.file.FileService;

import java.io.File;
import java.io.IOException;
import java.util.List;

import retrofit2.Call;

public class LocalFileUpdater extends ParentEventUploader {
    private final String TAG = "LocalFileUpdater";

    public LocalFileUpdater(Context context, TransferNotificationDispatcher transferNotificationDispatcher) {
        super(context, transferNotificationDispatcher);
    }

    @Override
    public FeatureDataSource getFeatureDataSource() {
        return FeatureDataSource.AUTO_UPDATE_LOCAL_FILE;
    }

    protected SeafException returnSuccess() {
        send(FeatureDataSource.AUTO_UPDATE_LOCAL_FILE, TransferEvent.EVENT_TRANSFER_TASK_COMPLETE);
        return SeafException.SUCCESS;
    }

    public SeafException upload() {
        SafeLogs.d(TAG, "started execution");
        //send a start event
        send(FeatureDataSource.AUTO_UPDATE_LOCAL_FILE, TransferEvent.EVENT_TRANSFER_TASK_START);

        if (!NetworkUtils.isConnected()) {
            SafeLogs.d(TAG, "network is not connected");
            return returnSuccess();
        }

        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        if (account == null) {
            SafeLogs.d(TAG, "account is null");
            return returnSuccess();
        }

        int totalPendingCount = GlobalTransferCacheList.LOCAL_FILE_MONITOR_QUEUE.getPendingCount();
        SafeLogs.d(TAG, "pending count: " + totalPendingCount);
        if (totalPendingCount <= 0) {
            return returnSuccess();
        }

        SeafException resultSeafException = SeafException.SUCCESS;

        while (true) {
            TransferModel missFieldDataTransferModel = GlobalTransferCacheList.LOCAL_FILE_MONITOR_QUEUE.pick(true);
            if (missFieldDataTransferModel == null) {
                SafeLogs.d(TAG, "model is null");
                break;
            }

            SafeLogs.d(TAG, "downloaded file path: " + missFieldDataTransferModel.full_path);
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
                SafeLogs.d(TAG, "file is not in the database: " + file.getName());
                continue;
            }

            try {
                TransferModel tm = parseFile(account, cacheList.get(0), file.getAbsolutePath());
                if (tm == null) {
                    continue;
                }

                transfer(account, tm);
            } catch (SeafException seafException) {

                // In some cases, the transmission needs to be interrupted
                boolean isInterrupt = isInterrupt(seafException);
                if (isInterrupt) {
                    SafeLogs.e("An exception occurred and the transmission has been interrupted");
                    notifyError(seafException);

                    resultSeafException = seafException;
                    break;
                } else {
                    SafeLogs.e("An exception occurred and the next transfer will continue");
                }
            }
        }

        if (resultSeafException == SeafException.SUCCESS) {
            Toasts.show(R.string.updated);
        } else {
            Toasts.show(R.string.upload_failed + ": " + resultSeafException.getMessage());
        }

        SafeLogs.d(TAG, "downloaded file monitor: complete, upload successful?" + resultSeafException.getMessage());
        //
        String errorMsg = null;
        if (resultSeafException != SeafException.SUCCESS) {
            errorMsg = resultSeafException.getMessage();
        }
        sendCompleteEvent(FeatureDataSource.AUTO_UPDATE_LOCAL_FILE, errorMsg, totalPendingCount);

        return SeafException.SUCCESS;
    }


    private TransferModel parseFile(Account account, FileCacheStatusEntity downloadedEntity, String localPath) throws SeafException {
        File file = new File(localPath);
        if (!file.exists()) {
            SafeLogs.d(TAG, "parseFile()", "local file is not exists: " + localPath);

            return null;
        }

        //compare the local database data with the md5 value of the file if it is the same
        String localMd5 = FileUtils.getFileMD5ToString(localPath).toLowerCase();
        if (TextUtils.equals(downloadedEntity.file_md5, localMd5)) {
            return null;
        }

        DirentFileModel fileModel = getDirentDetail(downloadedEntity.repo_id, downloadedEntity.full_path);

        TransferModel transferModel = new TransferModel();
        //
        transferModel.save_to = SaveTo.DB;
        transferModel.data_source = FeatureDataSource.AUTO_UPDATE_LOCAL_FILE;

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
        transferModel.transfer_strategy = ExistingFileStrategy.REPLACE;
        transferModel.transfer_status = TransferStatus.WAITING;
        transferModel.setId(transferModel.genStableId());

        SafeLogs.d(TAG, transferModel.toString());
        return transferModel;
    }

    private DirentFileModel getDirentDetail(String repoId, String path) throws SeafException {
        retrofit2.Response<DirentFileModel> res;
        try {
            //get parent dirent list from remote
            Call<DirentFileModel> call = HttpIO.getCurrentInstance().execute(FileService.class).getFileDetailCall(repoId, path);
            res = call.execute();
        } catch (IOException e) {
            throw SeafException.NETWORK_EXCEPTION;
        }

        if (!res.isSuccessful()) {
            SafeLogs.d(TAG, "request dirents failed", "res code = " + res.code());
            throw SeafException.NETWORK_EXCEPTION;
        }

        DirentFileModel t = res.body();
        if (t == null) {
            SafeLogs.d(TAG, "request dirents is null");
            throw SeafException.NETWORK_EXCEPTION;
        }

        return t;
    }
}
