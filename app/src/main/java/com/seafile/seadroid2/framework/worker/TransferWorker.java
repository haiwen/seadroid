package com.seafile.seadroid2.framework.worker;

import android.content.Context;

import androidx.annotation.NonNull;
import androidx.work.Data;
import androidx.work.ForegroundInfo;
import androidx.work.WorkerParameters;

import com.google.common.util.concurrent.ListenableFuture;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.enums.TransferStatus;
import com.seafile.seadroid2.framework.data.db.AppDatabase;
import com.seafile.seadroid2.framework.data.db.entities.FileTransferEntity;
import com.seafile.seadroid2.framework.data.model.dirents.DirentDirModel;
import com.seafile.seadroid2.framework.data.model.dirents.DirentFileModel;
import com.seafile.seadroid2.enums.TransferDataSource;
import com.seafile.seadroid2.framework.http.HttpIO;
import com.seafile.seadroid2.framework.notification.GeneralNotificationHelper;
import com.seafile.seadroid2.framework.util.HttpUtils;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.ui.file.FileService;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import okhttp3.RequestBody;

public abstract class TransferWorker extends BaseWorker {

    public static final int SEGMENT_SIZE = 8192;

    public static final String KEY_DATA_SOURCE = "key_data_source";
    public static final String KEY_DATA_STATUS = "key_data_event";
    public static final String KEY_DATA_RESULT = "key_data_result";

    public static final String KEY_TRANSFER_ID = "key_transfer_id";
    public static final String KEY_TRANSFER_NAME = "key_transfer_name";
    public static final String KEY_TRANSFER_PROGRESS = "key_transfer_progress";
    public static final String KEY_TRANSFER_TRANSFERRED_SIZE = "key_transfer_transferred_size";
    public static final String KEY_TRANSFER_TOTAL_SIZE = "key_transfer_total_size";

    //
    public static final String KEY_TRANSFER_TOTAL_COUNT = "key_transfer_total_count";
    public static final String KEY_TRANSFER_PENDING_COUNT = "key_transfer_pending_count";


    public static final String DATA_DIRENT_LIST_KEY = "data_dirent_list_key";


    public static final String DATA_FORCE_TRANSFER_KEY = "data_transfer_force_key";
    public static final String DATA_CANCEL_IDS = "data_cancel_ids";
    public static final String DATA_RESTART_IDS = "data_restart_ids";

    private final GeneralNotificationHelper generalNotificationHelper;

    public TransferWorker(@NonNull Context context, @NonNull WorkerParameters workerParams) {
        super(context, workerParams);

        generalNotificationHelper = new GeneralNotificationHelper(context);
    }

    public GeneralNotificationHelper getGeneralNotificationHelper() {
        return generalNotificationHelper;
    }

    public void showForegroundAsync(ForegroundInfo foregroundInfo) {
        if (foregroundInfo != null) {
            setForegroundAsync(foregroundInfo);
        }
    }

    protected boolean checkRemoteDirExists(String repoId, String dirPath) throws IOException {
        retrofit2.Response<DirentDirModel> response = HttpIO.getCurrentInstance()
                .execute(FileService.class)
                .getDirDetailCall(repoId, dirPath)
                .execute();

        return response.isSuccessful();
    }

    protected void mkdirRemote(String repoId, String path) throws IOException {
        Map<String, String> requestDataMap = new HashMap<>();
        requestDataMap.put("operation", "mkdir");
        requestDataMap.put("create_parents", "true");

        Map<String, RequestBody> requestBodyMap = HttpUtils.generateRequestBody(requestDataMap);

        retrofit2.Response<String> response = HttpIO.getCurrentInstance()
                .execute(FileService.class)
                .mkDirCall(repoId, path, requestBodyMap)
                .execute();

        if (response.isSuccessful()) {
            SLogs.d("folder create success：" + path);
        } else {
            SLogs.e("folder create failed：" + response.errorBody().string());
        }
    }

    protected DirentFileModel getRemoteFile(String repoId, String remotePath) throws IOException, SeafException {
        retrofit2.Response<DirentFileModel> fileDetailRes = HttpIO.getCurrentInstance()
                .execute(FileService.class)
                .getFileDetailCall(repoId, remotePath)
                .execute();

        //Successful: code >= 200 && code < 300;
        if (!fileDetailRes.isSuccessful()) {
//            if (fileDetailRes.code() >= 300) {
//                throw SeafException.networkException;
//            }

            return null;
        }

        return fileDetailRes.body();
    }

    protected long getCurrentPendingCount(Account account, TransferDataSource dataSource) {
        return AppDatabase.getInstance().fileTransferDAO()
                .countPendingTransferSync(account.getSignature(), dataSource);
    }

    protected long getCurrentPendingCountForAllAccount(TransferDataSource dataSource) {
        return AppDatabase.getInstance().fileTransferDAO()
                .countPendingTransferSync(dataSource);
    }
    
    public void sendEvent(TransferDataSource dataSource, String event) {
        sendFutureEvent(dataSource, event);
    }

    public ListenableFuture<Void> sendFutureEvent(TransferDataSource dataSource, String event) {
        Map<String, Object> eventMap = new HashMap<>();
        eventMap.put(TransferWorker.KEY_DATA_SOURCE, dataSource.name());
        eventMap.put(TransferWorker.KEY_DATA_STATUS, event);
        return send(eventMap);
    }

    public void sendFinishEvent(Account account, FileTransferEntity currentTransferEntity, long totalCount) {
        long pendingCount = getCurrentPendingCount(account, currentTransferEntity.data_source);
        if (currentTransferEntity.transfer_status == TransferStatus.SUCCEEDED) {
            sendProgressFinishEvent(currentTransferEntity, TransferEvent.EVENT_FILE_TRANSFER_SUCCESS, totalCount, pendingCount);
        } else {
            sendProgressFinishEvent(currentTransferEntity, TransferEvent.EVENT_FILE_TRANSFER_FAILED, totalCount, pendingCount);
        }
    }

    /**
     * This is the result of the transfer of a single file
     */
    public void sendProgressFinishEvent(FileTransferEntity transferEntity, String event, long totalCount, long pendingCount) {
        Map<String, Object> eventMap = new HashMap<>();

        // who
        eventMap.put(KEY_DATA_SOURCE, transferEntity.data_source.name());
        eventMap.put(KEY_DATA_RESULT, transferEntity.result);
        eventMap.put(KEY_DATA_STATUS, event);

        // transfer
        eventMap.put(KEY_TRANSFER_TOTAL_COUNT, totalCount);
        eventMap.put(KEY_TRANSFER_PENDING_COUNT, pendingCount);

        // progress
        eventMap.put(KEY_TRANSFER_NAME, transferEntity.file_name);
        eventMap.put(KEY_TRANSFER_ID, transferEntity.uid);

//        if (transferEntity.transfer_status == TransferStatus.SUCCEEDED) {
//            eventMap.put(KEY_DATA_STATUS, TransferEvent.EVENT_FILE_TRANSFER_END);
//        } else if (transferEntity.transfer_status == TransferStatus.IN_PROGRESS) {
//
//        } else { // failed, canceled, waiting
//            eventMap.put(KEY_DATA_STATUS, TransferEvent.EVENT_FILE_TRANSFER_FAILED);
//        }

        send(eventMap);
    }

    /**
     * This is a transfer progress event for a single file
     */
    public void sendProgressNotifyEvent(String fileName, String uid, int percent, long size, long totalSize, TransferDataSource dataSource) {
        Map<String, Object> eventMap = new HashMap<>();
        //
        eventMap.put(KEY_DATA_SOURCE, dataSource.name());
        eventMap.put(KEY_DATA_STATUS, TransferEvent.EVENT_FILE_IN_TRANSFER);

        //transfer
        eventMap.put(KEY_TRANSFER_NAME, fileName);
        eventMap.put(KEY_TRANSFER_ID, uid);
        eventMap.put(KEY_TRANSFER_PROGRESS, percent);
        eventMap.put(KEY_TRANSFER_TRANSFERRED_SIZE, size);
        eventMap.put(KEY_TRANSFER_TOTAL_SIZE, totalSize);

        send(eventMap);
    }

    private ListenableFuture<Void> send(Map<String, Object> eventData) {
        Data.Builder builder = new Data.Builder();
        if (eventData != null) {
            builder.putAll(eventData);
        }
        Data data = builder.build();
        return setProgressAsync(data);
    }

}
