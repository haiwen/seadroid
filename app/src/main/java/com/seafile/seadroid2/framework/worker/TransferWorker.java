package com.seafile.seadroid2.framework.worker;

import android.content.Context;

import androidx.annotation.NonNull;
import androidx.work.Data;
import androidx.work.ForegroundInfo;
import androidx.work.WorkerParameters;

import com.google.common.util.concurrent.ListenableFuture;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.framework.data.db.AppDatabase;
import com.seafile.seadroid2.framework.data.db.entities.DirentModel;
import com.seafile.seadroid2.framework.data.db.entities.FileTransferEntity;
import com.seafile.seadroid2.framework.data.db.entities.RepoModel;
import com.seafile.seadroid2.framework.data.model.dirents.DirentDirModel;
import com.seafile.seadroid2.framework.data.model.dirents.DirentFileModel;
import com.seafile.seadroid2.framework.data.model.enums.TransferDataSource;
import com.seafile.seadroid2.framework.data.model.repo.DirentWrapperModel;
import com.seafile.seadroid2.framework.http.HttpIO;
import com.seafile.seadroid2.framework.notification.GeneralNotificationHelper;
import com.seafile.seadroid2.framework.util.HttpUtils;
import com.seafile.seadroid2.framework.util.Objs;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.ui.file.FileService;
import com.seafile.seadroid2.ui.repo.RepoService;

import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import okhttp3.RequestBody;

public abstract class TransferWorker extends BaseWorker {

    public static final int SEGMENT_SIZE = 8192;

    public static final String KEY_DATA_EVENT = "data_event_key";
    public static final String KEY_DATA_TYPE = "data_type_key";
    public static final String KEY_DATA_PARAM = "data_param_key";

    public static final String KEY_DATA_PROGRESS = "data_progress_key";
    public static final String KEY_DATA_TRANSFERRED_SIZE = "data_transferred_size_key";
    public static final String KEY_DATA_TOTAL_SIZE = "data_total_size_key";


    public static final String DATA_DIRENT_LIST_KEY = "data_dirent_list_key";
    public static final String DATA_TRANSFER_ID_KEY = "data_transfer_key";
    public static final String DATA_TRANSFER_NAME_KEY = "data_transfer_name_key";

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


    protected boolean getRemoteDirentList(Account account, RepoModel repoModel, String parentDir) throws IOException, SeafException {
        retrofit2.Response<DirentWrapperModel> wrapperRes = HttpIO.getCurrentInstance()
                .execute(RepoService.class)
                .getDirentsCall(repoModel.repo_id, parentDir)
                .execute();

        //Successful: code >= 200 && code < 300;
        if (!wrapperRes.isSuccessful()) {
            throw SeafException.networkException;
        }

        DirentWrapperModel direntWrapperModel = wrapperRes.body();
        if (direntWrapperModel == null) {
            throw SeafException.networkException;
        }

        List<DirentModel> direntModels = Objs.parseDirentsForDB(
                direntWrapperModel.dirent_list,
                direntWrapperModel.dir_id,
                account.getSignature(),
                repoModel.repo_id,
                repoModel.repo_name);

        //delete
        AppDatabase.getInstance().direntDao().deleteAllByParentPathSync(repoModel.repo_id, parentDir);


        //insert
        insertAllDataInBatches(direntModels);
        return true;
    }

    public void insertAllDataInBatches(List<DirentModel> data) {
        final int batchSize = 1000;
        int totalSize = data.size();
        for (int i = 0; i < totalSize; i += batchSize) {
            int endIndex = Math.min(totalSize, i + batchSize);
            List<DirentModel> batchList = data.subList(i, endIndex);
            AppDatabase.getInstance().direntDao().insertAllSync(batchList);
        }
    }

    public void sendEvent(String event, TransferDataSource dataSource) {
        sendFutureEvent(event, dataSource);
    }

    public ListenableFuture<Void> sendFutureEvent(String event, TransferDataSource dataSource) {
        Map<String, Object> eventMap = new HashMap<>();
        eventMap.put(KEY_DATA_EVENT, event);
        eventMap.put(TransferWorker.KEY_DATA_TYPE, String.valueOf(dataSource));
        return send(eventMap);
    }

    public void sendTransferEvent(FileTransferEntity transferEntity, boolean isSuccess) {
        Map<String, Object> eventMap = new HashMap<>();

        eventMap.put(KEY_DATA_TYPE, String.valueOf(transferEntity.data_source));

        eventMap.put(DATA_TRANSFER_NAME_KEY, transferEntity.file_name);
        eventMap.put(DATA_TRANSFER_ID_KEY, transferEntity.uid);
        eventMap.put(KEY_DATA_TOTAL_SIZE, transferEntity.file_size);

        if (isSuccess) {
            eventMap.put(KEY_DATA_EVENT, TransferEvent.EVENT_TRANSFER_SUCCESS);

            eventMap.put(KEY_DATA_PROGRESS, 100);
            eventMap.put(KEY_DATA_TRANSFERRED_SIZE, transferEntity.file_size);
        } else {
            eventMap.put(KEY_DATA_EVENT, TransferEvent.EVENT_TRANSFER_FAILED);
            eventMap.put(KEY_DATA_PROGRESS, 0);
            eventMap.put(KEY_DATA_TRANSFERRED_SIZE, 0);
        }

        send(eventMap);
    }

    public void sendProgressNotifyEvent(String fileName, String uid, int percent, long size, long totalSize, TransferDataSource dataSource) {
        Map<String, Object> eventMap = new HashMap<>();
        eventMap.put(KEY_DATA_EVENT, TransferEvent.EVENT_TRANSFERRING);
        eventMap.put(KEY_DATA_TYPE, String.valueOf(dataSource));

        eventMap.put(DATA_TRANSFER_NAME_KEY, fileName);
        eventMap.put(DATA_TRANSFER_ID_KEY, uid);
        eventMap.put(KEY_DATA_PROGRESS, percent);
        eventMap.put(KEY_DATA_TRANSFERRED_SIZE, size);
        eventMap.put(KEY_DATA_TOTAL_SIZE, totalSize);

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
