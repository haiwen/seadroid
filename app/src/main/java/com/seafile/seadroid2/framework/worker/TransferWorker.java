package com.seafile.seadroid2.framework.worker;

import android.content.Context;

import androidx.annotation.NonNull;
import androidx.work.Data;
import androidx.work.WorkerParameters;

import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.framework.data.BlockInfoBean;
import com.seafile.seadroid2.framework.data.model.dirents.DirentDirModel;
import com.seafile.seadroid2.framework.data.model.dirents.DirentFileModel;
import com.seafile.seadroid2.framework.data.model.enums.TransferDataSource;
import com.seafile.seadroid2.framework.datastore.sp.FolderBackupManager;
import com.seafile.seadroid2.framework.http.IO;
import com.seafile.seadroid2.framework.notification.GeneralNotificationHelper;
import com.seafile.seadroid2.framework.util.HttpUtils;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.ui.file.FileService;

import org.apache.commons.lang3.StringUtils;
import org.json.JSONException;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;

import io.reactivex.disposables.CompositeDisposable;
import okhttp3.RequestBody;

public abstract class TransferWorker extends BaseWorker {


    public static final int SEGMENT_SIZE = 8192;
    public static final String KEY_DATA_EVENT = "data_event_key";
    public static final String KEY_DATA_TYPE = "data_type_key";

    public static final String KEY_DATA_PROGRESS = "data_progress_key";
    public static final String KEY_DATA_TRANSFERRED_SIZE = "data_transferred_size_key";
    public static final String KEY_DATA_TOTAL_SIZE = "data_total_size_key";


    public static final String DATA_DIRENT_LIST_KEY = "data_dirent_list_key";
    public static final String DATA_TRANSFER_KEY = "data_transfer_key";
    public static final String DATA_TRANSFER_NAME_KEY = "data_transfer_name_key";

    public static final String DATA_FORCE_TRANSFER_KEY = "data_transfer_force_key";
    public static final String DATA_CANCEL_IDS = "data_cancel_ids";
    public static final String DATA_RESTART_IDS = "data_restart_ids";

    private final GeneralNotificationHelper generalNotificationHelper;

    TransferWorker(@NonNull Context context, @NonNull WorkerParameters workerParams) {
        super(context, workerParams);

        generalNotificationHelper = new GeneralNotificationHelper(context);
    }

    public GeneralNotificationHelper getGeneralNotificationHelper() {
        return generalNotificationHelper;
    }

//    protected CompositeDisposable compositeDisposable = new CompositeDisposable();
//
//    public CompositeDisposable getCompositeDisposable() {
//        return compositeDisposable;
//    }


    protected void sendProgressEvent(String event) {
        Data data = new Data.Builder()
                .putString(KEY_DATA_EVENT, event)
                .putInt(KEY_DATA_PROGRESS, 100)
                .putLong(KEY_DATA_TRANSFERRED_SIZE, 0)
                .putLong(KEY_DATA_TOTAL_SIZE, 0)
                .build();

        setProgressAsync(data);
    }

    protected void sendProgress(String fileName, String uid, int percent, long size, long totalSize, TransferDataSource dataSource) {
        Data data = new Data.Builder()
                .putString(KEY_DATA_EVENT, TransferEvent.EVENT_TRANSFERRING)
                .putString(KEY_DATA_TYPE, String.valueOf(dataSource))

                .putString(DATA_TRANSFER_NAME_KEY, fileName)
                .putString(DATA_TRANSFER_KEY, uid)
                .putInt(KEY_DATA_PROGRESS, percent)
                .putLong(KEY_DATA_TRANSFERRED_SIZE, size)
                .putLong(KEY_DATA_TOTAL_SIZE, totalSize)
                .build();

        setProgressAsync(data);
    }


    protected String getFileUploadUrl(String repoId, String target_dir, boolean isUpdate) throws IOException, SeafException {
        retrofit2.Response<String> res;
        if (isUpdate) {
            res = IO.getInstanceWithLoggedIn()
                    .execute(FileService.class)
                    .getFileUpdateLink(repoId)
                    .execute();
        } else {

//            target_dir = StringUtils.removeEnd(target_dir, "/");

            res = IO.getInstanceWithLoggedIn()
                    .execute(FileService.class)
                    .getFileUploadLink(repoId, "/")
                    .execute();
        }

        if (!res.isSuccessful()) {
            throw new SeafException(res.code(),res.message());
        }

        String urlStr = res.body();
        urlStr = StringUtils.replace(urlStr, "\"", "");

        return urlStr;
    }

    //block
    protected BlockInfoBean getFileBlockUploadUrl(Account account, String repoId, LinkedList<String> blkListId) throws IOException, JSONException {
        String ids = String.join(",", blkListId);

        Map<String, String> requestDataMap = new HashMap<>();
        requestDataMap.put("blklist", ids);

        Map<String, RequestBody> requestBodyMap = HttpUtils.generateRequestBody(requestDataMap);

        retrofit2.Response<BlockInfoBean> res = IO.getInstanceWithLoggedIn()
                .execute(FileService.class)
                .getFileBlockUploadLink(repoId, requestBodyMap)
                .execute();

        if (!res.isSuccessful()) {
            return null;
        }

        return res.body();
    }

    protected boolean checkRemoteDirExists(String repoId, String dirPath) throws IOException {
        retrofit2.Response<DirentDirModel> response = IO.getInstanceWithLoggedIn()
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

        retrofit2.Response<String> response = IO.getInstanceWithLoggedIn()
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
        retrofit2.Response<DirentFileModel> fileDetailRes = IO.getInstanceWithLoggedIn()
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
}
