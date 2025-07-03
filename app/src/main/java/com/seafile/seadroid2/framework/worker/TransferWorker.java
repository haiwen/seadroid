package com.seafile.seadroid2.framework.worker;

import android.content.Context;
import android.os.Bundle;
import android.os.Handler;
import android.os.Looper;
import android.text.TextUtils;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.work.ForegroundInfo;
import androidx.work.WorkerParameters;

import com.google.common.util.concurrent.ListenableFuture;
import com.seafile.seadroid2.bus.BusHelper;
import com.seafile.seadroid2.enums.FeatureDataSource;
import com.seafile.seadroid2.enums.TransferDataSource;
import com.seafile.seadroid2.enums.TransferStatus;
import com.seafile.seadroid2.framework.http.HttpIO;
import com.seafile.seadroid2.framework.notification.GeneralNotificationHelper;
import com.seafile.seadroid2.framework.util.HttpUtils;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.util.Toasts;
import com.seafile.seadroid2.framework.worker.queue.TransferModel;
import com.seafile.seadroid2.ui.file.FileService;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import okhttp3.RequestBody;
import retrofit2.Response;

public abstract class TransferWorker extends BaseWorker {
    private final String TAG = "TransferWorker";
    public static final int SEGMENT_SIZE = 8192;

    public static final String KEY_DATA_SOURCE = "key_data_source";
    public static final String KEY_DATA_STATUS = "key_data_event";
    public static final String KEY_DATA_RESULT = "key_data_result";

    public static final String KEY_TRANSFER_ID = "key_transfer_id";
    public static final String KEY_TRANSFER_TRANSFERRED_SIZE = "key_transfer_transferred_size";
    public static final String KEY_TRANSFER_TOTAL_SIZE = "key_transfer_total_size";

    public static final String KEY_TRANSFER_COUNT = "key_transfer_count";

    public static final String DATA_DIRENT_LIST_KEY = "data_dirent_list_key";
    public static final String DATA_FORCE_TRANSFER_KEY = "data_transfer_force_key";

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

    protected void mkdirRemote(String repoId, String path) throws IOException {
        Map<String, String> requestDataMap = new HashMap<>();
        requestDataMap.put("operation", "mkdir");
        requestDataMap.put("create_parents", "true");

        Map<String, RequestBody> requestBodyMap = HttpUtils.generateRequestBody(requestDataMap);

        Response<String> response = HttpIO.getCurrentInstance()
                .execute(FileService.class)
                .mkDirCall(repoId, path, requestBodyMap)
                .execute();

        if (response.isSuccessful()) {
            SLogs.d(TAG, "mkdirRemote()", "folder create success：" + path);
        } else {
            SLogs.d(TAG, "mkdirRemote()", "folder create failed：" + response.errorBody().string());
        }
    }

    protected void showToast(String title) {
        showToast(title, null);
    }

    protected void showToast(int tRes1, int tRes2) {
        String title1 = getApplicationContext().getString(tRes1);
        String title2 = getApplicationContext().getString(tRes2);
        showToast(title1, title2);
    }

    protected void showToast(int tRes1) {
        String title1 = getApplicationContext().getString(tRes1);
        showToast(title1, null);
    }

    protected void showToast(String title1, String title2) {
        String r = null;
        if (!TextUtils.isEmpty(title1)) {
            r = title1;
        }

        if (!TextUtils.isEmpty(title2)) {
            r += title2;
        }
        if (TextUtils.isEmpty(r)) {
            return;
        }

        Toasts.show(r);
    }

    protected void sendCompleteEvent(FeatureDataSource dataSource, String errMsg, int totalPendingCount) {
        Bundle b = new Bundle();
        b.putString(TransferWorker.KEY_DATA_RESULT, errMsg);
        b.putInt(TransferWorker.KEY_TRANSFER_COUNT, totalPendingCount);
        send(dataSource, TransferEvent.EVENT_TRANSFER_TASK_COMPLETE, b);
    }

    protected void sendScanCompleteEvent(FeatureDataSource dataSource, String content, int totalPendingCount) {
        Bundle b = new Bundle();
        b.putString(TransferWorker.KEY_DATA_RESULT, content);
        b.putInt(TransferWorker.KEY_TRANSFER_COUNT, totalPendingCount);
        send(dataSource, TransferEvent.EVENT_SCAN_COMPLETE, b);
    }

    public void sendProgressEvent(FeatureDataSource dataSource, TransferModel transferModel) {
        Bundle b = new Bundle();
        b.putString(KEY_TRANSFER_ID, transferModel.getId());
        send(dataSource, TransferEvent.EVENT_FILE_IN_TRANSFER, b);
    }

    public void sendProgressCompleteEvent(FeatureDataSource dataSource, TransferModel transferModel) {
        Bundle b = new Bundle();
        b.putString(KEY_TRANSFER_ID, transferModel.getId());
        if (transferModel.transfer_status == TransferStatus.SUCCEEDED) {
            send(dataSource, TransferEvent.EVENT_FILE_TRANSFER_SUCCESS, b);
        } else {
            send(dataSource, TransferEvent.EVENT_FILE_TRANSFER_FAILED, b);
        }
    }


    public void send(FeatureDataSource dataSource, String event) {
        send(dataSource, event, null);
    }

    public void send(FeatureDataSource dataSource, String event, Bundle extra) {
        Bundle b = new Bundle();
        b.putString(TransferWorker.KEY_DATA_SOURCE, dataSource.name());
        b.putString(TransferWorker.KEY_DATA_STATUS, event);
        if (extra != null) {
            b.putAll(extra);
        }
        send(b);
    }

    public void send(Bundle eventData) {
        BusHelper.getTransferProgressObserver().post(eventData);
    }

}
