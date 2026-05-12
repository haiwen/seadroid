package com.seafile.seadroid2.framework.service;

import android.content.Context;
import android.os.Bundle;
import android.text.TextUtils;
import android.util.Pair;

import androidx.annotation.Nullable;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.TimeUtils;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.bus.BusHelper;
import com.seafile.seadroid2.enums.FeatureDataSource;
import com.seafile.seadroid2.enums.TransferStatus;
import com.seafile.seadroid2.framework.crypto.SecurePasswordManager;
import com.seafile.seadroid2.framework.datastore.sp.SettingsManager;
import com.seafile.seadroid2.framework.db.AppDatabase;
import com.seafile.seadroid2.framework.db.entities.EncKeyCacheEntity;
import com.seafile.seadroid2.framework.http.HttpManager;
import com.seafile.seadroid2.framework.model.ResultModel;
import com.seafile.seadroid2.framework.util.ExceptionUtils;
import com.seafile.seadroid2.framework.util.SafeLogs;
import com.seafile.seadroid2.framework.worker.TransferEvent;
import com.seafile.seadroid2.framework.worker.TransferWorker;
import com.seafile.seadroid2.framework.worker.queue.TransferModel;
import com.seafile.seadroid2.ui.dialog_fragment.DialogService;

import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import okhttp3.ResponseBody;

public class ParentEventTransfer {
    private final String TAG = "ParentEventTransfer";

    public static final int SEGMENT_SIZE = 8192;

    public static final String KEY_DATA_RESULT = "key_data_result";

    public static final String KEY_TRANSFER_ID = "key_transfer_id";

    public static final String KEY_TRANSFER_COUNT = "key_transfer_count";

    private final Context context;
    private final ITransferNotification notificationDispatcher;

    public ParentEventTransfer(Context context, ITransferNotification n) {
        this.context = context;
        this.notificationDispatcher = n;
    }

    public Context getContext() {
        if (context == null) {
            throw new RuntimeException("Context is null");
        }
        return context;
    }

    public ITransferNotification getTransferNotificationDispatcher() {
        return notificationDispatcher;
    }

    @FunctionalInterface
    protected interface RetryAction {
        void run() throws Exception;
    }

    /**
     * Try to refresh server-side repo password cache by local encrypted cache.
     */
    protected final boolean tryRemoteVerifyWithCachedPassword(Account account, String repoId) throws SeafException {
        try {
            List<EncKeyCacheEntity> entities = AppDatabase.getInstance().encKeyCacheDAO().getListByRepoIdSync(repoId);
            if (CollectionUtils.isEmpty(entities)) {
                return false;
            }

            EncKeyCacheEntity entity = entities.get(0);
            if (TextUtils.isEmpty(entity.enc_key) || TextUtils.isEmpty(entity.enc_iv)) {
                return false;
            }

            String cachedPassword = SecurePasswordManager.decryptPassword(entity.enc_key, entity.enc_iv);
            if (TextUtils.isEmpty(cachedPassword)) {
                return false;
            }

            setRepoPassword(account, repoId, cachedPassword);
            updateLocalPasswordCache(repoId, cachedPassword);
            return true;
        } catch (Exception e) {
            throw ExceptionUtils.parseByThrowable(e);
        }
    }

    @Nullable
    protected final SeafException retryAfterPasswordRefresh(Account account,
                                                            String repoId,
                                                            SeafException seafException,
                                                            RetryAction retryAction) {
        if (seafException != SeafException.INVALID_PASSWORD) {
            return seafException;
        }

        try {
            if (!tryRemoteVerifyWithCachedPassword(account, repoId)) {
                return seafException;
            }
        } catch (SeafException refreshError) {
            return refreshError;
        }

        try {
            retryAction.run();
            return null;
        } catch (Exception retryError) {
            return retryError instanceof SeafException
                    ? (SeafException) retryError
                    : ExceptionUtils.parseByThrowable(retryError);
        }
    }

    private void setRepoPassword(Account account, String repoId, String password) throws IOException, SeafException {
        Map<String, String> requestDataMap = new HashMap<>();
        requestDataMap.put("password", password);

        retrofit2.Call<ResultModel> setPasswordCall = HttpManager.getHttpWithAccount(account)
                .execute(DialogService.class)
                .setPasswordSync(repoId, requestDataMap);
        retrofit2.Response<ResultModel> res = setPasswordCall.execute();
        if (res.isSuccessful()) {
            ResultModel resultModel = res.body();
            if (resultModel == null) {
                throw SeafException.ILL_FORMAT_EXCEPTION;
            }
            if (!resultModel.success) {
                throw SeafException.INVALID_PASSWORD;
            }
            SafeLogs.d(TAG, "setPassword()", "set password success");
            return;
        }

        int code = res.code();
        SafeLogs.d(TAG, "setPassword()", "set password failed: " + code);
        try (ResponseBody responseBody = res.errorBody()) {
            if (responseBody != null) {
                throw ExceptionUtils.parseHttpException(code, responseBody.string());
            } else {
                throw ExceptionUtils.parseHttpException(code, null);
            }
        }
    }

    private void updateLocalPasswordCache(String repoId, String password) {
        try {
            EncKeyCacheEntity encEntity = new EncKeyCacheEntity();
            encEntity.v = 2;
            encEntity.repo_id = repoId;

            Pair<String, String> p = SecurePasswordManager.encryptPassword(password);
            if (p == null) {
                return;
            }

            encEntity.enc_key = p.first;
            encEntity.enc_iv = p.second;

            long expire = TimeUtils.getNowMills() + SettingsManager.DECRYPTION_EXPIRATION_TIME;
            encEntity.expire_time_long = expire;
            AppDatabase.getInstance().encKeyCacheDAO().insertSync(encEntity);
        } catch (Exception e) {
            SafeLogs.e(TAG, "updateLocalPasswordCache()", e.getMessage());
        }
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

    private void send(FeatureDataSource dataSource, String event, Bundle extra) {
        Bundle b = new Bundle();
        b.putString(TransferWorker.KEY_DATA_SOURCE, dataSource.name());
        b.putString(TransferWorker.KEY_DATA_STATUS, event);
        if (extra != null) {
            b.putAll(extra);
        }
        send(b);
    }

    private void send(Bundle eventData) {
        BusHelper.getTransferProgressObserver().post(eventData);
    }
}
