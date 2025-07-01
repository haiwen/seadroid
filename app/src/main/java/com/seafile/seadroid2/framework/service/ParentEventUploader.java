package com.seafile.seadroid2.framework.service;

import android.content.Context;
import android.net.Uri;
import android.text.TextUtils;

import androidx.annotation.NonNull;

import com.blankj.utilcode.util.CloneUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.config.Constants;
import com.seafile.seadroid2.enums.FeatureDataSource;
import com.seafile.seadroid2.enums.SaveTo;
import com.seafile.seadroid2.enums.TransferResult;
import com.seafile.seadroid2.enums.TransferStatus;
import com.seafile.seadroid2.framework.db.AppDatabase;
import com.seafile.seadroid2.framework.db.entities.FileBackupStatusEntity;
import com.seafile.seadroid2.framework.db.entities.FileCacheStatusEntity;
import com.seafile.seadroid2.framework.http.HttpIO;
import com.seafile.seadroid2.framework.notification.TransferNotificationDispatcher;
import com.seafile.seadroid2.framework.util.ExceptionUtils;
import com.seafile.seadroid2.framework.util.FileUtils;
import com.seafile.seadroid2.framework.util.SafeLogs;
import com.seafile.seadroid2.framework.util.Times;
import com.seafile.seadroid2.framework.worker.ExistingFileStrategy;
import com.seafile.seadroid2.framework.worker.GlobalTransferCacheList;
import com.seafile.seadroid2.framework.worker.body.ProgressRequestBody;
import com.seafile.seadroid2.framework.worker.body.ProgressUriRequestBody;
import com.seafile.seadroid2.framework.worker.queue.TransferModel;
import com.seafile.seadroid2.listener.FileTransferProgressListener;
import com.seafile.seadroid2.ui.file.FileService;

import org.apache.commons.lang3.StringUtils;

import java.io.File;
import java.io.IOException;

import okhttp3.Call;
import okhttp3.Callback;
import okhttp3.MultipartBody;
import okhttp3.OkHttpClient;
import okhttp3.Protocol;
import okhttp3.Request;
import okhttp3.RequestBody;
import okhttp3.Response;
import okhttp3.ResponseBody;

public abstract class ParentEventUploader extends ParentEventTransfer {
    private final String TAG = "ParentEventUploader";
    private final TransferNotificationDispatcher transferNotificationDispatcher;

    public ParentEventUploader(Context context, TransferNotificationDispatcher transferNotificationDispatcher) {
        super(context);
        this.transferNotificationDispatcher = transferNotificationDispatcher;
        _fileTransferProgressListener.setProgressListener(progressListener);
    }

    public TransferNotificationDispatcher getTransferNotificationDispatcher() {
        return transferNotificationDispatcher;
    }

    public abstract FeatureDataSource getFeatureDataSource();

    /**
     * listener
     */
    private final FileTransferProgressListener _fileTransferProgressListener = new FileTransferProgressListener();

    /**
     * progress listener
     */
    private final FileTransferProgressListener.TransferProgressListener progressListener = new FileTransferProgressListener.TransferProgressListener() {
        @Override
        public void onProgressNotify(TransferModel transferModel, int percent, long transferredSize, long totalSize) {
            SafeLogs.d(TAG, "onProgressNotify()", "UPLOAD: " + transferModel.file_name + " -> progress：" + percent);

            transferModel.transferred_size = transferredSize;
            GlobalTransferCacheList.updateTransferModel(transferModel);

            notifyProgress(transferModel.file_name, percent);

            sendProgressEvent(getFeatureDataSource(), transferModel);
        }
    };


    private void notifyProgress(String fileName, int percent) {
        if (transferNotificationDispatcher == null) {
            return;
        }

        transferNotificationDispatcher.showProgress(getFeatureDataSource(), fileName, percent);
    }

    public void notifyError(SeafException seafException) {
        if (getGeneralNotificationHelper() == null) {
            return;
        }

        if (seafException == SeafException.OUT_OF_QUOTA) {
            getGeneralNotificationHelper().showErrorNotification(R.string.above_quota);
        } else if (seafException == SeafException.NETWORK_EXCEPTION) {
            getGeneralNotificationHelper().showErrorNotification(R.string.network_error);
        } else if (seafException == SeafException.NOT_FOUND_USER_EXCEPTION) {
            getGeneralNotificationHelper().showErrorNotification(R.string.saf_account_not_found_exception);
        } else if (seafException == SeafException.USER_CANCELLED_EXCEPTION) {
            //do nothing
        } else {
            getGeneralNotificationHelper().showErrorNotification(seafException.getMessage());
        }
    }

    private TransferModel currentTransferModel;
    private Call newCall;

    private ProgressUriRequestBody uriRequestBody;
    private ProgressRequestBody fileRequestBody;

    private boolean isStop = false;
    private OkHttpClient primaryHttpClient;
    private OkHttpClient fallbackHttpClient;

    public OkHttpClient getPrimaryHttpClient(Account account) {
        if (primaryHttpClient == null) {
            primaryHttpClient = HttpIO.getInstanceByAccount(account).getSafeClient().getOkClient();
        }
        return primaryHttpClient;
    }

    public OkHttpClient getFallbackHttpClient(Account account) {
        if (fallbackHttpClient == null) {
            fallbackHttpClient = HttpIO.getInstanceByAccount(account).getSafeClient().getOkClient(true);
        }
        return fallbackHttpClient;
    }

    public TransferModel getCurrentTransferringModel() {
        return currentTransferModel;
    }

    /**
     * Stop downloading the model
     * <p>
     * the model is in the downloading, it will be stopped.
     */
    public void stopThis() {
        SafeLogs.d(TAG, "stopThis()", getFeatureDataSource().name());
        isStop = true;

        if (uriRequestBody != null) {
            uriRequestBody.setStop(true);
        }

        if (fileRequestBody != null) {
            fileRequestBody.setStop(true);
        }

        if (primaryHttpClient != null) {
            primaryHttpClient.dispatcher().cancelAll();
        }

        if (newCall != null) {
            newCall.cancel();
        }

    }

    public void transfer(Account account, TransferModel transferModel) throws SeafException {
        try {
            if (isStop) {
                isStop = false;
            }

            currentTransferModel = CloneUtils.deepClone(transferModel, TransferModel.class);
            SafeLogs.d(TAG, "transfer start, model:");
            SafeLogs.d(TAG, currentTransferModel.toString());

            transferFile(account);

            sendProgressCompleteEvent(getFeatureDataSource(), currentTransferModel);

        } catch (SeafException seafException) {
            // update db
            updateToFailed(seafException.getMessage());

            //send an event, update transfer entity first.
            sendProgressCompleteEvent(getFeatureDataSource(), currentTransferModel);
            throw seafException;
        }
    }


    private void transferFile(Account account) throws SeafException {
        if (account == null) {
            SafeLogs.d(TAG, "transferFile()", "account is null, can not upload file");
            throw SeafException.NOT_FOUND_USER_EXCEPTION;
        }

        if (TextUtils.isEmpty(account.token)) {
            SafeLogs.d(TAG, "transferFile()", "account is not logged in : " + account);
            throw SeafException.UNAUTHORIZED_EXCEPTION;
        }

        SafeLogs.d(TAG, "transferFile()", "start transfer, local file path: " + currentTransferModel.full_path);

        //net
        MultipartBody.Builder builder = new MultipartBody.Builder();
        builder.setType(MultipartBody.FORM);

        if (currentTransferModel.transfer_strategy == ExistingFileStrategy.REPLACE) {
            builder.addFormDataPart("target_file", currentTransferModel.target_path);
        } else {
            //parent_dir: / is repo root
            builder.addFormDataPart("parent_dir", "/");

//            parent_dir is the root directory.
//            when select the root of the repo, relative_path is null.
            String dir = currentTransferModel.getParentPath();
            dir = StringUtils.removeStart(dir, "/");
//
            builder.addFormDataPart("relative_path", dir);
        }

        //
        _fileTransferProgressListener.setCurrentTransferModel(currentTransferModel);

        //notify first
        sendProgressEvent(getFeatureDataSource(), currentTransferModel);

        notifyProgress(currentTransferModel.file_name, 0);
        SafeLogs.d(TAG, "transferFile()", "start transfer, remote path: " + currentTransferModel.target_path);

        //update
        currentTransferModel.transferred_size = 0;
        currentTransferModel.transfer_status = TransferStatus.IN_PROGRESS;
        GlobalTransferCacheList.updateTransferModel(currentTransferModel);

        long createdTime = -1;
        //uri: content://
        if (currentTransferModel.full_path.startsWith("content://")) {
            Uri uri = Uri.parse(currentTransferModel.full_path);
            boolean isHasPermission = FileUtils.isUriHasPermission(getContext(), uri);
            if (!isHasPermission) {
                throw SeafException.PERMISSION_EXCEPTION;
            }

            uriRequestBody = new ProgressUriRequestBody(getContext(), Uri.parse(currentTransferModel.full_path), currentTransferModel.file_size, _fileTransferProgressListener);
            builder.addFormDataPart("file", currentTransferModel.file_name, uriRequestBody);

            createdTime = FileUtils.getCreatedTimeFromUri(getContext(), uri);
        } else {
            File file = new File(currentTransferModel.full_path);
            if (!file.exists()) {
                throw SeafException.NOT_FOUND_EXCEPTION;
            }

            fileRequestBody = new ProgressRequestBody(file, _fileTransferProgressListener);
            builder.addFormDataPart("file", currentTransferModel.file_name, fileRequestBody);
            createdTime = FileUtils.getCreatedTimeFromPath(getContext(), file);
        }


        if (createdTime != -1) {
            String cTime = Times.convertLong2Time(createdTime);
            SafeLogs.d(TAG, "file create timestamp : " + cTime);
            builder.addFormDataPart("last_modify", cTime);
        }

        RequestBody requestBody = builder.build();
        //get upload link
        String uploadUrl = getFileUploadUrl(account, currentTransferModel.repo_id, currentTransferModel.getParentPath(), currentTransferModel.transfer_strategy == ExistingFileStrategy.REPLACE);
        if (TextUtils.isEmpty(uploadUrl)) {
            throw SeafException.REQUEST_URL_EXCEPTION;
        }

        //
        if (newCall != null && newCall.isExecuted()) {
            SafeLogs.d(TAG, "transferFile()", "newCall has executed(), cancel it");
            newCall.cancel();
        }

        Request request = new Request.Builder()
                .url(uploadUrl)
                .post(requestBody)
                .addHeader("Connection", "keep-alive")
                .addHeader("Accept", "*/*")
                .addHeader("User-Agent", Constants.UA.SEAFILE_ANDROID_UA)
                .addHeader("User-Agent", Constants.UA.SEAFILE_ANDROID_UPLOAD_UA)
                .build();


        newCall = getPrimaryHttpClient(account).newCall(request);

        try (Response response = newCall.execute()) {
            onRes(response);
        } catch (IOException e) {
            SafeLogs.e(TAG, e.getMessage());
            SafeLogs.e(e);
            onFallback(account, request);
        }
    }

    private void onFallback(Account account, Request request) throws SeafException {
        if (newCall != null && newCall.isExecuted()) {
            SafeLogs.d(TAG, "onFallback()", "newCall has executed(), cancel it");
            newCall.cancel();
        }

        SafeLogs.d(TAG, "onFallback()", "use fallback client to upload file");

        newCall = getFallbackHttpClient(account).newCall(request);

        try (Response response = newCall.execute()) {
            onRes(response);
        } catch (IOException e) {
            SafeLogs.e(TAG, e.getMessage());
            SafeLogs.e(e);

            throw SeafException.NETWORK_EXCEPTION;
        }
    }

    private boolean onRes(Response response) throws SeafException, IOException {
        int code = response.code();
        boolean isFallback = false;

        Protocol protocol = response.protocol();
        SafeLogs.d(TAG, "onRes()", "response code: " + code + ", protocol: " + protocol);
        if (Protocol.HTTP_2 == protocol) {
            isFallback = true;
        } else if (Protocol.QUIC == protocol) {
            isFallback = true;
        } else if (Protocol.H2_PRIOR_KNOWLEDGE == protocol) {
            isFallback = true;
        } else if (Protocol.HTTP_1_1 == protocol) {
            isFallback = false;
        }

        try (ResponseBody body = response.body()) {
            if (body == null) {
                SafeLogs.d(TAG, "transferFile()", "body is null");

                if (response.isSuccessful()) {
                    // if the returned data is abnormal due to some reason,
                    // it is set to null and uploaded when the next scan arrives
                    updateToSuccess(null);
                } else {
                    throw ExceptionUtils.parseHttpException(code, null);
                }

                return isFallback;
            }

            String bodyStr = body.string();
            if (response.isSuccessful()) {
                if (TextUtils.isEmpty(bodyStr)) {
                    // if the returned data is abnormal due to some reason,
                    // it is set to null and uploaded when the next scan arrives
                    updateToSuccess(null);
                } else {
                    String fileId = bodyStr.replace("\"", "");

                    SafeLogs.d(TAG, "transferFile()", "result，file ID：" + bodyStr);
                    updateToSuccess(fileId);
                }
            } else {
                throw ExceptionUtils.parseHttpException(code, bodyStr);
            }
        }
        return isFallback;
    }


    private String getFileUploadUrl(Account account, String repoId, String target_dir,
                                    boolean isUpdate) throws SeafException {
        retrofit2.Response<String> res;
        try {
            if (isUpdate) {
                res = HttpIO.getInstanceByAccount(account)
                        .execute(FileService.class)
                        .getFileUpdateLink(repoId)
                        .execute();
            } else {
                res = HttpIO.getInstanceByAccount(account)
                        .execute(FileService.class)
                        .getFileUploadLink(repoId, "/")
                        .execute();
            }
        } catch (IOException e) {
            SafeLogs.e(TAG, e.getMessage());
            throw SeafException.NETWORK_EXCEPTION;
        }

        if (!res.isSuccessful()) {
            throw SeafException.REQUEST_URL_EXCEPTION;
        }

        String urlStr = res.body();
        urlStr = StringUtils.replace(urlStr, "\"", "");

        return urlStr;
    }

    public void updateToFailed(String transferResult) {
        currentTransferModel.transferred_size = 0L;
        currentTransferModel.transfer_status = TransferStatus.FAILED;
        currentTransferModel.err_msg = transferResult;
        GlobalTransferCacheList.updateTransferModel(currentTransferModel);
    }

    private void updateToSuccess(String fileId) {
        currentTransferModel.transferred_size = currentTransferModel.file_size;
        currentTransferModel.transfer_status = TransferStatus.SUCCEEDED;
        currentTransferModel.err_msg = TransferResult.TRANSMITTED.name();
        GlobalTransferCacheList.updateTransferModel(currentTransferModel);

        if (currentTransferModel.save_to == SaveTo.DB) {
            if (currentTransferModel.data_source == FeatureDataSource.AUTO_UPDATE_LOCAL_FILE) {
                FileCacheStatusEntity transferEntity = FileCacheStatusEntity.convertFromUpload(currentTransferModel, fileId);
                AppDatabase.getInstance().fileCacheStatusDAO().insert(transferEntity);

                //
                AppDatabase.getInstance().direntDao().updateFileIdByPath(transferEntity.repo_id, transferEntity.full_path, fileId);
            } else {
                FileBackupStatusEntity transferEntity = FileBackupStatusEntity.convertTransferModel2This(currentTransferModel, fileId);
                AppDatabase.getInstance().fileTransferDAO().insert(transferEntity);
            }
        }
    }

    public boolean isInterrupt(SeafException result) {
        if (result.equals(SeafException.OUT_OF_QUOTA) ||
                result.equals(SeafException.INVALID_PASSWORD) ||
                result.equals(SeafException.SSL_EXCEPTION) ||
                result.equals(SeafException.UNAUTHORIZED_EXCEPTION) ||
                result.equals(SeafException.NOT_FOUND_USER_EXCEPTION) ||
                result.equals(SeafException.USER_CANCELLED_EXCEPTION)) {
            return true;
        }
        return false;
    }

}
