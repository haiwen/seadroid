package com.seafile.seadroid2.framework.service;

import android.content.Context;
import android.net.Uri;
import android.text.TextUtils;

import com.blankj.utilcode.util.CloneUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.enums.SaveTo;
import com.seafile.seadroid2.enums.TransferDataSource;
import com.seafile.seadroid2.enums.TransferResult;
import com.seafile.seadroid2.enums.TransferStatus;
import com.seafile.seadroid2.framework.db.AppDatabase;
import com.seafile.seadroid2.framework.db.entities.FileBackupStatusEntity;
import com.seafile.seadroid2.framework.db.entities.FileCacheStatusEntity;
import com.seafile.seadroid2.framework.http.HttpIO;
import com.seafile.seadroid2.framework.notification.base.BaseTransferNotificationHelper;
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
import okhttp3.MultipartBody;
import okhttp3.OkHttpClient;
import okhttp3.Request;
import okhttp3.RequestBody;
import okhttp3.Response;
import okhttp3.ResponseBody;

public abstract class ParentEventUploader extends ParentEventTransfer {
    private final String TAG = "ParentEventUploader";

    public ParentEventUploader(Context context) {
        super(context);

        _fileTransferProgressListener.setProgressListener(progressListener);
    }

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

            sendProgressEvent(transferModel);
        }
    };


    public abstract BaseTransferNotificationHelper getNotificationHelper();

    private void notifyProgress(String fileName, int percent) {
        if (getNotificationHelper() == null) {
            return;
        }

        getNotificationHelper().notifyProgress(fileName, percent);
    }

    public void notifyError(SeafException seafException) {
        if (getGeneralNotificationHelper() == null) {
            return;
        }

        if (seafException == SeafException.OUT_OF_QUOTA) {
            getGeneralNotificationHelper().showErrorNotification(R.string.above_quota, getNotificationHelper().getDefaultTitle());
        } else if (seafException == SeafException.NETWORK_EXCEPTION) {
            getGeneralNotificationHelper().showErrorNotification(R.string.network_error, getNotificationHelper().getDefaultTitle());
        } else if (seafException == SeafException.NOT_FOUND_USER_EXCEPTION) {
            getGeneralNotificationHelper().showErrorNotification(R.string.saf_account_not_found_exception, getNotificationHelper().getDefaultTitle());
        } else if (seafException == SeafException.USER_CANCELLED_EXCEPTION) {
            //do nothing
        } else {
            getGeneralNotificationHelper().showErrorNotification(seafException.getMessage(), getNotificationHelper().getDefaultTitle());
        }
    }

    private TransferModel currentTransferModel;
    private Call newCall;
    private OkHttpClient okHttpClient;

    public TransferModel getCurrentTransferringModel() {
        return currentTransferModel;
    }

    private boolean isStop = false;

    /**
     * Stop downloading the model
     * <p>
     * the model is in the downloading, it will be stopped.
     */
    public void stopThis() {
        SafeLogs.d(TAG, "stop()", "stop download");
        isStop = true;

        if (uriRequestBody != null) {
            uriRequestBody.setStop(true);
        }

        if (fileRequestBody != null) {
            fileRequestBody.setStop(true);
        }

        if (okHttpClient != null) {
            okHttpClient.dispatcher().cancelAll();
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

            sendProgressFinishEvent(currentTransferModel);

        } catch (SeafException seafException) {
            // update db
            updateToFailed(seafException.getMessage());

            //send an event, update transfer entity first.
            sendProgressFinishEvent(currentTransferModel);
            throw seafException;
        }
    }

    private ProgressUriRequestBody uriRequestBody;
    private ProgressRequestBody fileRequestBody;

    private void transferFile(Account account) throws SeafException {
        if (account == null) {
            SafeLogs.d(TAG, "transferFile()", "account is null, can not upload file");
            throw SeafException.NOT_FOUND_USER_EXCEPTION;
        }

        if (TextUtils.isEmpty(account.token)) {
            SafeLogs.d(TAG, "transferFile()", "account is not logged in : " + account);
            throw SeafException.NOT_FOUND_LOGGED_USER_EXCEPTION;
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
        sendProgressEvent(currentTransferModel);

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
            throw SeafException.REQUEST_TRANSFER_URL_EXCEPTION;
        }

        //
        if (newCall != null && newCall.isExecuted()) {
            SafeLogs.d(TAG, "transferFile()", "newCall has executed(), cancel it");
            newCall.cancel();
        }

        Request request = new Request.Builder()
                .url(uploadUrl)
                .post(requestBody)
                .build();

        if (okHttpClient == null) {
            okHttpClient = HttpIO.getInstanceByAccount(account).getOkHttpClient().getOkClient();
        }

        newCall = okHttpClient.newCall(request);

        try (Response response = newCall.execute()) {
            if (response.isSuccessful()) {
                try (ResponseBody body = response.body()) {
                    if (body != null) {
                        String str = body.string();
                        if (TextUtils.isEmpty(str)) {
                            // if the returned data is abnormal due to some reason,
                            // it is set to null and uploaded when the next scan arrives
                            updateToSuccess(null);
                        } else {
                            String fileId = str.replace("\"", "");

                            SafeLogs.d(TAG, "transferFile()", "result，file ID：" + str);
                            updateToSuccess(fileId);
                        }
                    } else {
                        // if the returned data is abnormal due to some reason,
                        // it is set to null and uploaded when the next scan arrives
                        updateToSuccess(null);
                    }
                }
            } else {
                int code = response.code();
                ResponseBody body = response.body();
                if (body != null) {
                    String b = body.string();
                    SafeLogs.d(TAG, "transferFile()", "upload failed：" + b);
                    //
                    if (!newCall.isCanceled()) {
                        newCall.cancel();
                    }

                    body.close();
                    throw ExceptionUtils.parse(code, b);
                }
            }
        } catch (IOException e) {
            SafeLogs.e(TAG, e.getMessage());
            throw SeafException.NETWORK_EXCEPTION;
        }
    }


    private String getFileUploadUrl(Account account, String repoId, String target_dir, boolean isUpdate) throws SeafException {
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
            throw SeafException.REQUEST_TRANSFER_URL_EXCEPTION;
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
            if (currentTransferModel.data_source == TransferDataSource.DOWNLOAD) {
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
                result.equals(SeafException.NOT_FOUND_LOGGED_USER_EXCEPTION) ||
                result.equals(SeafException.NOT_FOUND_USER_EXCEPTION) ||
                result.equals(SeafException.USER_CANCELLED_EXCEPTION)) {
            return true;
        }
        return false;
    }

}
