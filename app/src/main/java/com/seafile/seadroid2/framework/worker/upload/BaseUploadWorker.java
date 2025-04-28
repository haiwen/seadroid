package com.seafile.seadroid2.framework.worker.upload;

import android.content.ContentResolver;
import android.content.Context;
import android.content.res.AssetFileDescriptor;
import android.net.Uri;
import android.os.Handler;
import android.os.Looper;
import android.text.TextUtils;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.work.ForegroundInfo;
import androidx.work.WorkerParameters;

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
import com.seafile.seadroid2.framework.worker.queue.TransferModel;
import com.seafile.seadroid2.framework.http.HttpIO;
import com.seafile.seadroid2.framework.notification.base.BaseTransferNotificationHelper;
import com.seafile.seadroid2.framework.util.ExceptionUtils;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.worker.ExistingFileStrategy;
import com.seafile.seadroid2.framework.worker.GlobalTransferCacheList;
import com.seafile.seadroid2.framework.worker.TransferWorker;
import com.seafile.seadroid2.framework.worker.body.ProgressRequestBody;
import com.seafile.seadroid2.framework.worker.body.ProgressUriRequestBody;
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

public abstract class BaseUploadWorker extends TransferWorker {
    public abstract BaseTransferNotificationHelper getNotification();

    public BaseUploadWorker(@NonNull Context context, @NonNull WorkerParameters workerParams) {
        super(context, workerParams);

        fileTransferProgressListener.setProgressListener(progressListener);
    }

    private final FileTransferProgressListener fileTransferProgressListener = new FileTransferProgressListener();

    /**
     * listener
     */
    private final FileTransferProgressListener.TransferProgressListener progressListener = new FileTransferProgressListener.TransferProgressListener() {
        @Override
        public void onProgressNotify(TransferModel transferModel, int percent, long transferredSize, long totalSize) {
            SLogs.d("UPLOAD: " + transferModel.file_name + " -> progress：" + percent);
            transferModel.transferred_size = transferredSize;
            GlobalTransferCacheList.updateTransferModel(transferModel);

            notifyProgress(transferModel.file_name, percent);

            sendProgressEvent(transferModel);
        }
    };

    private Call newCall;
    private OkHttpClient okHttpClient;

    @Override
    public void onStopped() {
        super.onStopped();

        SLogs.e("BaseUploadWorker onStopped");

        if (newCall != null) {
            newCall.cancel();  // 明确取消网络请求
            newCall = null;    // 清除引用
        }

        currentTransferModel.transfer_status = TransferStatus.CANCELLED;
        currentTransferModel.err_msg = SeafException.USER_CANCELLED_EXCEPTION.getMessage();
        GlobalTransferCacheList.updateTransferModel(currentTransferModel);

        // 释放所有资源引用
        if (fileTransferProgressListener != null) {
            fileTransferProgressListener.setProgressListener(null);
        }
        currentTransferModel = null;

        // 新增：关闭 OkHttp 连接池（谨慎使用）
        if (okHttpClient != null) {
            okHttpClient.dispatcher().executorService().shutdownNow();
            okHttpClient.connectionPool().evictAll();
        }
    }

    private TransferModel currentTransferModel;

    public void transfer(Account account, TransferModel transferModel) throws SeafException, IOException {
        try {
            currentTransferModel = CloneUtils.deepClone(transferModel, TransferModel.class);
            SLogs.e("开始上传文件：");
            SLogs.e(currentTransferModel.toString());
            transferFile(account);

            sendProgressFinishEvent(currentTransferModel);

        } catch (IOException | SeafException e) {
            SLogs.e(e);

            SeafException seafException = ExceptionUtils.parseByThrowable(e);

            // update db
            updateToFailed(seafException.getMessage());

            //send an event, update transfer entity first.
            sendProgressFinishEvent(currentTransferModel);
            throw seafException;
        }
    }

    private void transferFile(Account account) throws IOException, SeafException {
        if (account == null) {
            SLogs.d("account is null, can not upload file");
            throw SeafException.NOT_FOUND_USER_EXCEPTION;
        }

        if (TextUtils.isEmpty(account.token)) {
            SLogs.d("account is not logged in : " + account);
            throw SeafException.NOT_FOUND_LOGGED_USER_EXCEPTION;
        }

        if (isStopped()) {
            return;
        }

        SLogs.d("start transfer, local file path: " + currentTransferModel.full_path);

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
        fileTransferProgressListener.setTransferModel(currentTransferModel);

        //notify first
        sendProgressEvent(currentTransferModel);
        notifyProgress(currentTransferModel.file_name, 0);
        SLogs.d("start transfer, remote path: " + currentTransferModel.target_path);

        //update
        currentTransferModel.transferred_size = 0;
        currentTransferModel.transfer_status = TransferStatus.IN_PROGRESS;
        GlobalTransferCacheList.updateTransferModel(currentTransferModel);

        //uri: content://
        if (currentTransferModel.full_path.startsWith("content://")) {
            boolean isHasPermission = hasPermission(Uri.parse(currentTransferModel.full_path));
            if (!isHasPermission) {
                throw SeafException.PERMISSION_EXCEPTION;
            }

            ProgressUriRequestBody progressRequestBody = new ProgressUriRequestBody(getApplicationContext(), Uri.parse(currentTransferModel.full_path), currentTransferModel.file_size, fileTransferProgressListener);
            builder.addFormDataPart("file", currentTransferModel.file_name, progressRequestBody);
        } else {
            File file = new File(currentTransferModel.full_path);
            if (!file.exists()) {
                throw SeafException.NOT_FOUND_EXCEPTION;
            }

            ProgressRequestBody progressRequestBody = new ProgressRequestBody(file, fileTransferProgressListener);
            builder.addFormDataPart("file", currentTransferModel.file_name, progressRequestBody);
        }

        RequestBody requestBody = builder.build();

        //get upload link
        String uploadUrl = getFileUploadUrl(currentTransferModel.repo_id, currentTransferModel.getParentPath(), currentTransferModel.transfer_strategy == ExistingFileStrategy.REPLACE);
        if (TextUtils.isEmpty(uploadUrl)) {
            throw SeafException.REQUEST_TRANSFER_URL_EXCEPTION;
        }

        //
        if (newCall != null && newCall.isExecuted()) {
            SLogs.d("Folder upload: newCall has executed()");
            newCall.cancel();
        }

        Request request = new Request.Builder()
                .url(uploadUrl)
                .post(requestBody)
                .build();

        if (okHttpClient == null) {
            okHttpClient = HttpIO.getCurrentInstance().getOkHttpClient().getOkClient();
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

                            SLogs.d("result，file ID：" + str);
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
                    SLogs.d("upload failed：" + b);
                    //
                    if (!newCall.isCanceled()) {
                        newCall.cancel();
                    }
                    body.close();
                    throw ExceptionUtils.parse(code, b);
                }
            }
        }
    }

    private boolean hasPermission(Uri uri) {
        try {
            ContentResolver resolver = getApplicationContext().getContentResolver();
            AssetFileDescriptor afd = resolver.openAssetFileDescriptor(uri, "r");
            if (afd != null) {
                afd.close();
                return true;
            }
            return true;
        } catch (Exception e) {
            SLogs.e("URI权限检查失败: " + e.getMessage());
        }
        return false;
    }

    private String getFileUploadUrl(String repoId, String target_dir, boolean isUpdate) throws IOException, SeafException {
        retrofit2.Response<String> res;
        if (isUpdate) {
            res = HttpIO.getCurrentInstance()
                    .execute(FileService.class)
                    .getFileUpdateLink(repoId)
                    .execute();
        } else {
            res = HttpIO.getCurrentInstance()
                    .execute(FileService.class)
                    .getFileUploadLink(repoId, "/")
                    .execute();
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
                AppDatabase.getInstance().direntDao().updateFileIdByPath(transferEntity.repo_id,transferEntity.full_path,fileId);
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

    private void notifyProgress(String fileName, int percent) {
        if (getNotification() == null) {
            return;
        }

        BaseTransferNotificationHelper notification = getNotification();
        ForegroundInfo f = notification.getForegroundProgressNotification(fileName, percent);
        showForegroundAsync(f);
    }

    public void notifyError(SeafException seafException) {
        if (getNotification() == null) {
            return;
        }

        if (seafException == SeafException.OUT_OF_QUOTA) {
            getGeneralNotificationHelper().showErrorNotification(R.string.above_quota, getNotification().getDefaultTitle());
        } else if (seafException == SeafException.NETWORK_EXCEPTION) {
            getGeneralNotificationHelper().showErrorNotification(R.string.network_error, getNotification().getDefaultTitle());
        } else if (seafException == SeafException.NOT_FOUND_USER_EXCEPTION) {
            getGeneralNotificationHelper().showErrorNotification(R.string.saf_account_not_found_exception, getNotification().getDefaultTitle());
        } else if (seafException == SeafException.USER_CANCELLED_EXCEPTION) {
            //do nothing
        } else {
            getGeneralNotificationHelper().showErrorNotification(seafException.getMessage(), getNotification().getDefaultTitle());
        }
    }
}
