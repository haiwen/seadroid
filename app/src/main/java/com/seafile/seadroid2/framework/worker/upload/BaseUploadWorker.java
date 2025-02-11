package com.seafile.seadroid2.framework.worker.upload;

import android.content.Context;
import android.content.UriPermission;
import android.net.Uri;
import android.text.TextUtils;

import androidx.annotation.NonNull;
import androidx.work.ForegroundInfo;
import androidx.work.WorkerParameters;

import com.blankj.utilcode.util.CloneUtils;
import com.blankj.utilcode.util.CollectionUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.enums.TransferDataSource;
import com.seafile.seadroid2.enums.TransferResult;
import com.seafile.seadroid2.enums.TransferStatus;
import com.seafile.seadroid2.framework.data.db.AppDatabase;
import com.seafile.seadroid2.framework.data.db.entities.DirentModel;
import com.seafile.seadroid2.framework.data.db.entities.FileTransferEntity;
import com.seafile.seadroid2.framework.http.HttpIO;
import com.seafile.seadroid2.framework.notification.base.BaseTransferNotificationHelper;
import com.seafile.seadroid2.framework.util.ExceptionUtils;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.worker.ExistingFileStrategy;
import com.seafile.seadroid2.framework.worker.TransferEvent;
import com.seafile.seadroid2.framework.worker.TransferWorker;
import com.seafile.seadroid2.framework.worker.body.ProgressRequestBody;
import com.seafile.seadroid2.framework.worker.body.ProgressUriRequestBody;
import com.seafile.seadroid2.listener.FileTransferProgressListener;
import com.seafile.seadroid2.ui.file.FileService;

import org.apache.commons.lang3.StringUtils;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.List;

import okhttp3.Call;
import okhttp3.MultipartBody;
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
        public void onProgressNotify(FileTransferEntity fileTransferEntity, int percent, long transferredSize, long totalSize) {
            SLogs.d(fileTransferEntity.file_name + " -> progress：" + percent);
            notifyProgress(fileTransferEntity.file_name, percent);

            //
            AppDatabase.getInstance().fileTransferDAO().update(fileTransferEntity);

            sendProgressNotifyEvent(fileTransferEntity.file_name, fileTransferEntity.uid, percent, transferredSize, totalSize, fileTransferEntity.data_source);

        }
    };

    private Call newCall;

    @Override
    public void onStopped() {
        super.onStopped();

//        cancelNotification();

        SLogs.e("BaseUploadWorker onStopped");
        currentTransferEntity.transfer_status = TransferStatus.CANCELLED;
        currentTransferEntity.result = SeafException.USER_CANCELLED_EXCEPTION.getMessage();
        AppDatabase.getInstance().fileTransferDAO().update(currentTransferEntity);

        if (newCall != null && !newCall.isCanceled()) {
            newCall.cancel();
        }
    }


    private FileTransferEntity currentTransferEntity;

    public void transfer(Account account, FileTransferEntity transferEntity, long totalPendingCount) throws SeafException, IOException {
        try {
            transferFile(account, transferEntity);

            //send an event, update transfer entity first.
            sendFinishEvent(account, transferEntity, totalPendingCount);

        } catch (IOException | SeafException e) {
            SLogs.e(e);

            SeafException seafException = ExceptionUtils.getExceptionByThrowable(e);

            // update db
            updateToFailed(seafException.getMessage());

            //send an event, update transfer entity first.
            sendFinishEvent(account, transferEntity, totalPendingCount);
            throw seafException;
        }
    }

    private void transferFile(Account account, FileTransferEntity transferEntity) throws IOException, SeafException {
        if (account == null) {
            SLogs.d("account is null, can not upload file");
            throw SeafException.NOT_FOUND_USER_EXCEPTION;
        }

        if (TextUtils.isEmpty(account.token)) {
            SLogs.d("account is not logged in : " + account);
            throw SeafException.NOT_FOUND_LOGGED_USER_EXCEPTION;
        }


        SLogs.d("start transfer, full_path: " + transferEntity.full_path);
        currentTransferEntity = CloneUtils.deepClone(transferEntity,FileTransferEntity.class);

        if (isStopped()) {
            return;
        }

        ExistingFileStrategy fileStrategy = currentTransferEntity.file_strategy;
        if (fileStrategy == ExistingFileStrategy.SKIP) {
            SLogs.d("folder backup: skip file(remote exists): " + currentTransferEntity.target_path);
            return;
        }

        //net
        MultipartBody.Builder builder = new MultipartBody.Builder();
        builder.setType(MultipartBody.FORM);

        if (currentTransferEntity.file_strategy == ExistingFileStrategy.REPLACE) {
            builder.addFormDataPart("target_file", currentTransferEntity.target_path);
        } else {
            //parent_dir: / is repo root
            builder.addFormDataPart("parent_dir", "/");
//            builder.addFormDataPart("parent_dir", transferEntity.getParent_path());

//            parent_dir is the root directory.
//            when select the root of the repo, relative_path is null.
            String dir = currentTransferEntity.getParent_path();
            dir = StringUtils.removeStart(dir, "/");
//
            builder.addFormDataPart("relative_path", dir);
        }

        //
        fileTransferProgressListener.setFileTransferEntity(currentTransferEntity);

        //show notification
        notifyProgress(currentTransferEntity.file_name, 0);
        SLogs.d("start transfer, target_path: " + currentTransferEntity.target_path);

        //db
        currentTransferEntity.transfer_status = TransferStatus.IN_PROGRESS;
        AppDatabase.getInstance().fileTransferDAO().update(currentTransferEntity);

        //uri: content://
        if (currentTransferEntity.full_path.startsWith("content://")) {

            boolean isHasPermission = hasPermission(Uri.parse(currentTransferEntity.full_path));
            if (!isHasPermission){
                throw SeafException.PERMISSION_EXCEPTION;
            }

            ProgressUriRequestBody progressRequestBody = new ProgressUriRequestBody(getApplicationContext(), Uri.parse(currentTransferEntity.full_path), currentTransferEntity.file_size, fileTransferProgressListener);
            builder.addFormDataPart("file", currentTransferEntity.file_name, progressRequestBody);
        } else {
            File file = new File(currentTransferEntity.full_path);
            if (!file.exists()) {
                throw SeafException.NOT_FOUND_EXCEPTION;
            }

            ProgressRequestBody progressRequestBody = new ProgressRequestBody(file, fileTransferProgressListener);
            builder.addFormDataPart("file", currentTransferEntity.file_name, progressRequestBody);
        }


        RequestBody requestBody = builder.build();

        //get upload link
        String uploadUrl = getFileUploadUrl(currentTransferEntity.repo_id, currentTransferEntity.getParent_path(), currentTransferEntity.file_strategy == ExistingFileStrategy.REPLACE);
        if (TextUtils.isEmpty(uploadUrl)) {
            throw SeafException.REQUEST_TRANSFER_URL_EXCEPTION;
        }

        //
        if (newCall != null && newCall.isExecuted()) {
            SLogs.d("Folder upload: newCall has executed()");
        }

        Request request = new Request.Builder()
                .url(uploadUrl)
                .post(requestBody)
                .build();
        newCall = HttpIO.getCurrentInstance().getOkHttpClient().getOkClient().newCall(request);

        try (Response response = newCall.execute()) {
            if (response.isSuccessful()) {
                ResponseBody body = response.body();
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

            } else {
                int code = response.code();
                String b = response.body() != null ? response.body().string() : null;
                SLogs.d("upload failed：" + b);

                //
                if (!newCall.isCanceled()) {
                    newCall.cancel();
                }

                throw ExceptionUtils.parseErrorJson(code, b);
            }
        }
    }

    private boolean hasPermission(Uri uri) {
        List<UriPermission> permissions = getApplicationContext().getContentResolver().getPersistedUriPermissions();
        for (UriPermission permission : permissions) {
            if (permission.getUri().equals(uri) && permission.isReadPermission()) {
                return true;
            }
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

//            target_dir = StringUtils.removeEnd(target_dir, "/");

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
        currentTransferEntity.transferred_size = 0L;
        currentTransferEntity.modified_at = System.currentTimeMillis();
        currentTransferEntity.transfer_status = TransferStatus.FAILED;
        currentTransferEntity.result = transferResult;

        AppDatabase.getInstance().fileTransferDAO().update(currentTransferEntity);
    }

    private void updateToSuccess(String fileId) {
        //db
        currentTransferEntity.file_id = fileId;
        currentTransferEntity.transferred_size = currentTransferEntity.file_size;
        currentTransferEntity.action_end_at = System.currentTimeMillis();
        currentTransferEntity.modified_at = currentTransferEntity.action_end_at;
        currentTransferEntity.result = TransferResult.TRANSMITTED.name();
        currentTransferEntity.transfer_status = TransferStatus.SUCCEEDED;

        AppDatabase.getInstance().fileTransferDAO().update(currentTransferEntity);

        //update
        List<DirentModel> direntList = AppDatabase.getInstance().direntDao().getListByFullPathSync(currentTransferEntity.repo_id, currentTransferEntity.full_path);
        if (!CollectionUtils.isEmpty(direntList)) {
            DirentModel direntModel = direntList.get(0);
            direntModel.last_modified_at = currentTransferEntity.modified_at;
            direntModel.id = fileId;
            direntModel.size = currentTransferEntity.file_size;
            direntModel.transfer_status = currentTransferEntity.transfer_status;

            AppDatabase.getInstance().direntDao().update(direntModel);
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
        if (seafException == SeafException.OUT_OF_QUOTA) {
            getGeneralNotificationHelper().showErrorNotification(R.string.above_quota, getNotification().getDefaultTitle());
        } else if (seafException == SeafException.NETWORK_EXCEPTION) {
            getGeneralNotificationHelper().showErrorNotification(R.string.network_error, getNotification().getDefaultTitle());
        } else if (seafException == SeafException.NOT_FOUND_USER_EXCEPTION) {
            getGeneralNotificationHelper().showErrorNotification(R.string.saf_account_not_found_exception, getNotification().getDefaultTitle());
        } else {
            getGeneralNotificationHelper().showErrorNotification(seafException.getMessage(), getNotification().getDefaultTitle());
        }
    }
}
