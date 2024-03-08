package com.seafile.seadroid2.worker;

import android.content.Context;
import android.text.TextUtils;

import androidx.annotation.NonNull;
import androidx.work.Data;
import androidx.work.WorkerParameters;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.ToastUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.data.db.AppDatabase;
import com.seafile.seadroid2.data.db.entities.FileTransferEntity;
import com.seafile.seadroid2.data.model.enums.TransferAction;
import com.seafile.seadroid2.data.model.enums.TransferFeature;
import com.seafile.seadroid2.data.model.enums.TransferResult;
import com.seafile.seadroid2.data.model.enums.TransferStatus;
import com.seafile.seadroid2.io.http.IO;
import com.seafile.seadroid2.listener.FileTransferProgressListener;
import com.seafile.seadroid2.notification.AlbumBackupNotificationManager;
import com.seafile.seadroid2.ui.repo.RepoService;
import com.seafile.seadroid2.util.SLogs;
import com.seafile.seadroid2.util.Utils;
import com.seafile.seadroid2.worker.body.ProgressRequestBody;

import org.apache.commons.lang3.StringUtils;
import org.json.JSONException;

import java.io.File;
import java.io.IOException;
import java.util.List;

import okhttp3.Call;
import okhttp3.MultipartBody;
import okhttp3.Request;
import okhttp3.RequestBody;
import okhttp3.Response;

public class UploadMediaWorker extends TransferWorker {
    private AlbumBackupNotificationManager notificationManager;
    private final FileTransferProgressListener fileTransferProgressListener = new FileTransferProgressListener();

    public UploadMediaWorker(@NonNull Context context, @NonNull WorkerParameters workerParams) {
        super(context, workerParams);

        notificationManager = new AlbumBackupNotificationManager(context);
        fileTransferProgressListener.setProgressListener(progressListener);
    }

    @NonNull
    @Override
    public Result doWork() {

        notificationManager.dismissNotification();

        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        if (account == null) {
            return Result.failure();
        }
        boolean isUploaded = false;
        while (true) {
            List<FileTransferEntity> list = AppDatabase
                    .getInstance()
                    .fileTransferDAO()
                    .getListByAction(account.getSignature(),
                            TransferAction.UPLOAD,
                            TransferStatus.TRANSFER_WAITING,
                            TransferFeature.ALBUM_BACKUP);

            if (CollectionUtils.isEmpty(list)) {
                break;
            }

            SLogs.d("start upload media worker");

            isUploaded = true;

            for (FileTransferEntity fileTransferEntity : list) {
                if (isStopped()) {
                    break;
                }

                try {
                    transferFile(account, fileTransferEntity);
                } catch (IOException | SeafException | JSONException e) {
                    throw new RuntimeException(e);
                }
            }
        }

        //
        if (isUploaded) {
            ToastUtils.showLong(R.string.upload_finished);
            SLogs.d("all task run");
        } else {
            SLogs.d("nothing to run");
        }

        notificationManager.dismissNotification();

        return Result.success();
    }

    @Override
    public void onStopped() {
        super.onStopped();

        notificationManager.dismissNotification();
        if (newCall != null) {
            newCall.cancel();
        }
    }

    private final FileTransferProgressListener.TransferProgressListener progressListener = new FileTransferProgressListener.TransferProgressListener() {
        @Override
        public void onProgressNotify(FileTransferEntity fileTransferEntity, int percent, long transferredSize, long totalSize) {
            SLogs.e(fileTransferEntity.file_name + " -> 上传进度：" + percent);
            notificationManager.updateProgress(fileTransferEntity.file_name, percent);

            //
            AppDatabase.getInstance().fileTransferDAO().update(fileTransferEntity);

            Data data = new Data.Builder()
                    .putString(DATA_TRANSFER_NAME_KEY, fileTransferEntity.file_name)
                    .putString(DATA_TRANSFER_KEY, fileTransferEntity.uid)
                    .putInt(DATA_PROGRESS_KEY, percent)
                    .putLong(DATA_TRANSFERRED_SIZE_KEY, transferredSize)
                    .putLong(DATA_TOTAL_SIZE_KEY, totalSize)
                    .build();
            setProgressAsync(data);
        }
    };

    private Call newCall;

    private void transferFile(Account account, FileTransferEntity transferEntity) throws IOException, SeafException, JSONException {
        SLogs.e("相册上传：" + transferEntity.full_path);
        notificationManager.updateProgress(transferEntity.file_name, 0);

        String uploadUrl = getUploadLink(account, transferEntity.repo_id, transferEntity.is_update);
        if (TextUtils.isEmpty(uploadUrl)) {
            SLogs.e("upload media: get upload link failed");
            return;
        }

        uploadFile(account, uploadUrl, transferEntity);
    }

    private String getUploadLink(Account account, String repoId, boolean isUpdate) throws IOException {
        retrofit2.Response<String> res = IO.getSingleton()
                .execute(RepoService.class)
                .getFileUploadLink(repoId, "/")
                .execute();

        if (!res.isSuccessful()) {
            return null;
        }

        String resStr = res.body();
        if (TextUtils.isEmpty(resStr)) {
            return null;
        }

        if (resStr.startsWith("\"")) {
            resStr = resStr.replace("\"", "");
        }
        return resStr;
    }

    private void uploadFile(Account account, String link, FileTransferEntity transferEntity) throws IOException, SeafException {
        //
        fileTransferProgressListener.setFileTransferEntity(transferEntity);

        //db
        transferEntity.transfer_status = TransferStatus.TRANSFER_IN_PROGRESS;
        AppDatabase.getInstance().fileTransferDAO().update(transferEntity);

        File file = new File(transferEntity.full_path);
        if (!file.exists()) {

            transferEntity.transfer_status = TransferStatus.TRANSFER_FAILED;
            transferEntity.transfer_result = TransferResult.FILE_NOT_FOUND;
            AppDatabase.getInstance().fileTransferDAO().update(transferEntity);

            throw SeafException.notFoundException;
        }

        MultipartBody.Builder builder = new MultipartBody.Builder();
        builder.setType(MultipartBody.FORM);

        String dir = transferEntity.target_path;
        if (transferEntity.is_update) {
            String targetFilePath = Utils.pathJoin(dir, file.getName());
            builder.addFormDataPart("target_file", targetFilePath);
        } else {
            //parent_dir: / is repo root
            builder.addFormDataPart("parent_dir", "/");

            dir = StringUtils.removeStart(dir, "/");
            builder.addFormDataPart("relative_path", dir);
        }

        ProgressRequestBody progressRequestBody = new ProgressRequestBody(file, fileTransferProgressListener);
        builder.addFormDataPart("file", file.getName(), progressRequestBody);

        RequestBody requestBody = builder.build();
        Request request = new Request.Builder()
                .url(link)
                .post(requestBody)
                .build();
        newCall = IO.getSingleton().getClient().newCall(request);

        Response response = newCall.execute();

        if (!response.isSuccessful()) {
            String b = response.body() != null ? response.body().string() : null;//[text={"error": "Out of quota.\n"}]
            SLogs.e("上传结果，失败：" + b);

            transferEntity.transfer_status = TransferStatus.TRANSFER_FAILED;

            if (!TextUtils.isEmpty(b) && "out of quota".contains(b.toLowerCase())) {

                transferEntity.transfer_result = TransferResult.QUOTA_EXCEEDED;
                AppDatabase.getInstance().fileTransferDAO().update(transferEntity);

                throw SeafException.OutOfQuota;
            }

            transferEntity.transfer_result = TransferResult.NETWORK_CONNECTION;
            AppDatabase.getInstance().fileTransferDAO().update(transferEntity);
            throw SeafException.networkException;
        }


        String str = response.body().string();
        String fileId = str.replace("\"", "");
        SLogs.e("上传照片结果，文件 ID：" + str);

        transferEntity.transferred_size = file.length();
        transferEntity.action_end_at = System.currentTimeMillis();
        transferEntity.modified_at = transferEntity.action_end_at;
        transferEntity.file_id = fileId;
        transferEntity.transfer_status = TransferStatus.TRANSFER_SUCCEEDED;
        AppDatabase.getInstance().fileTransferDAO().update(transferEntity);

    }
}
