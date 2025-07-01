package com.seafile.seadroid2.framework.worker.download;

import android.content.Context;
import android.os.Bundle;
import android.text.TextUtils;
import android.util.Pair;

import androidx.annotation.NonNull;
import androidx.work.ForegroundInfo;
import androidx.work.WorkerParameters;

import com.blankj.utilcode.util.CloneUtils;
import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.TimeUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.annotation.Todo;
import com.seafile.seadroid2.enums.FeatureDataSource;
import com.seafile.seadroid2.enums.SaveTo;
import com.seafile.seadroid2.enums.TransferResult;
import com.seafile.seadroid2.enums.TransferStatus;
import com.seafile.seadroid2.framework.crypto.SecurePasswordManager;
import com.seafile.seadroid2.framework.datastore.DataManager;
import com.seafile.seadroid2.framework.datastore.sp.SettingsManager;
import com.seafile.seadroid2.framework.db.AppDatabase;
import com.seafile.seadroid2.framework.db.entities.EncKeyCacheEntity;
import com.seafile.seadroid2.framework.db.entities.FileCacheStatusEntity;
import com.seafile.seadroid2.framework.http.HttpIO;
import com.seafile.seadroid2.framework.model.ResultModel;
import com.seafile.seadroid2.framework.notification.DownloadNotificationHelper;
import com.seafile.seadroid2.framework.notification.base.BaseNotification;
import com.seafile.seadroid2.framework.util.ExceptionUtils;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.util.Toasts;
import com.seafile.seadroid2.framework.worker.BackgroundJobManagerImpl;
import com.seafile.seadroid2.framework.worker.GlobalTransferCacheList;
import com.seafile.seadroid2.framework.worker.TransferEvent;
import com.seafile.seadroid2.framework.worker.TransferWorker;
import com.seafile.seadroid2.framework.worker.queue.TransferModel;
import com.seafile.seadroid2.listener.FileTransferProgressListener;
import com.seafile.seadroid2.ui.dialog_fragment.DialogService;
import com.seafile.seadroid2.ui.file.FileService;

import org.apache.commons.lang3.StringUtils;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URLEncoder;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import okhttp3.Call;
import okhttp3.OkHttpClient;
import okhttp3.Request;
import okhttp3.Response;
import okhttp3.ResponseBody;

/**
 * Worker Tag:
 *
 * @see BackgroundJobManagerImpl#TAG_ALL
 * @see BackgroundJobManagerImpl#TAG_TRANSFER
 */
public class DownloadWorker extends BaseDownloadWorker {
    private final String TAG = "DownloadWorker";

    private final DownloadNotificationHelper notificationHelper;
    private final FileTransferProgressListener transferProgressListener;
    private TransferModel currentTransferModel;

    @Override
    public BaseNotification getNotification() {
        return notificationHelper;
    }

    public DownloadWorker(@NonNull Context context, @NonNull WorkerParameters workerParams) {
        super(context, workerParams);

        notificationHelper = new DownloadNotificationHelper(context);

        transferProgressListener = new FileTransferProgressListener((transferModel, percent, transferredSize, totalSize) -> {
            SLogs.d(TAG, "onProgressNotify()", transferModel.file_name + " -> progress：" + percent);
            transferModel.transferred_size = transferredSize;
            GlobalTransferCacheList.updateTransferModel(transferModel);

            ForegroundInfo foregroundInfo = notificationHelper.getForegroundProgressNotification(transferModel.file_name, percent);
            showForegroundAsync(foregroundInfo);

            //
            sendProgressEvent(transferModel);
        });
    }

    @Override
    public void onStopped() {
        super.onStopped();

        if (notificationHelper != null) {
            notificationHelper.cancel();
        }

    }

    @NonNull
    @Override
    public Result doWork() {
        SLogs.d(TAG, "doWork()", "started execution");

        Account account = getCurrentAccount();
        if (account == null) {
            return returnSuccess();
        }

        //count
        int totalPendingCount = GlobalTransferCacheList.DOWNLOAD_QUEUE.getPendingCount();
        if (totalPendingCount <= 0) {
            SLogs.d(TAG, "doWork()", "download list is empty.");
            return returnSuccess();
        }

        ForegroundInfo foregroundInfo = notificationHelper.getForegroundNotification();
        showForegroundAsync(foregroundInfo);

        //tip
        String tip = getApplicationContext().getResources().getQuantityString(R.plurals.transfer_download_started, totalPendingCount, totalPendingCount);
        Toasts.show(tip);

        String interruptibleExceptionMsg = null;
        while (true) {
            if (isStopped()) {
                break;
            }

            TransferModel transferModel = GlobalTransferCacheList.DOWNLOAD_QUEUE.pick();
            if (transferModel == null) {
                break;
            }

            try {
                int p = GlobalTransferCacheList.DOWNLOAD_QUEUE.getPendingCount();
                SLogs.d(TAG, "doWork()", "pending count: " + p + ", download start：" + transferModel.full_path);
                currentTransferModel = CloneUtils.deepClone(transferModel, TransferModel.class);
                transferFile(account);

            } catch (Exception e) {

                if (notificationHelper != null) {
                    notificationHelper.cancel();
                }

                SeafException seafException = ExceptionUtils.parseByThrowable(e);
                interruptibleExceptionMsg = seafException.getMessage();
                notifyError(seafException);
                //
                break;
            }
        }

        SLogs.d(TAG, "doWork()", "all task complete");
        //
        if (TextUtils.isEmpty(interruptibleExceptionMsg)) {
            showToast(R.string.download_finished);
        }

        //
        Bundle b = new Bundle();
        b.putString(TransferWorker.KEY_DATA_RESULT, interruptibleExceptionMsg);
        b.putInt(TransferWorker.KEY_TRANSFER_COUNT, totalPendingCount);
        sendWorkerEvent(FeatureDataSource.DOWNLOAD, TransferEvent.EVENT_TRANSFER_TASK_COMPLETE, b);

        return Result.success();
    }

    protected Result returnSuccess() {
        sendFinishEvent();
        return Result.success();
    }

    protected void sendFinishEvent() {
        sendWorkerEvent(FeatureDataSource.DOWNLOAD, TransferEvent.EVENT_TRANSFER_TASK_COMPLETE);
    }

    private final int retryMaxCount = 1;

    private void transferFile(Account account) throws Exception {

        ForegroundInfo foregroundInfo = notificationHelper.getForegroundProgressNotification(currentTransferModel.file_name, 0);
        showForegroundAsync(foregroundInfo);

        try {
            downloadFile(account);

            sendProgressFinishEvent(currentTransferModel);

            SLogs.d(TAG, "transferFile()", "download complete：" + currentTransferModel.full_path);
        } catch (Exception e) {
            SLogs.d(TAG, "transferFile()", "download file failed -> " + currentTransferModel.full_path);
            SeafException seafException = ExceptionUtils.parseByThrowable(e);
            SLogs.e(seafException);
            checkInterrupt(account, seafException);
        }
    }

    @Todo("need to be refactored")
    private void checkInterrupt(Account account, SeafException seafException) throws Exception {
        if (isRetry(seafException)) {
            if (currentTransferModel.retry_times >= retryMaxCount) {
                //no interrupt
                updateToFailed(seafException.getMessage());
                return;
            }

            currentTransferModel.retry_times = currentTransferModel.retry_times + 1;
            if (seafException == SeafException.INVALID_PASSWORD) {
                boolean decryptResult = decryptRepo(currentTransferModel.repo_id);
                if (decryptResult) {
                    transferFile(account);
                } else {

                    //
                    // An error occurred while verifying the password with the local password,
                    // and the local password may be invalid/empty/incorrect.
                    // The user needs to re-enter the password again on the home page.
                    //
                    updateToFailed(seafException.getMessage());
                    //interrupt
                    throw seafException;
                }
            } else {
                transferFile(account);
            }
        } else if (isInterrupt(seafException)) {

            updateToFailed(seafException.getMessage());
            //interrupt
            throw seafException;
        } else {

            //no interrupt
            updateToFailed(seafException.getMessage());
        }
    }

    private void downloadFile(Account account) throws Exception {

        Pair<String, String> pair = getDownloadLink(false);
        String dlink = pair.first;
        String fileId = pair.second;

        download(account, dlink, fileId);
    }

    private Pair<String, String> getDownloadLink(boolean isReUsed) throws SeafException, IOException {
        retrofit2.Response<String> res = HttpIO.getCurrentInstance()
                .execute(FileService.class)
                .getFileDownloadLinkSync(currentTransferModel.repo_id, currentTransferModel.full_path, isReUsed ? 1 : 0)
                .execute();

        if (!res.isSuccessful()) {
            throw SeafException.REQUEST_URL_EXCEPTION;
        }

        String fileId = res.headers().get("oid");
        String dlink = res.body();
        if (dlink == null) {
            throw SeafException.REQUEST_URL_EXCEPTION;
        }

        dlink = StringUtils.replace(dlink, "\"", "");
        int i = dlink.lastIndexOf('/');
        if (i == -1) {
            // Handle invalid dlink appropriately
            return null;
        }

        dlink = dlink.substring(0, i) + "/" + URLEncoder.encode(dlink.substring(i + 1), "UTF-8");

        // should return "\"http://gonggeng.org:8082/...\"" or "\"https://gonggeng.org:8082/...\"
        if (dlink.startsWith("http") && fileId != null) {
            return new Pair<>(dlink, fileId);
        } else {
            throw SeafException.ILL_FORMAT_EXCEPTION;
        }
    }

    private OkHttpClient okHttpClient;

    private void download(Account account, String dlink, String fileId) throws Exception {
        transferProgressListener.setCurrentTransferModel(currentTransferModel);

        currentTransferModel.transfer_status = TransferStatus.IN_PROGRESS;
        GlobalTransferCacheList.updateTransferModel(currentTransferModel);

        Request request = new Request.Builder()
                .url(dlink)
                .get()
                .build();

        if (okHttpClient == null) {
            okHttpClient = HttpIO.getCurrentInstance().getSafeClient().getOkClient();
        }

        Call newCall = okHttpClient.newCall(request);

        try (Response response = newCall.execute()) {
            if (!response.isSuccessful()) {
                int code = response.code();
                String b = response.body() != null ? response.body().string() : null;
                SLogs.d(TAG, "download()", "download failed：" + code + ", resBody is : " + b);

                //
                newCall.cancel();

                throw ExceptionUtils.parseHttpException(code, b);
            }

            try (ResponseBody responseBody = response.body()) {
                if (responseBody == null) {
                    int code = response.code();
                    SLogs.d(TAG, "download()", "download failed：" + code + ", resBody is null ", currentTransferModel.target_path);

                    throw SeafException.NETWORK_EXCEPTION;
                }

                File localFile = DataManager.getLocalFileCachePath(account, currentTransferModel.repo_id, currentTransferModel.repo_name, currentTransferModel.full_path);


                long fileSize = responseBody.contentLength();
                if (fileSize == -1) {
                    SLogs.d(TAG, "download()", "download failed：contentLength is -1", localFile.getAbsolutePath());
                    fileSize = currentTransferModel.file_size;
                }

                //todo 检查剩余空间

                File tempFile = DataManager.createTempFile();
                try (InputStream inputStream = responseBody.byteStream();
                     FileOutputStream fileOutputStream = new FileOutputStream(tempFile)) {

                    long totalBytesRead = 0;

                    int bytesRead;
                    byte[] buffer = new byte[SEGMENT_SIZE];
                    while ((bytesRead = inputStream.read(buffer, 0, buffer.length)) != -1) {
                        if (isStopped()) {
                            throw SeafException.USER_CANCELLED_EXCEPTION;
                        }

                        fileOutputStream.write(buffer, 0, bytesRead);
                        totalBytesRead += bytesRead;

                        //notify Notification and update DB
                        transferProgressListener.onProgressNotify(totalBytesRead, fileSize);
                    }

                    //notify complete
                    transferProgressListener.onProgressNotify(fileSize, fileSize);
                }

                //important
                Path path = java.nio.file.Files.move(tempFile.toPath(), localFile.toPath(), java.nio.file.StandardCopyOption.REPLACE_EXISTING);
                boolean isSuccess = path.toFile().exists();
                if (isSuccess) {
                    java.nio.file.Files.deleteIfExists(tempFile.toPath());
                    updateToSuccess(fileId, localFile);
                }
            }
        }
    }

    @Todo("todo")
    public boolean isRetry(SeafException result) {
        if (result.equals(SeafException.INVALID_PASSWORD)) {
            return true;
        }
        return false;
    }

    public boolean isInterrupt(SeafException result) {
        if (result.equals(SeafException.INVALID_PASSWORD) ||
                result.equals(SeafException.SSL_EXCEPTION) ||
                result.equals(SeafException.UNAUTHORIZED_EXCEPTION) ||
                result.equals(SeafException.NOT_FOUND_USER_EXCEPTION) ||
                result.equals(SeafException.NOT_FOUND_DIR_EXCEPTION) ||
                result.equals(SeafException.USER_CANCELLED_EXCEPTION)) {
            return true;
        }
        return false;
    }

    private void updateToFailed(String transferResult) {
        currentTransferModel.transferred_size = 0L;
        currentTransferModel.transfer_status = TransferStatus.FAILED;
        currentTransferModel.err_msg = transferResult;
        GlobalTransferCacheList.updateTransferModel(currentTransferModel);
    }

    private void updateToSuccess(String fileId, File localFile) {
        currentTransferModel.transferred_size = currentTransferModel.file_size;
        currentTransferModel.transfer_status = TransferStatus.SUCCEEDED;
        currentTransferModel.err_msg = TransferResult.TRANSMITTED.name();
        GlobalTransferCacheList.updateTransferModel(currentTransferModel);

        if (currentTransferModel.save_to == SaveTo.DB) {
            FileCacheStatusEntity transferEntity = FileCacheStatusEntity.convertFromDownload(currentTransferModel, fileId);
            AppDatabase.getInstance().fileCacheStatusDAO().insert(transferEntity);
        }
    }

    public void notifyError(SeafException seafException) {
        if (seafException == SeafException.NETWORK_EXCEPTION) {
            getGeneralNotificationHelper().showErrorNotification(R.string.network_error, R.string.download);
        } else if (seafException == SeafException.NOT_FOUND_USER_EXCEPTION) {
            getGeneralNotificationHelper().showErrorNotification(R.string.saf_account_not_found_exception, R.string.download);
        } else if (seafException == SeafException.USER_CANCELLED_EXCEPTION) {
            //do nothing
        } else {
            getGeneralNotificationHelper().showErrorNotification(seafException.getMessage(), R.string.download);
        }
    }


    public boolean decryptRepo(String repoId) {
        List<EncKeyCacheEntity> encList = AppDatabase.getInstance().encKeyCacheDAO().getListByRepoIdSync(repoId);
        if (CollectionUtils.isEmpty(encList)) {
            return false;
        }

        EncKeyCacheEntity encKeyCacheEntity = encList.get(0);
        if (TextUtils.isEmpty(encKeyCacheEntity.enc_key) || TextUtils.isEmpty(encKeyCacheEntity.enc_iv)) {
            return false;
        }

        String decryptPassword = SecurePasswordManager.decryptPassword(encKeyCacheEntity.enc_key, encKeyCacheEntity.enc_iv);
        try {

            //
            setPassword(repoId, decryptPassword);

            //
            insert(repoId, decryptPassword);

            return true;
        } catch (IOException | SeafException e) {
            SLogs.e(e);
            return false;
        }
    }


    public void setPassword(String repoId, String password) throws IOException, SeafException {
        Map<String, String> requestDataMap = new HashMap<>();
        requestDataMap.put("password", password);

        retrofit2.Call<ResultModel> setPasswordCall = HttpIO.getCurrentInstance().execute(DialogService.class).setPasswordSync(repoId, requestDataMap);
        retrofit2.Response<ResultModel> res = setPasswordCall.execute();
        if (res.isSuccessful()) {
            ResultModel resultModel = res.body();
            SLogs.d(TAG, "setPassword()", "set password success");
        } else {
            int code = res.code();
            SLogs.d(TAG, "setPassword()", "set password failed: " + code);
            try (ResponseBody responseBody = res.errorBody()) {
                if (responseBody != null) {
                    throw ExceptionUtils.parseHttpException(code, responseBody.string());
                } else {
                    throw ExceptionUtils.parseHttpException(code, null);
                }
            }
        }
    }

    private void insert(String repoId, String password) {
        EncKeyCacheEntity encEntity = new EncKeyCacheEntity();
        encEntity.v = 2;
        encEntity.repo_id = repoId;

        Pair<String, String> p = SecurePasswordManager.encryptPassword(password);
        if (p != null) {
            encEntity.enc_key = p.first;
            encEntity.enc_iv = p.second;

            long expire = TimeUtils.getNowMills();
            expire += SettingsManager.DECRYPTION_EXPIRATION_TIME;
            encEntity.expire_time_long = expire;
            AppDatabase.getInstance().encKeyCacheDAO().insert(encEntity);
        }
    }

}
