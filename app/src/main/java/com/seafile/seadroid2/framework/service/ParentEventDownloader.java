package com.seafile.seadroid2.framework.service;

import android.content.Context;
import android.text.TextUtils;
import android.util.Pair;

import com.blankj.utilcode.util.CloneUtils;
import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.TimeUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
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
import com.seafile.seadroid2.framework.notification.TransferNotificationDispatcher;
import com.seafile.seadroid2.framework.util.ExceptionUtils;
import com.seafile.seadroid2.framework.util.SafeLogs;
import com.seafile.seadroid2.framework.worker.GlobalTransferCacheList;
import com.seafile.seadroid2.framework.worker.queue.TransferModel;
import com.seafile.seadroid2.listener.FileTransferProgressListener;
import com.seafile.seadroid2.ui.dialog_fragment.DialogService;
import com.seafile.seadroid2.ui.file.FileService;

import org.apache.commons.lang3.StringUtils;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
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

public abstract class ParentEventDownloader extends ParentEventTransfer {
    private final String TAG = "ParentEventDownloader";

    private final TransferNotificationDispatcher transferNotificationDispatcher;

    public ParentEventDownloader(Context context, TransferNotificationDispatcher transferNotificationDispatcher) {
        super(context);
        this.transferNotificationDispatcher = transferNotificationDispatcher;
    }

    public TransferNotificationDispatcher getTransferNotificationDispatcher() {
        return transferNotificationDispatcher;
    }

    public abstract FeatureDataSource getFeatureDataSource();

    private final FileTransferProgressListener transferProgressListener = new FileTransferProgressListener((transferModel, percent, transferredSize, totalSize) -> {
        SafeLogs.d(TAG, "onProgressNotify()", transferModel.file_name + " -> progress：" + percent);
        transferModel.transferred_size = transferredSize;
        GlobalTransferCacheList.updateTransferModel(transferModel);

        notifyProgress(transferModel.file_name, percent);

        //
        sendProgressEvent(getFeatureDataSource(), transferModel);
    });

    private void notifyProgress(String fileName, int percent) {
        if (getTransferNotificationDispatcher() == null) {
            return;
        }

        getTransferNotificationDispatcher().showProgress(getFeatureDataSource(), fileName, percent);
    }


    private final int retryMaxCount = 1;
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

            sendProgressCompleteEvent(getFeatureDataSource(), currentTransferModel);

            SafeLogs.d(TAG, "transferFile()", "download complete：" + currentTransferModel.full_path);
        } catch (Exception e) {
            SafeLogs.d(TAG, "transferFile()", "download file failed -> " + currentTransferModel.full_path);
            SeafException seafException = ExceptionUtils.parseByThrowable(e);
            SafeLogs.e(TAG, seafException.getMessage());
            checkInterrupt(account, seafException);
        }
    }

    private void checkInterrupt(Account account, SeafException seafException) throws SeafException {
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

    private void transferFile(Account account) throws SeafException {

        Pair<String, String> pair = getDownloadLink(false);
        String dlink = pair.first;
        String fileId = pair.second;

        download(account, dlink, fileId);
    }

    private Pair<String, String> getDownloadLink(boolean isReUsed) throws SeafException {
        retrofit2.Response<String> res;
        try {
            res = HttpIO.getCurrentInstance()
                    .execute(FileService.class)
                    .getFileDownloadLinkSync(currentTransferModel.repo_id, currentTransferModel.full_path, isReUsed ? 1 : 0)
                    .execute();

        } catch (IOException e) {
            SafeLogs.e(TAG, e.getMessage());
            throw SeafException.NETWORK_EXCEPTION;
        }

        if (!res.isSuccessful()) {
            throw SeafException.REQUEST_TRANSFER_URL_EXCEPTION;
        }

        String fileId = res.headers().get("oid");
        String dlink = res.body();
        if (TextUtils.isEmpty(dlink)) {
            throw SeafException.REQUEST_TRANSFER_URL_EXCEPTION;
        }


        dlink = StringUtils.replace(dlink, "\"", "");
        int i = dlink.lastIndexOf('/');
        if (i == -1) {
            // Handle invalid dlink appropriately
            return null;
        }

        try {
            dlink = dlink.substring(0, i) + "/" + URLEncoder.encode(dlink.substring(i + 1), "UTF-8");
        } catch (UnsupportedEncodingException e) {
            SafeLogs.e(TAG, e.getMessage());
            throw SeafException.REQUEST_TRANSFER_URL_EXCEPTION;
        }

        // should return "\"http://gonggeng.org:8082/...\"" or "\"https://gonggeng.org:8082/...\"
        if (dlink.startsWith("http") && fileId != null) {
            return new Pair<>(dlink, fileId);
        } else {
            throw SeafException.REQUEST_TRANSFER_URL_EXCEPTION;
        }
    }

    private void download(Account account, String dlink, String fileId) throws SeafException {
        transferProgressListener.setCurrentTransferModel(currentTransferModel);

        //
        sendProgressEvent(getFeatureDataSource(), currentTransferModel);

        //
        notifyProgress(currentTransferModel.file_name, 0);

        //update status
        currentTransferModel.transferred_size = 0;
        currentTransferModel.transfer_status = TransferStatus.IN_PROGRESS;
        GlobalTransferCacheList.updateTransferModel(currentTransferModel);

        SafeLogs.d(TAG, "download()", "download start：" + currentTransferModel.full_path);

        Request request = new Request.Builder()
                .url(dlink)
                .get()
                .build();

        //
        if (newCall != null && newCall.isExecuted()) {
            SafeLogs.d(TAG, "transferFile()", "newCall has executed(), cancel it");
            newCall.cancel();
        }

        if (okHttpClient == null) {
            okHttpClient = HttpIO.getInstanceByAccount(account).getOkHttpClient().getOkClient();
        }

        newCall = okHttpClient.newCall(request);

        try (Response response = newCall.execute()) {
            if (!response.isSuccessful()) {
                int code = response.code();
                String b = response.body() != null ? response.body().string() : null;
                SafeLogs.d(TAG, "download()", "download failed：" + code + ", resBody is : " + b);

                //
                newCall.cancel();

                throw ExceptionUtils.parse(code, b);
            }

            try (ResponseBody responseBody = response.body()) {
                if (responseBody == null) {
                    int code = response.code();
                    SafeLogs.d(TAG, "download()", "download failed：" + code + ", resBody is null ", currentTransferModel.target_path);

                    throw SeafException.NETWORK_EXCEPTION;
                }

                File localFile = DataManager.getLocalFileCachePath(account, currentTransferModel.repo_id, currentTransferModel.repo_name, currentTransferModel.full_path);

                long fileSize = responseBody.contentLength();
                if (fileSize == -1) {
                    SafeLogs.d(TAG, "download()", "download failed：contentLength is -1", localFile.getAbsolutePath());
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
                        if (isStop) {
                            SafeLogs.d(TAG, "download()", "download is stop, break");
                            return;
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
        } catch (IOException e) {
            SafeLogs.e(TAG, e.getMessage());
            throw SeafException.NETWORK_EXCEPTION;
        }
    }

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
            SafeLogs.e(TAG, e.getMessage());
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
            SafeLogs.d(TAG, "setPassword()", "set password success");
        } else {
            int code = res.code();
            SafeLogs.d(TAG, "setPassword()", "set password failed: " + code);
            try (ResponseBody responseBody = res.errorBody()) {
                if (responseBody != null) {
                    throw ExceptionUtils.parse(code, responseBody.string());
                } else {
                    throw ExceptionUtils.parse(code, null);
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
