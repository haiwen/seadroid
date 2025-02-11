package com.seafile.seadroid2.framework.worker.download;

import android.content.Context;
import android.text.TextUtils;
import android.util.Pair;

import androidx.annotation.NonNull;
import androidx.work.Data;
import androidx.work.ForegroundInfo;
import androidx.work.WorkerParameters;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.FileUtils;
import com.blankj.utilcode.util.ToastUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.framework.data.db.entities.DirentModel;
import com.seafile.seadroid2.enums.TransferDataSource;
import com.seafile.seadroid2.framework.datastore.DataManager;
import com.seafile.seadroid2.framework.data.db.AppDatabase;
import com.seafile.seadroid2.framework.data.db.entities.FileTransferEntity;
import com.seafile.seadroid2.enums.TransferResult;
import com.seafile.seadroid2.enums.TransferStatus;
import com.seafile.seadroid2.framework.http.HttpIO;
import com.seafile.seadroid2.framework.notification.base.BaseNotification;
import com.seafile.seadroid2.framework.util.ExceptionUtils;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.worker.BackgroundJobManagerImpl;
import com.seafile.seadroid2.framework.worker.ExistingFileStrategy;
import com.seafile.seadroid2.framework.worker.TransferEvent;
import com.seafile.seadroid2.framework.worker.TransferWorker;
import com.seafile.seadroid2.listener.FileTransferProgressListener;
import com.seafile.seadroid2.framework.notification.DownloadNotificationHelper;
import com.seafile.seadroid2.ui.file.FileService;

import org.apache.commons.lang3.StringUtils;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URLEncoder;
import java.nio.file.Path;
import java.util.List;
import java.util.UUID;

import okhttp3.Call;
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
    public static final UUID UID = UUID.nameUUIDFromBytes(DownloadWorker.class.getSimpleName().getBytes());

    private final DownloadNotificationHelper notificationHelper;
    private final FileTransferProgressListener fileTransferProgressListener = new FileTransferProgressListener();

    @Override
    public BaseNotification getNotification() {
        return notificationHelper;
    }

    public DownloadWorker(@NonNull Context context, @NonNull WorkerParameters workerParams) {
        super(context, workerParams);

        notificationHelper = new DownloadNotificationHelper(context);
        fileTransferProgressListener.setProgressListener(progressListener);
    }

    @Override
    public void onStopped() {
        super.onStopped();
    }

    @NonNull
    @Override
    public Result doWork() {

        Account account = getCurrentAccount();
        if (account == null) {
            return Result.success();
        }

        //count
        int totalPendingCount = AppDatabase.getInstance().fileTransferDAO().countPendingDownloadListSync(account.getSignature());
        if (totalPendingCount <= 0) {
            SLogs.i("download list is empty.");
            return Result.success(getFinishData(null));
        }

        ForegroundInfo foregroundInfo = notificationHelper.getForegroundNotification();
        showForegroundAsync(foregroundInfo);

        //tip
        String tip = getApplicationContext().getResources().getQuantityString(R.plurals.transfer_download_started, totalPendingCount, totalPendingCount);
        ToastUtils.showLong(tip);

        //start download

        String interruptibleExceptionMsg = null;
        boolean isFirst = true;

        while (true) {

            if (isStopped()) {
                break;
            }


            List<FileTransferEntity> transferList = getList(isFirst, account);
            if (isFirst) {
                isFirst = false;

                if (CollectionUtils.isEmpty(transferList)) {
                    continue;
                }
            } else if (CollectionUtils.isEmpty(transferList)) {
                break;
            }

            try {

                for (FileTransferEntity fileTransferEntity : transferList) {

                    try {
                        transferFile(account, fileTransferEntity, totalPendingCount);
                    } catch (Exception e) {
                        SeafException seafException = ExceptionUtils.getExceptionByThrowable(e);
                        //Is there an interruption in the transmission in some cases?
                        boolean isInterrupt = isInterrupt(seafException);
                        if (isInterrupt) {
                            SLogs.e("上传文件时发生了异常，已中断传输");
                            notifyError(seafException);

                            // notice this, see BaseUploadWorker#isInterrupt()
                            throw e;
                        } else {
                            SLogs.e("上传文件时发生了异常，继续下一个传输");
                        }
                    }
                }

            } catch (Exception e) {

                SLogs.e("upload file file failed: ", e);
                interruptibleExceptionMsg = e.getMessage();

                break;
            }
        }

        SLogs.i("all task run");

        //
        if (TextUtils.isEmpty(interruptibleExceptionMsg)) {
            ToastUtils.showLong(R.string.download_finished);
        }

        return Result.success(getFinishData(interruptibleExceptionMsg));
    }

    private Data getFinishData(String exceptionMsg) {
        return new Data.Builder()
                .putString(TransferWorker.KEY_DATA_SOURCE, TransferDataSource.DOWNLOAD.name())
                .putString(TransferWorker.KEY_DATA_STATUS, TransferEvent.EVENT_FINISH)
                .putString(TransferWorker.KEY_DATA_RESULT, exceptionMsg)
                .build();
    }

    private List<FileTransferEntity> getList(boolean isFirst, Account account) {
        List<FileTransferEntity> transferList;
        if (isFirst) {
            //get all: FAILED
            transferList = AppDatabase.getInstance()
                    .fileTransferDAO()
                    .getOnePendingFailedDownloadByAccountSync(
                            account.getSignature()
                    );
        } else {
            //get one: WAITING, IN_PROGRESS
            transferList = AppDatabase.getInstance()
                    .fileTransferDAO()
                    .getOnePendingDownloadByAccountSync(account.getSignature());
        }

        return transferList;
    }

    private final FileTransferProgressListener.TransferProgressListener progressListener = new FileTransferProgressListener.TransferProgressListener() {
        @Override
        public void onProgressNotify(FileTransferEntity fileTransferEntity, int percent, long transferredSize, long totalSize) {
            SLogs.i(fileTransferEntity.file_name + " -> progress：" + percent);

//            int diff = AppDatabase.getInstance().fileTransferDAO().countPendingDownloadListSync(fileTransferEntity.related_account);

            ForegroundInfo foregroundInfo = notificationHelper.getForegroundProgressNotification(fileTransferEntity.file_name, percent);
            showForegroundAsync(foregroundInfo);

            //
            AppDatabase.getInstance().fileTransferDAO().update(fileTransferEntity);

            //
            sendProgressNotifyEvent(fileTransferEntity.file_name, fileTransferEntity.uid, percent, transferredSize, totalSize, fileTransferEntity.data_source);
        }
    };

    private void transferFile(Account account, FileTransferEntity transferEntity, long totalPendingCount) throws Exception {
        SLogs.i("download start：" + transferEntity.full_path);

        //show notification
//        int diff = AppDatabase.getInstance().fileTransferDAO().countPendingDownloadListSync(transferEntity.related_account);
        ForegroundInfo foregroundInfo = notificationHelper.getForegroundProgressNotification(transferEntity.file_name, 0);
        showForegroundAsync(foregroundInfo);

        try {
            downloadFile(account, transferEntity);

            sendFinishEvent(account, transferEntity, totalPendingCount);

        } catch (Exception e) {
            SLogs.e("download file failed -> " + transferEntity.full_path);

            SeafException seafException = ExceptionUtils.getExceptionByThrowable(e);

            updateToFailed(transferEntity, seafException.getMessage());

            //send an event, update transfer entity first.
            sendFinishEvent(account, transferEntity, totalPendingCount);

            //Is there an interruption in the transmission in some cases?
            boolean isInterrupt = isInterrupt(seafException);
            if (isInterrupt) {
                SLogs.e("上传文件时发生了异常，已中断传输");
                notifyError(seafException);
                throw e;
            } else {

            }

        }
    }

    private void downloadFile(Account account, FileTransferEntity transferEntity) throws Exception {

        Pair<String, String> pair = getDownloadLink(transferEntity, false);
        String dlink = pair.first;
        String fileId = pair.second;

        //fileId = 0000000000000000000000000000000000000000

        File localFile = DataManager.getLocalRepoFile(account, transferEntity);

        if (localFile.exists() && transferEntity.file_strategy == ExistingFileStrategy.SKIP) {
            SLogs.i("skip this file, file_strategy is SKIP ：" + localFile.getAbsolutePath());
            return;
        }

        download(account, transferEntity, dlink, localFile);

        SLogs.i("download finish：" + transferEntity.full_path);
    }

    private Pair<String, String> getDownloadLink(FileTransferEntity transferEntity, boolean isReUsed) throws SeafException, IOException {
        retrofit2.Response<String> res = HttpIO.getCurrentInstance()
                .execute(FileService.class)
                .getFileDownloadLinkSync(transferEntity.repo_id, transferEntity.full_path, isReUsed ? 1 : 0)
                .execute();

        if (!res.isSuccessful()) {
            throw SeafException.REQUEST_TRANSFER_URL_EXCEPTION;
        }

        String fileId = res.headers().get("oid");
        String dlink = res.body();
        if (dlink == null) {
            throw SeafException.REQUEST_TRANSFER_URL_EXCEPTION;
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

    private void download(Account account, FileTransferEntity fileTransferEntity, String dlink, File localFile) throws Exception {
        fileTransferProgressListener.setFileTransferEntity(fileTransferEntity);

        fileTransferEntity.transfer_status = TransferStatus.IN_PROGRESS;
        AppDatabase.getInstance().fileTransferDAO().update(fileTransferEntity);

        Request request = new Request.Builder()
                .url(dlink)
                .get()
                .build();

        Call newCall = HttpIO.getCurrentInstance().getOkHttpClient().getOkClient().newCall(request);

        try (Response response = newCall.execute()) {
            if (!response.isSuccessful()) {
                int code = response.code();
                String b = response.body() != null ? response.body().string() : null;
                SLogs.d("upload failed：" + b);

                //
                newCall.cancel();

                throw ExceptionUtils.parseErrorJson(code, b);
            }

            ResponseBody responseBody = response.body();
            if (responseBody == null) {
                int code = response.code();
                throw ExceptionUtils.parseErrorJson(code, null);
            }

            long fileSize = responseBody.contentLength();
            if (fileSize == -1) {
                SLogs.e("download file error -> contentLength is -1, " + localFile.getAbsolutePath());
                fileSize = fileTransferEntity.file_size;
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
                    fileTransferProgressListener.onProgressNotify(totalBytesRead, fileSize);
                }

                //notify complete
                fileTransferProgressListener.onProgressNotify(fileSize, fileSize);
            }

            //important
//            tempFile.renameTo(localFile);
            Path path = java.nio.file.Files.move(tempFile.toPath(), localFile.toPath(), java.nio.file.StandardCopyOption.REPLACE_EXISTING);
            boolean isSuccess = path.toFile().exists();

            if (isSuccess) {
                updateToSuccess(fileTransferEntity, localFile);
            }
        }
    }

    public boolean isInterrupt(SeafException result) {
        if (result.equals(SeafException.INVALID_PASSWORD) ||
                result.equals(SeafException.SSL_EXCEPTION) ||
                result.equals(SeafException.NOT_FOUND_LOGGED_USER_EXCEPTION) ||
                result.equals(SeafException.NOT_FOUND_USER_EXCEPTION) ||
                result.equals(SeafException.NOT_FOUND_DIR_EXCEPTION) ||
                result.equals(SeafException.USER_CANCELLED_EXCEPTION)) {
            return true;
        }
        return false;
    }

    private void updateToFailed(FileTransferEntity fileTransferEntity, String transferResult) {
        fileTransferEntity.transfer_status = TransferStatus.FAILED;
        fileTransferEntity.result = transferResult;
        AppDatabase.getInstance().fileTransferDAO().update(fileTransferEntity);
    }

    private void updateToSuccess(FileTransferEntity fileTransferEntity, File localFile) {
        fileTransferEntity.transferred_size = localFile.length();
        fileTransferEntity.result = TransferResult.TRANSMITTED.name();
        fileTransferEntity.transfer_status = TransferStatus.SUCCEEDED;
        fileTransferEntity.action_end_at = System.currentTimeMillis();
        fileTransferEntity.file_original_modified_at = fileTransferEntity.action_end_at;//now
        fileTransferEntity.file_size = localFile.length();
        fileTransferEntity.file_md5 = FileUtils.getFileMD5ToString(fileTransferEntity.target_path).toLowerCase();

        AppDatabase.getInstance().fileTransferDAO().update(fileTransferEntity);

        //update
        List<DirentModel> direntList = AppDatabase.getInstance().direntDao().getListByFullPathSync(fileTransferEntity.repo_id, fileTransferEntity.full_path);
        if (!CollectionUtils.isEmpty(direntList)) {
            DirentModel direntModel = direntList.get(0);
            direntModel.last_modified_at = fileTransferEntity.modified_at;
            direntModel.id = fileTransferEntity.file_id;
            direntModel.size = fileTransferEntity.file_size;
            direntModel.transfer_status = fileTransferEntity.transfer_status;

            AppDatabase.getInstance().direntDao().insert(direntModel);
        }
    }

    public void notifyError(SeafException seafException) {
        if (seafException == SeafException.NETWORK_EXCEPTION) {
            getGeneralNotificationHelper().showErrorNotification(R.string.network_error, R.string.download);
        } else if (seafException == SeafException.NOT_FOUND_USER_EXCEPTION) {
            getGeneralNotificationHelper().showErrorNotification(R.string.saf_account_not_found_exception, R.string.download);
        } else {
            getGeneralNotificationHelper().showErrorNotification(seafException.getMessage(), R.string.download);
        }
    }
}
