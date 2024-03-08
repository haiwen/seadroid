package com.seafile.seadroid2.worker;

import android.content.Context;
import android.util.Pair;

import androidx.annotation.NonNull;
import androidx.work.Data;
import androidx.work.WorkerParameters;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.FileUtils;
import com.blankj.utilcode.util.ToastUtils;
import com.github.kevinsawicki.http.HttpRequest;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.data.db.AppDatabase;
import com.seafile.seadroid2.data.db.entities.FileTransferEntity;
import com.seafile.seadroid2.data.model.enums.TransferAction;
import com.seafile.seadroid2.data.model.enums.TransferResult;
import com.seafile.seadroid2.data.model.enums.TransferStatus;
import com.seafile.seadroid2.io.http.IO;
import com.seafile.seadroid2.listener.FileTransferProgressListener;
import com.seafile.seadroid2.notification.DownloadNotificationManager;
import com.seafile.seadroid2.ui.repo.RepoService;
import com.seafile.seadroid2.util.SLogs;

import org.json.JSONException;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.util.List;

import javax.net.ssl.SSLHandshakeException;

import okhttp3.Call;
import okhttp3.Request;
import okhttp3.Response;
import okhttp3.ResponseBody;

public class DownloadWorker extends TransferWorker {

    private final DownloadNotificationManager notificationManager;
    private final FileTransferProgressListener fileTransferProgressListener = new FileTransferProgressListener();

    public DownloadWorker(@NonNull Context context, @NonNull WorkerParameters workerParams) {
        super(context, workerParams);

        notificationManager = new DownloadNotificationManager(context);
        fileTransferProgressListener.setProgressListener(progressListener);
    }

    @NonNull
    @Override
    public Result doWork() {
        notificationManager.dismissNotification();

        Account account = getCurrentAccount();
        if (account == null) {
            return Result.success();
        }

        boolean isDownloaded = false;
        while (true) {
            List<FileTransferEntity> list = AppDatabase
                    .getInstance()
                    .fileTransferDAO()
                    .getListByAction(account.getSignature(), TransferAction.DOWNLOAD, TransferStatus.TRANSFER_WAITING);
            if (CollectionUtils.isEmpty(list)) {
                break;
            }

            isDownloaded = true;

            int fileCount = list.size();
            String tip = getApplicationContext().getResources().getQuantityString(R.plurals.transfer_download_started, fileCount, fileCount);
            ToastUtils.showLong(tip);

            for (FileTransferEntity fileTransferEntity : list) {
                if (isStopped()) {
                    break;
                }

                try {
                    transferFile(account, fileTransferEntity);
                } catch (IOException | SeafException | JSONException e) {
                    Result.failure();
                }
            }
        }

        //
        if (isDownloaded) {
            ToastUtils.showLong(R.string.download_finished);
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

    private Call newCall;

    private final FileTransferProgressListener.TransferProgressListener progressListener = new FileTransferProgressListener.TransferProgressListener() {
        @Override
        public void onProgressNotify(FileTransferEntity fileTransferEntity, int percent, long transferredSize, long totalSize) {
            SLogs.e(fileTransferEntity.file_name + " -> 下载进度：" + percent);
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

    private void transferFile(Account account, FileTransferEntity transferEntity) throws IOException, SeafException, JSONException {
        SLogs.e("开始下载：" + transferEntity.full_path);
        notificationManager.updateProgress(transferEntity.file_name, 0);

        if (transferEntity.is_block) {
            downloadFileByBlock(account, transferEntity);
        } else {
            downloadFile(account, transferEntity);
        }
    }

    private void downloadFile(Account account, FileTransferEntity transferEntity) throws SeafException {
        Pair<String, String> pair = getDownloadLink(transferEntity, false);
        String dlink = pair.first;
        String fileId = pair.second;

        File localFile = getLocalSaveDir(transferEntity);

        download(transferEntity, dlink, localFile);

        SLogs.e("下载结束：" + transferEntity.full_path);
    }

    private void downloadFileByBlock(Account account, FileTransferEntity transferEntity) {

    }

    public Pair<String, String> getDownloadLink(FileTransferEntity transferEntity, boolean isReUsed) throws SeafException {
        try {

            retrofit2.Response<String> res = IO.getSingleton()
                    .execute(RepoService.class)
                    .getFileDownloadLink(transferEntity.repo_id, transferEntity.full_path)
                    .execute();

            if (!res.isSuccessful()) {
                return null;
            }


            String downloadLink = res.body();
            String fileId = res.headers().get("oid");

            if (downloadLink.startsWith("\"")) {
                downloadLink = downloadLink.replace("\"", "");
            }

            // should return "\"http://gonggeng.org:8082/...\"" or "\"https://gonggeng.org:8082/...\"
            if (downloadLink.startsWith("http") && fileId != null) {
                return new Pair<>(downloadLink, fileId);
            } else {
                throw SeafException.illFormatException;
            }
        } catch (SeafException e) {
            throw e;
        } catch (UnsupportedEncodingException e) {
            throw SeafException.encodingException;
        } catch (IOException e) {
            throw SeafException.networkException;
        } catch (HttpRequest.HttpRequestException e) {
            throw getSeafExceptionFromHttpRequestException(e);
        }
    }

    private SeafException getSeafExceptionFromHttpRequestException(HttpRequest.HttpRequestException e) {
        if (e.getCause() instanceof SSLHandshakeException) {
            return SeafException.sslException;
        } else {
            return SeafException.networkException;
        }
    }


    private void download(FileTransferEntity fileTransferEntity, String dlink, File localFile) throws SeafException {
        if (dlink == null)
            return;

        fileTransferProgressListener.setFileTransferEntity(fileTransferEntity);

        fileTransferEntity.transfer_status = TransferStatus.TRANSFER_IN_PROGRESS;
        AppDatabase.getInstance().fileTransferDAO().update(fileTransferEntity);

        InputStream inputStream = null;
        FileOutputStream fileOutputStream = null;

        try {
            int i = dlink.lastIndexOf('/');
            String quoted = dlink.substring(0, i) + "/" + URLEncoder.encode(dlink.substring(i + 1), "UTF-8");

            Request request = getGetRequest(quoted);
            newCall = IO.getSingleton().getClient().newCall(request);

            Response response = newCall.execute();

            if (!response.isSuccessful()) {

                fileTransferEntity.transfer_status = TransferStatus.TRANSFER_FAILED;
                fileTransferEntity.transfer_result = TransferResult.NETWORK_CONNECTION;
                AppDatabase.getInstance().fileTransferDAO().update(fileTransferEntity);

                throw SeafException.networkException;
            }

            ResponseBody responseBody = response.body();
            if (responseBody == null) {

                fileTransferEntity.transfer_status = TransferStatus.TRANSFER_FAILED;
                fileTransferEntity.transfer_result = TransferResult.NETWORK_CONNECTION;
                AppDatabase.getInstance().fileTransferDAO().update(fileTransferEntity);

                throw SeafException.networkException;
            }

            inputStream = responseBody.byteStream();
            fileOutputStream = new FileOutputStream(localFile);

            long totalBytesRead = 0;
            long fileSize = responseBody.contentLength();

            int bytesRead;
            byte[] buffer = new byte[TransferWorker.SEGMENT_SIZE];
            while ((bytesRead = inputStream.read(buffer, 0, buffer.length)) != -1) {
                if (isStopped()) {
                    return;
                }

                fileOutputStream.write(buffer, 0, bytesRead);
                totalBytesRead += bytesRead;

                //notify Notification and update DB
                fileTransferProgressListener.onProgressNotify(totalBytesRead, fileSize);
            }

            //
            fileTransferEntity.transferred_size = fileSize;
            AppDatabase.getInstance().fileTransferDAO().update(fileTransferEntity);

            responseBody.close();

            if (localFile.length() != fileSize) {
                SLogs.e("Rename file error : " + localFile.getAbsolutePath());

                fileTransferEntity.transfer_status = TransferStatus.TRANSFER_FAILED;
                fileTransferEntity.transfer_result = TransferResult.FILE_ERROR;
                AppDatabase.getInstance().fileTransferDAO().update(fileTransferEntity);
                return;
            }

            fileTransferEntity.transfer_result = TransferResult.TRANSMITTED;
            fileTransferEntity.transfer_status = TransferStatus.TRANSFER_SUCCEEDED;
            fileTransferEntity.action_end_at = System.currentTimeMillis();
            fileTransferEntity.file_size = localFile.length();
            fileTransferEntity.file_md5 = FileUtils.getFileMD5ToString(fileTransferEntity.target_path).toLowerCase();

            AppDatabase.getInstance().fileTransferDAO().update(fileTransferEntity);

        } catch (SeafException e) {
            throw e;
        } catch (UnsupportedEncodingException e) {
            throw SeafException.encodingException;
        } catch (IOException e) {
            e.printStackTrace();
            throw SeafException.networkException;
        } catch (HttpRequest.HttpRequestException e) {
            throw getSeafExceptionFromHttpRequestException(e);
        } finally {
            try {
                if (fileOutputStream != null) {
                    fileOutputStream.close();
                }
                if (inputStream != null) {
                    inputStream.close();
                }

            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        }
    }
}
