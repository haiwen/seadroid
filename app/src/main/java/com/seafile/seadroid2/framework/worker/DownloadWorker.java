package com.seafile.seadroid2.framework.worker;

import android.content.Context;
import android.text.TextUtils;
import android.util.Pair;

import androidx.annotation.NonNull;
import androidx.work.Data;
import androidx.work.WorkerParameters;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.FileUtils;
import com.blankj.utilcode.util.ToastUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.framework.crypto.Crypto;
import com.seafile.seadroid2.framework.data.Block;
import com.seafile.seadroid2.framework.data.db.entities.DirentModel;
import com.seafile.seadroid2.framework.data.db.entities.RepoModel;
import com.seafile.seadroid2.framework.datastore.DataManager;
import com.seafile.seadroid2.framework.data.FileBlocks;
import com.seafile.seadroid2.framework.datastore.StorageManager;
import com.seafile.seadroid2.framework.data.db.AppDatabase;
import com.seafile.seadroid2.framework.data.db.entities.EncKeyCacheEntity;
import com.seafile.seadroid2.framework.data.db.entities.FileTransferEntity;
import com.seafile.seadroid2.framework.data.model.enums.TransferResult;
import com.seafile.seadroid2.framework.data.model.enums.TransferStatus;
import com.seafile.seadroid2.framework.http.IO;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.listener.FileTransferProgressListener;
import com.seafile.seadroid2.framework.notification.DownloadNotificationHelper;
import com.seafile.seadroid2.ui.file.FileService;
import com.seafile.seadroid2.framework.worker.body.MonitoredFileOutputStream;

import org.apache.commons.lang3.StringUtils;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URLEncoder;
import java.util.ArrayList;
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
 * @see BackgroundJobManagerImpl#TAG_TRANSFER_DOWNLOAD
 * @see BackgroundJobManagerImpl#TAG_TRANSFER_DOWNLOAD_FILES_WORKER
 */
public class DownloadWorker extends BaseDownloadFileWorker {
    public static final UUID UID = UUID.randomUUID();

    private final DownloadNotificationHelper notificationManager;
    private final FileTransferProgressListener fileTransferProgressListener = new FileTransferProgressListener();

    public DownloadWorker(@NonNull Context context, @NonNull WorkerParameters workerParams) {
        super(context, workerParams);

        notificationManager = new DownloadNotificationHelper(context);
        fileTransferProgressListener.setProgressListener(progressListener);
    }

    @Override
    public void onStopped() {
        super.onStopped();

        notificationManager.cancel();
    }

    @NonNull
    @Override
    public Result doWork() {
        notificationManager.cancel();

        Account account = getCurrentAccount();
        if (account == null) {
            return Result.success();
        }
        notificationManager.showNotification();

        //
        boolean isDownloaded = false;
        while (true) {
            List<FileTransferEntity> list = AppDatabase
                    .getInstance()
                    .fileTransferDAO()
                    .getPendingDownloadListByActionSync(account.getSignature());
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
                } catch (Exception e) {
                    catchExceptionAndUpdateDB(fileTransferEntity, e);
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

        notificationManager.cancel();

        Data data = new Data.Builder()
                .putString(TransferWorker.KEY_DATA_EVENT, isDownloaded ? TransferEvent.EVENT_TRANSFERRED_WITH_DATA : TransferEvent.EVENT_NOT_TRANSFERRED)
                .build();
        return Result.success(data);
    }


    private final FileTransferProgressListener.TransferProgressListener progressListener = new FileTransferProgressListener.TransferProgressListener() {
        @Override
        public void onProgressNotify(FileTransferEntity fileTransferEntity, int percent, long transferredSize, long totalSize) {
            SLogs.d(fileTransferEntity.file_name + " -> progress：" + percent);
            notificationManager.notifyProgress(fileTransferEntity.file_name, percent);

            //
            AppDatabase.getInstance().fileTransferDAO().update(fileTransferEntity);

            //
            sendProgress(fileTransferEntity.file_name, fileTransferEntity.uid, percent, transferredSize, totalSize, fileTransferEntity.data_source);
        }
    };

    private void transferFile(Account account, FileTransferEntity transferEntity) throws Exception {
        SLogs.d("download start：" + transferEntity.full_path);
        notificationManager.notifyProgress(transferEntity.file_name, 0);

        List<RepoModel> repoModels = AppDatabase.getInstance().repoDao().getByIdSync(transferEntity.repo_id);

        if (CollectionUtils.isEmpty(repoModels)) {
            SLogs.d("no repo for repoId: " + transferEntity.repo_id);
            return;
        }

        if (repoModels.get(0).canLocalDecrypt()) {
            downloadFileByBlock(account, transferEntity);
        } else {
            downloadFile(account, transferEntity);
        }
    }

    private void downloadFile(Account account, FileTransferEntity transferEntity) throws Exception {

        Pair<String, String> pair = getDownloadLink(transferEntity, false);
        String dlink = pair.first;
        String fileId = pair.second;

        File localFile = DataManager.getLocalRepoFile(account, transferEntity);

        if (localFile.exists() && transferEntity.file_strategy == ExistingFileStrategy.SKIP) {
            SLogs.d("skip this file, file_strategy is SKIP ：" + localFile.getAbsolutePath());
            return;
        }

        download(transferEntity, dlink, localFile);

        SLogs.d("download finish：" + transferEntity.full_path);
    }

    private Pair<String, String> getDownloadLink(FileTransferEntity transferEntity, boolean isReUsed) throws SeafException, IOException {
        retrofit2.Response<String> res = IO.getInstanceWithLoggedIn()
                .execute(FileService.class)
                .getFileDownloadLink(transferEntity.repo_id, transferEntity.full_path)
                .execute();

        if (!res.isSuccessful()) {
            throw SeafException.networkException;
        }

        String fileId = res.headers().get("oid");
        String dlink = res.body();
        if (dlink == null) {
            throw SeafException.networkException;
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
            throw SeafException.illFormatException;
        }
    }

    private void download(FileTransferEntity fileTransferEntity, String dlink, File localFile) throws Exception {


        fileTransferProgressListener.setFileTransferEntity(fileTransferEntity);

        fileTransferEntity.transfer_status = TransferStatus.IN_PROGRESS;
        AppDatabase.getInstance().fileTransferDAO().update(fileTransferEntity);

        Request request = new Request.Builder()
                .url(dlink)
                .get()
                .build();

        Call newCall = IO.getInstanceWithLoggedIn().getClient().newCall(request);

        try (Response response = newCall.execute()) {
            if (!response.isSuccessful()) {
                throw SeafException.networkException;
            }

            ResponseBody responseBody = response.body();
            if (responseBody == null) {
                throw SeafException.networkException;
            }

            long fileSize = responseBody.contentLength();
            if (fileSize == -1) {
                SLogs.d("download file error -> contentLength is -1");
                SLogs.d(localFile.getAbsolutePath());

                fileSize = fileTransferEntity.file_size;

//                updateEntityErrorState(fileTransferEntity);
//                return;
            }

            File tempFile = DataManager.createTempFile();
            try (InputStream inputStream = responseBody.byteStream();
                 FileOutputStream fileOutputStream = new FileOutputStream(tempFile)) {

                long totalBytesRead = 0;

                int bytesRead;
                byte[] buffer = new byte[SEGMENT_SIZE];
                while ((bytesRead = inputStream.read(buffer, 0, buffer.length)) != -1) {
                    if (isStopped()) {
                        throw SeafException.userCancelledException;
                    }

                    fileOutputStream.write(buffer, 0, bytesRead);
                    totalBytesRead += bytesRead;

                    //notify Notification and update DB
                    fileTransferProgressListener.onProgressNotify(totalBytesRead, fileSize);
                }

                //notify complete
                fileTransferProgressListener.onProgressNotify(fileSize, fileSize);
            }

            tempFile.renameTo(localFile);

            if (localFile.length() != fileSize) {
                SLogs.d("download file error -> localFile.size != downloadedSize");
                SLogs.d(localFile.getAbsolutePath());
                updateEntityErrorState(fileTransferEntity);
            } else {
                updateEntitySuccessState(fileTransferEntity, localFile);
            }
        }
    }

    private void updateEntityErrorState(FileTransferEntity fileTransferEntity) {
        fileTransferEntity.transfer_status = TransferStatus.FAILED;
        fileTransferEntity.transfer_result = TransferResult.FILE_ERROR;
        AppDatabase.getInstance().fileTransferDAO().update(fileTransferEntity);
    }

    private void updateEntitySuccessState(FileTransferEntity fileTransferEntity, File localFile) {
        fileTransferEntity.transferred_size = localFile.length();
        fileTransferEntity.transfer_result = TransferResult.TRANSMITTED;
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

    ///////////////block///////////////
    private FileBlocks getDownloadBlockList(FileTransferEntity transferEntity) throws Exception {
        retrofit2.Response<FileBlocks> res = IO.getInstanceWithLoggedIn()
                .execute(FileService.class)
                .getFileBlockDownloadLink(transferEntity.repo_id, transferEntity.full_path)
                .execute();

        if (!res.isSuccessful()) {
            throw SeafException.networkException;
        }

        FileBlocks fileBlocks = res.body();
        if (fileBlocks == null) {
            throw SeafException.networkException;
        }

        return fileBlocks;
    }

    private void downloadFileByBlock(Account account, FileTransferEntity transferEntity) throws Exception {

        File localFile = DataManager.getLocalRepoFile(account, transferEntity);
        if (localFile.exists() && transferEntity.file_strategy == ExistingFileStrategy.SKIP) {
            SLogs.d("skip this file, file_strategy is SKIP ：" + localFile.getAbsolutePath());
            return;
        }

        FileBlocks fileBlocks = getDownloadBlockList(transferEntity);

        List<EncKeyCacheEntity> encKeyCacheEntityList = AppDatabase.getInstance().encKeyCacheDAO().getOneByRepoIdSync(transferEntity.repo_id);

        if (CollectionUtils.isEmpty(encKeyCacheEntityList)) {
            throw SeafException.decryptException;
        }
        EncKeyCacheEntity entity = encKeyCacheEntityList.get(0);

        final String encKey = entity.enc_key;
        final String encIv = entity.enc_iv;
        if (TextUtils.isEmpty(encKey) || TextUtils.isEmpty(encIv)) {
            throw SeafException.decryptException;
        }

        //TODO
//        if (CollectionUtils.isEmpty(fileBlocks.blocks)) {
//            if (!localFile.createNewFile()) {
//                SLogs.w( "Failed to create file " + localFile.getName());
//                return;
//            }
//
//            addCachedFile(repoName, repoID, path, fileBlocks.fileID, localFile);
//            return localFile;
//        }

        fileTransferProgressListener.setFileTransferEntity(transferEntity);

        List<File> tempFileList = new ArrayList<>();
        for (Block blk : fileBlocks.getBlocks()) {
            File tempBlock = new File(StorageManager.getInstance().getTempDir(), blk.blockId);

            retrofit2.Response<String> blockRes = IO.getInstanceWithLoggedIn()
                    .execute(FileService.class)
                    .getBlockDownloadLink(transferEntity.repo_id, fileBlocks.getFileId(), blk.blockId)
                    .execute();

            if (!blockRes.isSuccessful()) {
                throw SeafException.networkException;
            }

            String dlink = blockRes.body();
            dlink = StringUtils.replace(dlink, "\"", "");

            downloadBlock(fileBlocks, blk.blockId, dlink, tempBlock, transferEntity.file_size);

            final byte[] bytes = org.apache.commons.io.FileUtils.readFileToByteArray(tempBlock);
            final byte[] decryptedBlock = Crypto.decrypt(bytes, encKey, encIv);
            org.apache.commons.io.FileUtils.writeByteArrayToFile(localFile, decryptedBlock, true);

            tempFileList.add(tempBlock);
        }

        //remove cache file
        tempFileList.forEach(File::delete);

        //
        updateEntitySuccessState(transferEntity, localFile);
    }


    private void downloadBlock(FileBlocks fileBlocks, String blockId, String dlink, File localFile, long fileSize) throws Exception {
        InputStream inputStream = null;
        MonitoredFileOutputStream monitoredFileOutputStream = null;
        try {

            Request request = new Request.Builder()
                    .url(dlink)
                    .get()
                    .build();
            Call newCall = IO.getInstanceWithLoggedIn().getClient().newCall(request);

            Response response = newCall.execute();

            if (!response.isSuccessful()) {
                throw SeafException.networkException;
            }

            ResponseBody responseBody = response.body();
            if (responseBody == null) {
                throw SeafException.networkException;
            }

            long tempFileSize = responseBody.contentLength();

            inputStream = responseBody.byteStream();
            monitoredFileOutputStream = new MonitoredFileOutputStream(fileBlocks, blockId, localFile, fileSize, fileTransferProgressListener);


            int bytesRead;
            byte[] buffer = new byte[SEGMENT_SIZE];
            while ((bytesRead = inputStream.read(buffer, 0, buffer.length)) != -1) {
                if (isStopped()) {
                    throw SeafException.userCancelledException;
                }
                monitoredFileOutputStream.write(buffer, 0, bytesRead);
            }

            responseBody.close();

            if (localFile.length() != tempFileSize) {
                SLogs.d("Rename file error : " + localFile.getAbsolutePath());
                throw SeafException.networkException;
            }

        } finally {
            if (monitoredFileOutputStream != null) {
                monitoredFileOutputStream.close();
            }
            if (inputStream != null) {
                inputStream.close();
            }
        }
    }

}
