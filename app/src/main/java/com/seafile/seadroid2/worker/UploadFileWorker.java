package com.seafile.seadroid2.worker;

import android.content.Context;
import android.content.pm.ServiceInfo;
import android.os.Build;
import android.text.TextUtils;

import androidx.annotation.NonNull;
import androidx.work.Data;
import androidx.work.ForegroundInfo;
import androidx.work.WorkerParameters;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.ToastUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.crypto.Crypto;
import com.seafile.seadroid2.data.Block;
import com.seafile.seadroid2.data.BlockInfoBean;
import com.seafile.seadroid2.data.FileBlocks;
import com.seafile.seadroid2.data.StorageManager;
import com.seafile.seadroid2.data.db.AppDatabase;
import com.seafile.seadroid2.data.model.dirents.DirentDirModel;
import com.seafile.seadroid2.data.model.dirents.DirentFileModel;
import com.seafile.seadroid2.data.db.entities.EncKeyCacheEntity;
import com.seafile.seadroid2.data.db.entities.FileTransferEntity;
import com.seafile.seadroid2.data.model.enums.TransferAction;
import com.seafile.seadroid2.data.model.enums.TransferFeature;
import com.seafile.seadroid2.data.model.enums.TransferResult;
import com.seafile.seadroid2.data.model.enums.TransferStatus;
import com.seafile.seadroid2.io.http.IO;
import com.seafile.seadroid2.listener.FileTransferProgressListener;
import com.seafile.seadroid2.notification.NotificationUtils;
import com.seafile.seadroid2.notification.UploadNotificationManager;
import com.seafile.seadroid2.ui.repo.RepoService;
import com.seafile.seadroid2.util.HttpUtils;
import com.seafile.seadroid2.util.SLogs;
import com.seafile.seadroid2.util.Utils;
import com.seafile.seadroid2.worker.body.ProgressRequestBody;

import org.apache.commons.lang3.StringUtils;
import org.json.JSONArray;
import org.json.JSONException;

import java.io.DataInputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Files;
import java.security.NoSuchAlgorithmException;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import okhttp3.Call;
import okhttp3.MultipartBody;
import okhttp3.Request;
import okhttp3.RequestBody;
import okhttp3.Response;

public class UploadFileWorker extends TransferWorker {
    private final UploadNotificationManager notificationManager;
    private final FileTransferProgressListener fileTransferProgressListener = new FileTransferProgressListener();

    public UploadFileWorker(@NonNull Context context, @NonNull WorkerParameters workerParams) {
        super(context, workerParams);

        notificationManager = new UploadNotificationManager(context);
        fileTransferProgressListener.setProgressListener(progressListener);
    }

    private ForegroundInfo createForegroundInfo() {
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.Q) {
            return new ForegroundInfo(
                    NotificationUtils.NOTIFICATION_UPLOAD_ID,
                    notificationManager.getNotification(),
                    ServiceInfo.FOREGROUND_SERVICE_TYPE_DATA_SYNC);
        } else {
            return new ForegroundInfo(
                    NotificationUtils.NOTIFICATION_UPLOAD_ID,
                    notificationManager.getNotification());
        }
    }

    @NonNull
    @Override
    public Result doWork() {
//        setForegroundAsync(createForegroundInfo());

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
                            TransferFeature.FOLDER_BACKUP);

            if (CollectionUtils.isEmpty(list)) {
                break;
//            return Result.success();
            }

            SLogs.d("start upload file worker");

            isUploaded = true;

            for (FileTransferEntity fileTransferEntity : list) {
                if (isStopped()) {
                    break;
                }

                try {
                    transferFile(account, fileTransferEntity);
                } catch (IOException | JSONException e) {
                    fileTransferEntity.transfer_status = TransferStatus.TRANSFER_FAILED;
                    fileTransferEntity.transfer_result = TransferResult.NETWORK_CONNECTION;
                    AppDatabase.getInstance().fileTransferDAO().update(fileTransferEntity);

                    throw new RuntimeException(e);
                } catch (SeafException seafException) {
                    if (seafException == SeafException.notFoundException) {
                        fileTransferEntity.transfer_status = TransferStatus.TRANSFER_FAILED;
                        fileTransferEntity.transfer_result = TransferResult.FILE_NOT_FOUND;
                        AppDatabase.getInstance().fileTransferDAO().update(fileTransferEntity);
                    }
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
        SLogs.e("开始上传：" + transferEntity.full_path);
        notificationManager.updateProgress(transferEntity.file_name, 0);

        if (transferEntity.is_block) {
            uploadBlockFile(account, transferEntity);
        } else {
            String uploadUrl = getUploadLink(account, transferEntity.repo_id, transferEntity.is_update);
            uploadFile(account, uploadUrl, transferEntity);
        }
    }

    private String getUploadLink(Account account, String repoId, boolean isUpdate) throws IOException {

        retrofit2.Response<String> res;

        if (isUpdate) {
            res = IO.getSingleton()
                    .execute(RepoService.class)
                    .getFileUpdateLink(repoId)
                    .execute();
        } else {
            res = IO.getSingleton()
                    .execute(RepoService.class)
                    .getFileUploadLink(repoId, "/")
                    .execute();
        }

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

    private DirentFileModel getRemoteFile(String repoId, String remotePath) throws IOException, SeafException {
        retrofit2.Response<DirentFileModel> fileDetailRes = IO.getSingleton()
                .execute(RepoService.class)
                .getFileDetailCall(repoId, remotePath)
                .execute();
        if (!fileDetailRes.isSuccessful()) {
            return null;
        }
        return fileDetailRes.body();
    }

    /**
     * @return true: The file in repo already exists and does not need to be uploaded
     */
    private ExistingFilePolicy checkRemoteFileExists(String repoId, String localPath, String remotePath) throws IOException, SeafException {


        //prepare to compare with remote file

        File file = new File(localPath);

        //check local file
        if (!file.exists()) {
            SLogs.e("local file not exists");
            throw SeafException.notFoundException;
        }

        DirentFileModel direntFileModel = getRemoteFile(repoId, remotePath);
        if (direntFileModel == null) {
            // nothing in remote
            return ExistingFilePolicy.APPEND;
        }

        if (direntFileModel.size == file.length()) {
            return ExistingFilePolicy.KEEP;
        }

        return ExistingFilePolicy.REPLACE_REMOTE_FILE;


        //FILE SYNC FEAT is not implemented in this version (v3.0.0).


//        long fm = file.lastModified();
//        long dm = direntFileModel.getMtimeInMills();
//
//        //text files need to be compared by file lastModified. because text files is editable.
//        if (fm > dm) {
//            //local file is newer than remote file
//
//            return ExistingFilePolicy.REPLACE_REMOTE_FILE;
//
//        } else if (fm == dm) {
//            //local file is same as remote file
//
//            SLogs.e("remote file is exists");
//            return ExistingFilePolicy.KEEP;
//
//        } else {
//            //
//            boolean setState = file.setLastModified(dm);
//            if (setState) {
//                SLogs.e("本地文件的修改时间已更新");
//            }
//
////            return ExistingFilePolicy.REPLACE_LOCAL_FILE;
//            return ExistingFilePolicy.REPLACE_REMOTE_FILE;
//        }
    }

    private boolean checkRemoteDirExists(String repoId, String dirPath) throws IOException {
        retrofit2.Response<DirentDirModel> response = IO.getSingleton()
                .execute(RepoService.class)
                .getDirDetailCall(repoId, dirPath)
                .execute();

        return response.isSuccessful();
    }

    private void mkdirRemote(String repoId, String path) throws IOException {
        Map<String, String> requestDataMap = new HashMap<>();
        requestDataMap.put("operation", "mkdir");
        requestDataMap.put("create_parents", "true");

        Map<String, RequestBody> requestBodyMap = HttpUtils.generateRequestBody(requestDataMap);

        retrofit2.Response<String> stringResponse = IO.getSingleton()
                .execute(RepoService.class)
                .mkDirCall(repoId, path, requestBodyMap)
                .execute();

        if (stringResponse.isSuccessful()) {
            SLogs.e("创建文件夹成功：" + path);
        } else {
            SLogs.e("创建文件夹失败：" + stringResponse.errorBody().string());
        }
    }

    //PEhPFNWIfINtWURm9BFIUDEnZD6Eqd103I4KtWLc0Sg
    private void uploadFile(Account account, String link, FileTransferEntity transferEntity) throws IOException, SeafException {

        File file = new File(transferEntity.full_path);
        if (!file.exists()) {
            throw SeafException.notFoundException;
        }

        ExistingFilePolicy policy = checkRemoteFileExists(transferEntity.repo_id, transferEntity.full_path, transferEntity.target_path + "/" + transferEntity.file_name);
        if (ExistingFilePolicy.KEEP == policy) {
            SLogs.e("upload file: skip file(remote exists): " + transferEntity.full_path);

            transferEntity.transfer_status = TransferStatus.TRANSFER_SUCCEEDED;
            transferEntity.transfer_result = TransferResult.TRANSMITTED;
            AppDatabase.getInstance().fileTransferDAO().update(transferEntity);

            return;
        }

        if (ExistingFilePolicy.REPLACE_REMOTE_FILE == policy) {
            transferEntity.is_update = true;
        } else if (ExistingFilePolicy.APPEND == policy) {
            transferEntity.is_update = false;
        }

        //
        fileTransferProgressListener.setFileTransferEntity(transferEntity);

        //db
        transferEntity.transfer_status = TransferStatus.TRANSFER_IN_PROGRESS;
        AppDatabase.getInstance().fileTransferDAO().update(transferEntity);

        MultipartBody.Builder builder = new MultipartBody.Builder();
        builder.setType(MultipartBody.FORM);

        String dir = transferEntity.target_path;
        if (transferEntity.is_update) {
            String targetFilePath = Utils.pathJoin(dir, file.getName());
            builder.addFormDataPart("target_file", targetFilePath);
        } else {
            //parent_dir: / is repo root
            builder.addFormDataPart("parent_dir", "/");

            //parent_dir is the root directory.
            //when select the root of the repo, relative_path is null.
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
            String b = response.body() != null ? response.body().string() : null;
            //[text={"error": "Out of quota.\n"}]
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
        SLogs.e("上传结果，文件 ID：" + str);

        transferEntity.transferred_size = file.length();
        transferEntity.action_end_at = System.currentTimeMillis();
        transferEntity.modified_at = transferEntity.action_end_at;
        transferEntity.file_id = fileId;
        transferEntity.transfer_status = TransferStatus.TRANSFER_SUCCEEDED;

        AppDatabase.getInstance().fileTransferDAO().update(transferEntity);
    }


    //block
    private BlockInfoBean getUploadBlockLink(Account account, String repoId, LinkedList<String> blkListId) throws IOException, JSONException {
        String ids = String.join(",", blkListId);

        Map<String, String> requestDataMap = new HashMap<>();
        requestDataMap.put("blklist", ids);

        Map<String, RequestBody> requestBodyMap = HttpUtils.generateRequestBody(requestDataMap);

        retrofit2.Response<BlockInfoBean> res = IO.getSingleton()
                .execute(RepoService.class)
                .getFileBlockUploadLink(repoId, requestBodyMap)
                .execute();

        if (!res.isSuccessful()) {
            return null;
        }
        return res.body();
    }


    private void uploadBlockFile(Account account, FileTransferEntity transferEntity) throws SeafException, IOException, JSONException {

        ExistingFilePolicy policy = checkRemoteFileExists(transferEntity.repo_id, transferEntity.full_path, transferEntity.target_path + "/" + transferEntity.file_name);
        if (ExistingFilePolicy.KEEP == policy) {
            SLogs.e("upload block file: skip file(remote exists): " + transferEntity.full_path);

            transferEntity.transfer_status = TransferStatus.TRANSFER_SUCCEEDED;
            transferEntity.transfer_result = TransferResult.TRANSMITTED;
            AppDatabase.getInstance().fileTransferDAO().update(transferEntity);

            return;
        }

        if (ExistingFilePolicy.REPLACE_REMOTE_FILE == policy) {
            transferEntity.is_update = true;
        } else if (ExistingFilePolicy.APPEND == policy) {
            transferEntity.is_update = false;
        }

        //notify
        fileTransferProgressListener.setFileTransferEntity(transferEntity);

        //update db
        transferEntity.transfer_status = TransferStatus.TRANSFER_IN_PROGRESS;
        AppDatabase.getInstance().fileTransferDAO().update(transferEntity);

        //
        List<EncKeyCacheEntity> encKeyCacheEntityList = AppDatabase.getInstance().encKeyCacheDAO().getByRepoIdSync(transferEntity.repo_id);
        if (CollectionUtils.isEmpty(encKeyCacheEntityList)) {
            return;
        }

        Optional<EncKeyCacheEntity> first = encKeyCacheEntityList.stream().findFirst();
        if (!first.isPresent()) {
            return;
        }

        EncKeyCacheEntity encKeyCacheEntity = first.get();
        final String encKey = encKeyCacheEntity.enc_key;
        final String encIv = encKeyCacheEntity.enc_iv;
        if (TextUtils.isEmpty(encKey) || TextUtils.isEmpty(encIv)) {
            throw SeafException.encryptException;
        }

        final FileBlocks chunkFile = chunkFile(encKey, encIv, transferEntity.full_path);
        if (chunkFile == null) {
            SLogs.e("chunkFile is null");
            return;
        }

        if (chunkFile.blocks.isEmpty()) {
            throw SeafException.blockListNullPointerException;
        }

        List<Block> blocks = chunkFile.blocks;
        LinkedList<String> blkListId = new LinkedList<>();
        for (Block block : blocks) {
            blkListId.addLast(block.getBlockId());
        }

        BlockInfoBean infoBean = getUploadBlockLink(account, transferEntity.repo_id, blkListId);
        if (infoBean == null) {
            SLogs.e("infoBean is null");
            return;
        }

        if (!CollectionUtils.isEmpty(infoBean.blklist)) {
            uploadBlocksCommon(infoBean.rawblksurl, infoBean.blklist, transferEntity.full_path, blocks);
        }

        commitUpload(infoBean.commiturl, blkListId, transferEntity);
    }


    /**
     * Upload file blocks to server
     */
    private String uploadBlocksCommon(String link, List<String> needUploadId, String filePath, List<Block> blocks) throws SeafException, IOException {

        File file = new File(filePath);
        if (!file.exists()) {
            throw SeafException.notFoundException;
        }

        MultipartBody.Builder builder = new MultipartBody.Builder();
        //set type
        builder.setType(MultipartBody.FORM);
        String fn = file.getName();
        builder.addFormDataPart("filename", fn);

        for (int i = 0; i < blocks.size(); i++) {
            Block block = blocks.get(i);
            for (String s : needUploadId) {
                if (s.equals(block.getBlockId())) {
                    File blk = new File(block.path);

                    ProgressRequestBody progressRequestBody = new ProgressRequestBody(blk, fileTransferProgressListener);
                    builder.addFormDataPart("file", blk.getName(), progressRequestBody);
                    break;
                }
            }
        }

        RequestBody body = builder.build();

        Request request = new Request.Builder().url(link)
                .post(body)
                .build();
        Response response = IO.getSingleton().getClient().newCall(request).execute();

        if (!response.isSuccessful()) {
            throw new SeafException(SeafException.OTHER_EXCEPTION, "File upload failed");
        }

        return response.body().string();
    }

    /**
     * commit blocks to server
     */
    private void commitUpload(String link, List<String> blkIds, FileTransferEntity fileTransferEntity)
            throws SeafException, IOException {
        File file = new File(fileTransferEntity.full_path);
        if (!file.exists()) {
            throw SeafException.notFoundException;
        }

        boolean isRemoteDirExists = checkRemoteDirExists(fileTransferEntity.repo_id, fileTransferEntity.target_path);
        if (!isRemoteDirExists) {
            mkdirRemote(fileTransferEntity.repo_id, fileTransferEntity.target_path);
        }

        MultipartBody.Builder builder = new MultipartBody.Builder();
        builder.setType(MultipartBody.FORM);

        //set header ,to replace file
        if (fileTransferEntity.is_update) {
            builder.addFormDataPart("replace", "1");
        } else {
            builder.addFormDataPart("replace", "0");
        }

        builder.addFormDataPart("parent_dir", fileTransferEntity.target_path);

        builder.addFormDataPart("file_size", String.valueOf(file.length()));
        builder.addFormDataPart("file_name", file.getName());

        JSONArray jsonArray = new JSONArray(blkIds);
        String js = jsonArray.toString();
        builder.addFormDataPart("blockids", js);

        RequestBody body = builder.build();

        Request request = new Request.Builder().url(link).post(body).build();
        try (Response response = IO.getSingleton().getClient().newCall(request).execute()) {
            if (response.isSuccessful()) {

                String fileId = response.body().string();

                //db
                fileTransferEntity.file_id = fileId;
                fileTransferEntity.transfer_status = TransferStatus.TRANSFER_SUCCEEDED;
                fileTransferEntity.action_end_at = System.currentTimeMillis();
                fileTransferEntity.modified_at = fileTransferEntity.action_end_at;
                fileTransferEntity.transfer_result = TransferResult.TRANSMITTED;
                AppDatabase.getInstance().fileTransferDAO().update(fileTransferEntity);
            }
        }

        fileTransferEntity.transfer_status = TransferStatus.TRANSFER_FAILED;
//        fileTransferEntity.transfer_result = TransferResult.TRANSMITTED;
        fileTransferEntity.action_end_at = System.currentTimeMillis();
        AppDatabase.getInstance().fileTransferDAO().update(fileTransferEntity);

        throw new SeafException(SeafException.OTHER_EXCEPTION, "File upload failed");
    }

    private final int BUFFER_SIZE = 2 * 1024 * 1024;

    private FileBlocks chunkFile(String encKey, String enkIv, String filePath) throws IOException {
        File file = new File(filePath);
        InputStream in = null;
        DataInputStream dis = null;
        OutputStream out = null;

        byte[] buffer = new byte[BUFFER_SIZE];
        FileBlocks seafBlock = new FileBlocks();

        try {
            in = Files.newInputStream(file.toPath());
            dis = new DataInputStream(in);

            // Log.d(DEBUG_TAG, "file size " + file.length());
            int byteRead;
            while ((byteRead = dis.read(buffer, 0, BUFFER_SIZE)) != -1) {
                byte[] cipher;
                if (byteRead < BUFFER_SIZE)
                    cipher = Crypto.encrypt(buffer, byteRead, encKey, enkIv);
                else
                    cipher = Crypto.encrypt(buffer, encKey, enkIv);

                final String blkid = Crypto.sha1(cipher);
                File blk = new File(StorageManager.getInstance().getTempDir(), blkid);
                Block block = new Block(blkid, blk.getAbsolutePath(), blk.length(), 0L);
                seafBlock.blocks.add(block);
                out = Files.newOutputStream(blk.toPath());
                out.write(cipher);
                out.close();
            }

            in.close();

            return seafBlock;
        } catch (NoSuchAlgorithmException | IOException e) {
            e.printStackTrace();
            return null;
        } finally {
            if (dis != null) dis.close();
            if (out != null) out.close();
            if (in != null) in.close();
        }
    }
}
