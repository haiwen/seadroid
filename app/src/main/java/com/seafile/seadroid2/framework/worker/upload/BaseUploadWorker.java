package com.seafile.seadroid2.framework.worker.upload;

import android.content.Context;
import android.text.TextUtils;

import androidx.annotation.NonNull;
import androidx.work.ForegroundInfo;
import androidx.work.WorkerParameters;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.FileUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.AccountInfo;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.framework.crypto.Crypto;
import com.seafile.seadroid2.framework.data.Block;
import com.seafile.seadroid2.framework.data.BlockInfoBean;
import com.seafile.seadroid2.framework.data.FileBlocks;
import com.seafile.seadroid2.framework.data.db.AppDatabase;
import com.seafile.seadroid2.framework.data.db.entities.DirentModel;
import com.seafile.seadroid2.framework.data.db.entities.EncKeyCacheEntity;
import com.seafile.seadroid2.framework.data.db.entities.FileTransferEntity;
import com.seafile.seadroid2.framework.data.db.entities.RepoModel;
import com.seafile.seadroid2.framework.data.model.dirents.DirentFileModel;
import com.seafile.seadroid2.framework.data.model.enums.TransferResult;
import com.seafile.seadroid2.framework.data.model.enums.TransferStatus;
import com.seafile.seadroid2.framework.datastore.StorageManager;
import com.seafile.seadroid2.framework.http.HttpIO;
import com.seafile.seadroid2.framework.notification.base.BaseTransferNotificationHelper;
import com.seafile.seadroid2.framework.util.HttpUtils;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.worker.ExistingFileStrategy;
import com.seafile.seadroid2.framework.worker.TransferEvent;
import com.seafile.seadroid2.framework.worker.TransferWorker;
import com.seafile.seadroid2.framework.worker.body.ProgressRequestBody;
import com.seafile.seadroid2.listener.FileTransferProgressListener;
import com.seafile.seadroid2.ui.account.AccountService;
import com.seafile.seadroid2.ui.file.FileService;

import org.apache.commons.lang3.StringUtils;
import org.json.JSONArray;
import org.json.JSONException;

import java.io.DataInputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.UnsupportedEncodingException;
import java.net.SocketTimeoutException;
import java.nio.file.Files;
import java.security.NoSuchAlgorithmException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.net.ssl.SSLHandshakeException;

import okhttp3.Call;
import okhttp3.MultipartBody;
import okhttp3.Request;
import okhttp3.RequestBody;
import okhttp3.Response;

public abstract class BaseUploadWorker extends TransferWorker {
    public abstract BaseTransferNotificationHelper getNotification();

    public BaseUploadWorker(@NonNull Context context, @NonNull WorkerParameters workerParams) {
        super(context, workerParams);

        fileTransferProgressListener.setProgressListener(progressListener);
    }


    private TransferResult parseTransferException(Exception e) {
        if (e instanceof JSONException) {
            return TransferResult.ENCODING_EXCEPTION;
        } else if (e instanceof SeafException) {
            if (e == SeafException.notFoundException) {
                return TransferResult.FILE_NOT_FOUND;
            } else if (e == SeafException.OUT_OF_QUOTA) {
                return TransferResult.OUT_OF_QUOTA;
            } else if (e == SeafException.networkException) {
                return TransferResult.NETWORK_CONNECTION;
            } else if (e == SeafException.sslException) {
                return TransferResult.SSL_EXCEPTION;
            } else if (e == SeafException.illFormatException) {
                return TransferResult.ENCODING_EXCEPTION;
            } else if (e == SeafException.notLoggedInException) {
                return TransferResult.ACCOUNT_NOT_LOGGED_IN;
            } else if (e == SeafException.notFoundUserException) {
                return TransferResult.ACCOUNT_NOT_FOUND;
            }
        } else if (e instanceof UnsupportedEncodingException) {
            return TransferResult.ENCODING_EXCEPTION;
        } else if (e instanceof SSLHandshakeException) {
            return TransferResult.SSL_EXCEPTION;
        } else if (e instanceof SocketTimeoutException) {
            return TransferResult.NETWORK_CONNECTION;
        } else if (e instanceof IOException) {
            return TransferResult.NETWORK_CONNECTION;
        }

        return TransferResult.UNKNOWN;
    }

    public TransferResult onException(FileTransferEntity transferEntity, Exception e) {
        transferEntity.transfer_status = TransferStatus.FAILED;
        transferEntity.action_end_at = System.currentTimeMillis();
        transferEntity.transfer_result = parseTransferException(e);

        //update db
        AppDatabase.getInstance().fileTransferDAO().update(transferEntity);

        return transferEntity.transfer_result;
    }


    public void notifyError(TransferResult result) {
        if (result == TransferResult.OUT_OF_QUOTA) {
            getGeneralNotificationHelper().showErrorNotification(R.string.above_quota, getNotification().getDefaultTitle());
        } else if (result == TransferResult.NETWORK_CONNECTION) {
            getGeneralNotificationHelper().showErrorNotification(R.string.network_error, getNotification().getDefaultTitle());
        } else if (result == TransferResult.ACCOUNT_NOT_FOUND) {
            getGeneralNotificationHelper().showErrorNotification(R.string.saf_account_not_found_exception, getNotification().getDefaultTitle());
        } else {
            getGeneralNotificationHelper().showErrorNotification(String.valueOf(result), getNotification().getDefaultTitle());
        }
    }

    public String isInterrupt(TransferResult result) {
        String finishFlagEvent = null;
        if (result == TransferResult.ENCODING_EXCEPTION) {
            finishFlagEvent = TransferEvent.EVENT_FINISH;
        } else if (result == TransferResult.FILE_NOT_FOUND) {
//            finishFlagEvent = null;
        } else if (result == TransferResult.OUT_OF_QUOTA) {
            finishFlagEvent = TransferEvent.EVENT_CANCEL_OUT_OF_QUOTA;
        } else if (result == TransferResult.NETWORK_CONNECTION) {
            finishFlagEvent = TransferEvent.EVENT_CANCEL_WITH_NETWORK_ERR;
        } else if (result == TransferResult.SSL_EXCEPTION) {
            finishFlagEvent = TransferEvent.EVENT_FINISH;
        } else if (result == TransferResult.ACCOUNT_NOT_LOGGED_IN) {
            finishFlagEvent = TransferEvent.EVENT_FINISH;
        } else if (result == TransferResult.ACCOUNT_NOT_FOUND) {
            finishFlagEvent = TransferEvent.EVENT_FINISH;
        } else if (result == TransferResult.UNKNOWN) {
            finishFlagEvent = TransferEvent.EVENT_FINISH;
        }

        return finishFlagEvent;
    }


    protected boolean calcQuota(List<FileTransferEntity> list) throws SeafException, IOException {

        //
        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        if (account != null && account.isQuotaUnlimited()) {
            return true;
        }

        AccountInfo accountInfo = getAccountInfo();
        long remain = accountInfo.getTotal() - accountInfo.getUsage();

        long localTotalSize = 0;
        for (FileTransferEntity transferEntity : list) {
            long size = FileUtils.getFileLength(transferEntity.full_path);
            localTotalSize += size;
            if (localTotalSize > remain) {
                return false;
            }
        }

        return remain > localTotalSize;
    }

    protected AccountInfo getAccountInfo() throws IOException, SeafException {
        retrofit2.Response<AccountInfo> response = HttpIO.getCurrentInstance()
                .execute(AccountService.class)
                .getAccountInfoCall()
                .execute();

        if (!response.isSuccessful()) {
            throw SeafException.networkException;
        }

        AccountInfo accountInfo = response.body();
        if (accountInfo == null) {
            //do nothing, continue work
            throw SeafException.networkException;
        }

        return accountInfo;
    }

    /**
     *
     */
    private final Set<String> repoNetDataIsFetched = new HashSet<>();

    private ExistingFileStrategy compareLocal(File file, FileTransferEntity transferEntity) {
        List<DirentModel> entList = AppDatabase.getInstance().direntDao().getListByFullPathSync(transferEntity.repo_id, transferEntity.target_path);
        if (CollectionUtils.isEmpty(entList)) {
            return ExistingFileStrategy.NOT_FOUND_IN_REMOTE;
        }

        if (TextUtils.equals(transferEntity.file_id, entList.get(0).id)) {
            return ExistingFileStrategy.SKIP;
        }

        // improve
        if (entList.get(0).size == file.length()) {
            return ExistingFileStrategy.SKIP;
        }

        return ExistingFileStrategy.APPEND;
    }

    /**
     * @return true: The file in repo already exists and does not need to be uploaded
     */
    protected ExistingFileStrategy checkRemoteFileExists(Account account, RepoModel repoModel, FileTransferEntity transferEntity) throws IOException, SeafException {

        if (ExistingFileStrategy.REPLACE == transferEntity.file_strategy) {
            return ExistingFileStrategy.REPLACE;
        }

        String repoId = transferEntity.repo_id;
        String localPath = transferEntity.full_path;
        String remotePath = transferEntity.target_path;

        //prepare to compare with remote file

        File file = new File(localPath);

        //check local file
        if (!file.exists()) {
            SLogs.d("local file not exists: " + localPath);
            throw SeafException.notFoundException;
        }

        List<DirentModel> entList = AppDatabase.getInstance().direntDao().getListByFullPathSync(repoId, remotePath);
        if (CollectionUtils.isEmpty(entList)) {
            if (repoNetDataIsFetched.contains(repoId)) {
                return ExistingFileStrategy.NOT_FOUND_IN_REMOTE;
            }

            //load data from net
            try {
                boolean isSuccess = getRemoteDirentList(account, repoModel, transferEntity.getParent_path());
                if (isSuccess) {
                    repoNetDataIsFetched.add(repoId);

                    return compareLocal(file, transferEntity);
                }

            } catch (SeafException e) {

                //Second attempt
                DirentFileModel direntFileModel = getRemoteFile(repoId, remotePath);
                if (direntFileModel == null) {
                    // nothing in remote
                    return ExistingFileStrategy.NOT_FOUND_IN_REMOTE;
                }

                if (TextUtils.equals(transferEntity.file_id, direntFileModel.id)) {
                    return ExistingFileStrategy.SKIP;
                }

                // improve
                if (direntFileModel.size == file.length()) {
                    return ExistingFileStrategy.SKIP;
                }
            }

            return ExistingFileStrategy.APPEND;
        } else {
            return compareLocal(file, transferEntity);
        }

//
//        DirentFileModel direntFileModel = getRemoteFile(repoId, remotePath);
//        if (direntFileModel == null) {
//            // nothing in remote
//            return ExistingFileStrategy.NOT_FOUND_IN_REMOTE;
//        }
//
//        if (TextUtils.equals(transferEntity.file_id, direntFileModel.id)) {
//            return ExistingFileStrategy.SKIP;
//        }
//
//        // improve
//        if (direntFileModel.size == file.length()) {
//            return ExistingFileStrategy.SKIP;
//        }

//        return ExistingFileStrategy.REPLACE;


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
//            SLogs.d("remote file is exists");
//            return ExistingFilePolicy.KEEP;
//
//        } else {
//            //
//            boolean setState = file.setLastModified(dm);
//            if (setState) {
//                SLogs.d("本地文件的修改时间已更新");
//            }
//
////            return ExistingFilePolicy.REPLACE_LOCAL_FILE;
//            return ExistingFilePolicy.REPLACE_REMOTE_FILE;
//        }
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

        if (newCall != null && !newCall.isCanceled()) {
            newCall.cancel();
        }
    }

    private void notifyProgress(String fileName, int percent) {
        if (getNotification() == null) {
            return;
        }

        BaseTransferNotificationHelper notification = getNotification();
        ForegroundInfo f = notification.getForegroundProgressNotification(fileName, percent);
        showForegroundAsync(f);
    }

    public void transferFile(Account account, FileTransferEntity transferEntity) throws IOException, SeafException, JSONException {
        SLogs.d("start transfer, full_path: " + transferEntity.full_path);
        SLogs.d("start transfer, target_path: " + transferEntity.target_path);
        notifyProgress(transferEntity.file_name, 0);

        List<RepoModel> repoModels = AppDatabase.getInstance().repoDao().getByIdSync(transferEntity.repo_id);

        if (CollectionUtils.isEmpty(repoModels)) {
            SLogs.d("no repo for repoId: " + transferEntity.repo_id);
            return;
        }

        //update modified_at field
        transferEntity.modified_at = System.currentTimeMillis();
        AppDatabase.getInstance().fileTransferDAO().update(transferEntity);

        RepoModel repo = repoModels.get(0);
        if (repo.canLocalDecrypt()) {
            uploadBlockFile(account, repo, transferEntity);
        } else {
            uploadFile(account, repo, transferEntity);
        }
    }

    /**
     * upload file
     */
    private void uploadFile(Account account, RepoModel repoModel, FileTransferEntity transferEntity) throws IOException, SeafException {
        if (isStopped()) {
            return;
        }

        File file = new File(transferEntity.full_path);
        if (!file.exists()) {
            throw SeafException.notFoundException;
        }

        ExistingFileStrategy fileStrategy = transferEntity.file_strategy;
        if (fileStrategy == ExistingFileStrategy.AUTO) {
            fileStrategy = checkRemoteFileExists(account, repoModel, transferEntity);
        }

        if (fileStrategy == ExistingFileStrategy.SKIP) {
            SLogs.d("folder backup: skip file(remote exists): " + transferEntity.target_path);

            transferEntity.transfer_status = TransferStatus.SUCCEEDED;
            transferEntity.transfer_result = TransferResult.TRANSMITTED;
            AppDatabase.getInstance().fileTransferDAO().update(transferEntity);

            return;
        }

        //net
        MultipartBody.Builder builder = new MultipartBody.Builder();
        builder.setType(MultipartBody.FORM);

        if (transferEntity.file_strategy == ExistingFileStrategy.REPLACE) {
            builder.addFormDataPart("target_file", transferEntity.target_path);
        } else {
            //parent_dir: / is repo root
            builder.addFormDataPart("parent_dir", "/");
//            builder.addFormDataPart("parent_dir", transferEntity.getParent_path());

//            parent_dir is the root directory.
//            when select the root of the repo, relative_path is null.
            String dir = transferEntity.getParent_path();
            dir = StringUtils.removeStart(dir, "/");
//
            builder.addFormDataPart("relative_path", dir);
        }


        //
        fileTransferProgressListener.setFileTransferEntity(transferEntity);

        //db
        transferEntity.transfer_status = TransferStatus.IN_PROGRESS;
        AppDatabase.getInstance().fileTransferDAO().update(transferEntity);


        ProgressRequestBody progressRequestBody = new ProgressRequestBody(file, fileTransferProgressListener);
        builder.addFormDataPart("file", file.getName(), progressRequestBody);

        RequestBody requestBody = builder.build();

        //get upload link
        String uploadUrl = getFileUploadUrl(transferEntity.repo_id, transferEntity.getParent_path(), transferEntity.file_strategy == ExistingFileStrategy.REPLACE);
        if (TextUtils.isEmpty(uploadUrl)) {
            throw SeafException.networkException;
        }

        //
        if (newCall != null && newCall.isExecuted()) {
            SLogs.d("Folder upload: newCall has executed()");
        }

        try {

            Request request = new Request.Builder()
                    .url(uploadUrl)
                    .post(requestBody)
                    .build();
            newCall = HttpIO.getCurrentInstance().getOkHttpClient().getOkClient().newCall(request);

            Response response = newCall.execute();

            if (!response.isSuccessful()) {
                String b = response.body() != null ? response.body().string() : null;
                SLogs.d("result，failed：" + b);

                //
                newCall.cancel();

                //[text={"error": "Out of quota.\n"}]
                if (b != null && b.toLowerCase().contains("out of quota")) {
                    throw SeafException.OUT_OF_QUOTA;
                }

                throw SeafException.networkException;
            }

            String str = response.body().string();
            String fileId = str.replace("\"", "");
            SLogs.d("result，file ID：" + str);

            updateSuccess(transferEntity, fileId, file);
        } catch (Exception e) {
            throw e;
        }
    }


    private void uploadBlockFile(Account account, RepoModel repoModel, FileTransferEntity transferEntity) throws SeafException, IOException, JSONException {
        if (isStopped()) {
            return;
        }

        File file = new File(transferEntity.full_path);
        if (!file.exists()) {
            throw SeafException.notFoundException;
        }

        ExistingFileStrategy policy = checkRemoteFileExists(account, repoModel, transferEntity);
        if (ExistingFileStrategy.SKIP == policy) {
            SLogs.d("skip block file(remote exists): " + transferEntity.target_path);

            transferEntity.transfer_status = TransferStatus.SUCCEEDED;
            transferEntity.transfer_result = TransferResult.TRANSMITTED;
            AppDatabase.getInstance().fileTransferDAO().update(transferEntity);

            return;
        }

        //notify
        fileTransferProgressListener.setFileTransferEntity(transferEntity);

        //update db
        transferEntity.transfer_status = TransferStatus.IN_PROGRESS;
        AppDatabase.getInstance().fileTransferDAO().update(transferEntity);

        //
        List<EncKeyCacheEntity> encKeyCacheEntityList = AppDatabase.getInstance().encKeyCacheDAO().getOneByRepoIdSync(transferEntity.repo_id);
        if (CollectionUtils.isEmpty(encKeyCacheEntityList)) {
            throw SeafException.encryptException;
        }

        EncKeyCacheEntity encKeyCacheEntity = encKeyCacheEntityList.get(0);
        final String encKey = encKeyCacheEntity.enc_key;
        final String encIv = encKeyCacheEntity.enc_iv;
        if (TextUtils.isEmpty(encKey) || TextUtils.isEmpty(encIv)) {
            throw SeafException.encryptException;
        }

        final FileBlocks chunkFile = chunkFile(encKey, encIv, transferEntity.full_path);
        if (chunkFile == null) {
            SLogs.d("chunkFile is null");
            return;
        }

        if (chunkFile.getBlocks().isEmpty()) {
            throw SeafException.blockListNullPointerException;
        }

        List<Block> blocks = chunkFile.getBlocks();
        LinkedList<String> blkListId = new LinkedList<>();
        for (Block block : blocks) {
            blkListId.addLast(block.getBlockId());
        }

        BlockInfoBean infoBean = getFileBlockUploadUrl(account, transferEntity.repo_id, blkListId);
        if (infoBean == null) {
            SLogs.d("infoBean is null");
            return;
        }

        if (!CollectionUtils.isEmpty(infoBean.blklist)) {
            uploadBlocksCommon(infoBean.rawblksurl, infoBean.blklist, transferEntity.full_path, blocks);
        }

        commitUpload(infoBean.commiturl, blkListId, transferEntity);
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
            throw new SeafException(res.code(), res.message());
        }

        String urlStr = res.body();
        urlStr = StringUtils.replace(urlStr, "\"", "");

        return urlStr;
    }

    //block
    private BlockInfoBean getFileBlockUploadUrl(Account account, String repoId, LinkedList<String> blkListId) throws IOException, JSONException {
        String ids = String.join(",", blkListId);

        Map<String, String> requestDataMap = new HashMap<>();
        requestDataMap.put("blklist", ids);

        Map<String, RequestBody> requestBodyMap = HttpUtils.generateRequestBody(requestDataMap);

        retrofit2.Response<BlockInfoBean> res = HttpIO.getCurrentInstance()
                .execute(FileService.class)
                .getFileBlockUploadLink(repoId, requestBodyMap)
                .execute();

        if (!res.isSuccessful()) {
            return null;
        }

        return res.body();
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
        Response response = HttpIO.getCurrentInstance().getOkHttpClient().getOkClient().newCall(request).execute();

        if (!response.isSuccessful()) {
            throw SeafException.networkException;
        }

        return response.body().string();
    }

    /**
     * commit blocks to server
     */
    private void commitUpload(String link, List<String> blkIds, FileTransferEntity transferEntity) throws SeafException, IOException {

        File file = new File(transferEntity.full_path);
        if (!file.exists()) {
            throw SeafException.notFoundException;
        }

        boolean isRemoteDirExists = checkRemoteDirExists(transferEntity.repo_id, transferEntity.getParent_path());
        if (!isRemoteDirExists) {
            mkdirRemote(transferEntity.repo_id, transferEntity.getParent_path());
        }

        MultipartBody.Builder builder = new MultipartBody.Builder();
        builder.setType(MultipartBody.FORM);

        //set header ,to replace file
        if (transferEntity.file_strategy == ExistingFileStrategy.REPLACE) {
            builder.addFormDataPart("replace", "1");
        } else {
            builder.addFormDataPart("replace", "0");
        }

        builder.addFormDataPart("parent_dir", transferEntity.getParent_path());

        builder.addFormDataPart("file_size", String.valueOf(file.length()));
        builder.addFormDataPart("file_name", file.getName());

        JSONArray jsonArray = new JSONArray(blkIds);
        String js = jsonArray.toString();
        builder.addFormDataPart("blockids", js);

        RequestBody body = builder.build();

        Request request = new Request.Builder().url(link).post(body).build();

        //
        if (newCall != null && newCall.isExecuted()) {
            SLogs.d("Folder upload block: newCall has executed()");
        }
        try {
            newCall = HttpIO.getCurrentInstance().getOkHttpClient().getOkClient().newCall(request);
            Response response = newCall.execute();

            if (!response.isSuccessful()) {
                String b = response.body() != null ? response.body().string() : null;
                SLogs.d("上传结果，失败：" + b);

                //[text={"error": "Out of quota.\n"}]
                if (b != null && b.toLowerCase().contains("out of quota")) {
                    throw SeafException.OUT_OF_QUOTA;
                }

                throw SeafException.networkException;
            }

            String fileId = response.body().string();

            updateSuccess(transferEntity, fileId, file);
        } catch (Exception e) {
            throw e;
        }
    }

    private void updateSuccess(FileTransferEntity transferEntity, String fileId, File file) {
        //db
        transferEntity.file_id = fileId;
        transferEntity.transferred_size = file.length();
        transferEntity.action_end_at = System.currentTimeMillis();
        transferEntity.modified_at = transferEntity.action_end_at;
        transferEntity.file_original_modified_at = file.lastModified();
        transferEntity.transfer_result = TransferResult.TRANSMITTED;
        transferEntity.transfer_status = TransferStatus.SUCCEEDED;

        AppDatabase.getInstance().fileTransferDAO().update(transferEntity);

        //update
        List<DirentModel> direntList = AppDatabase.getInstance().direntDao().getListByFullPathSync(transferEntity.repo_id, transferEntity.full_path);
        if (!CollectionUtils.isEmpty(direntList)) {
            DirentModel direntModel = direntList.get(0);
            direntModel.last_modified_at = transferEntity.modified_at;
            direntModel.id = fileId;
            direntModel.size = transferEntity.file_size;
            direntModel.transfer_status = transferEntity.transfer_status;

            AppDatabase.getInstance().direntDao().update(direntModel);
        }


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
                seafBlock.getBlocks().add(block);
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
