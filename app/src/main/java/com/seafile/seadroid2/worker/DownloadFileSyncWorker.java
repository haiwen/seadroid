package com.seafile.seadroid2.worker;

import android.content.Context;
import android.text.TextUtils;

import androidx.annotation.NonNull;
import androidx.work.WorkerParameters;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.GsonUtils;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.data.db.AppDatabase;
import com.seafile.seadroid2.data.model.dirents.DirentFileModel;
import com.seafile.seadroid2.data.db.entities.DirentModel;
import com.seafile.seadroid2.data.model.dirents.DirentRecursiveFileModel;
import com.seafile.seadroid2.data.db.entities.RepoModel;
import com.seafile.seadroid2.data.db.entities.FileTransferEntity;
import com.seafile.seadroid2.data.model.enums.TransferAction;
import com.seafile.seadroid2.data.model.enums.TransferResult;
import com.seafile.seadroid2.data.model.enums.TransferStatus;
import com.seafile.seadroid2.io.http.IO;
import com.seafile.seadroid2.ui.repo.RepoService;
import com.seafile.seadroid2.util.SLogs;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

public class DownloadFileSyncWorker extends TransferWorker {

    public DownloadFileSyncWorker(@NonNull Context context, @NonNull WorkerParameters workerParams) {
        super(context, workerParams);
    }

    @NonNull
    @Override
    public Result doWork() {
        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        if (account == null) {
            return Result.success();
        }

        String transferId = getInputData().getString(DATA_TRANSFER_KEY);
        String data = getInputData().getString(DATA_DIRENT_KEY);

        if (TextUtils.isEmpty(data) && TextUtils.isEmpty(transferId)) {
            //start download job
            BackgroundJobManagerImpl.getInstance().startFileDownloadJob();

            return Result.success();
        }

        if (!TextUtils.isEmpty(transferId)) {
            FileTransferEntity dbEntity = AppDatabase
                    .getInstance()
                    .fileTransferDAO()
                    .getByTransferId(transferId);

            if (dbEntity == null) {
                return Result.success();
            }

            //do not download if transfer is in progress or waiting.
            if (dbEntity.transfer_status == TransferStatus.TRANSFER_IN_PROGRESS ||
                    dbEntity.transfer_status == TransferStatus.TRANSFER_WAITING) {
                return Result.success();
            }

            dbEntity.transfer_status = TransferStatus.TRANSFER_IN_PROGRESS;
            dbEntity.transferred_size = 0;
            dbEntity.transfer_result = TransferResult.NO_RESULT;
            AppDatabase.getInstance().fileTransferDAO().update(dbEntity);
        }

        if (!TextUtils.isEmpty(data)) {
            DirentModel direntModel = GsonUtils.fromJson(data, DirentModel.class);
            if (direntModel == null) {
                return Result.success();
            }

            try {
                if (!direntModel.isDir()) {
                    insertIntoDbWhenDirentIsFile(account, direntModel);
                } else {
                    List<DirentRecursiveFileModel> list = getRecursiveFiles(direntModel);
                    insertIntoDbWhenDirentIsDir(account, direntModel, list);
                }
                return Result.success();
            } catch (IOException e) {
                return Result.failure();
            }
        }

        return Result.success();
    }


    private void insertIntoDbWhenDirentIsFile(Account account, DirentModel direntModel) throws IOException {

        RepoModel repoModel = AppDatabase.getInstance().repoDao().getOneByIdSync(direntModel.repo_id);
        if (repoModel == null) {
            SLogs.e("convertDirentModel2This: repoModel is null, when repoId = " + direntModel.repo_id);
            return;
        }

        DirentFileModel direntFileModel = fetchFile(direntModel);
        if (direntFileModel == null) {
            return;
        }

        FileTransferEntity transferEntity = FileTransferEntity.convertDirentModel2This(repoModel.encrypted, direntModel);
        if (transferEntity == null) {
            return;
        }

        //newest file id
        transferEntity.file_id = direntFileModel.id;
        transferEntity.file_size = direntFileModel.size;
        transferEntity.target_path = getLocalSaveDir(transferEntity).getAbsolutePath();

        FileTransferEntity existsEntity = AppDatabase
                .getInstance()
                .fileTransferDAO()
                .getOneByFullPath(account.getSignature(), transferEntity.full_path, TransferAction.DOWNLOAD);

        if (existsEntity != null) {
            if (TextUtils.equals(existsEntity.file_id, transferEntity.file_id)) {
                //it's the same file，do not insert into db.
                return;
            }
        }

        //insert
        AppDatabase.getInstance().fileTransferDAO().insert(transferEntity);

        //start upload worker
        BackgroundJobManagerImpl.getInstance().startFileDownloadJob();
    }

    private DirentFileModel fetchFile(DirentModel direntModel) throws IOException {
        retrofit2.Response<DirentFileModel> res = IO.getSingleton()
                .execute(RepoService.class)
                .getFileDetailCall(direntModel.repo_id, direntModel.full_path)
                .execute();
        if (!res.isSuccessful()) {
            return null;
        }

        return res.body();
    }

    private List<DirentRecursiveFileModel> getRecursiveFiles(DirentModel direntModel) throws IOException {
        retrofit2.Response<List<DirentRecursiveFileModel>> res = IO.getSingleton()
                .execute(RepoService.class)
                .getDirRecursiveFileCall(direntModel.repo_id, direntModel.full_path)
                .execute();
        if (!res.isSuccessful()) {
            return Collections.emptyList();
        }

        return res.body();
    }

    private void insertIntoDbWhenDirentIsDir(Account account, DirentModel direntModel, List<DirentRecursiveFileModel> list) {

        if (CollectionUtils.isEmpty(list)) {
            return;
        }

        RepoModel repoModel = AppDatabase.getInstance().repoDao().getOneByIdSync(direntModel.repo_id);
        if (repoModel == null) {
            SLogs.e("convertDirentModel2This: repoModel is null, when repoId = " + direntModel.repo_id);
            return;
        }

        List<FileTransferEntity> transferEntityList = new ArrayList<>();

        for (DirentRecursiveFileModel model : list) {
            FileTransferEntity transferEntity = FileTransferEntity.convertDirentRecursiveModel2This(repoModel, model);

            //newest file id
            transferEntity.file_id = model.id;
            transferEntity.file_size = model.size;
            transferEntity.target_path = getLocalSaveDir(transferEntity).getAbsolutePath();

            transferEntityList.add(transferEntity);
        }

        List<String> fullPaths = transferEntityList.stream().map(m -> m.full_path).collect(Collectors.toList());
        List<FileTransferEntity> existsList = AppDatabase
                .getInstance()
                .fileTransferDAO()
                .getListByFullPaths(account.getSignature(), fullPaths, TransferAction.DOWNLOAD);

        List<FileTransferEntity> newList = CollectionUtils.newArrayList();

        if (!CollectionUtils.isEmpty(existsList)) {
            for (FileTransferEntity transferEntity : transferEntityList) {
                Optional<FileTransferEntity> optional = existsList.stream().filter(f -> TextUtils.equals(f.full_path, transferEntity.full_path)).findFirst();
                if (optional.isPresent()) {
                    FileTransferEntity dbEntity = optional.get();
                    //Whether it's the same file
                    if (TextUtils.equals(dbEntity.file_id, transferEntity.file_id)) {
                        //it's the same file，do not insert into db.
                    } else {
                        newList.add(transferEntity);
                    }
                } else {
                    newList.add(transferEntity);
                }
            }
        } else {
            newList.addAll(transferEntityList);
        }

        //insert
        AppDatabase.getInstance().fileTransferDAO().insertAll(newList);

        BackgroundJobManagerImpl.getInstance().startFileDownloadJob();
    }
}
