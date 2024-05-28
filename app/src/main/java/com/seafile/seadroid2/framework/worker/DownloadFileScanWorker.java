package com.seafile.seadroid2.framework.worker;

import android.content.Context;
import android.text.TextUtils;

import androidx.annotation.NonNull;
import androidx.work.WorkerParameters;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.FileUtils;
import com.blankj.utilcode.util.ToastUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.framework.datastore.DataManager;
import com.seafile.seadroid2.framework.data.db.AppDatabase;
import com.seafile.seadroid2.framework.data.model.dirents.DirentFileModel;
import com.seafile.seadroid2.framework.data.db.entities.DirentModel;
import com.seafile.seadroid2.framework.data.model.dirents.DirentRecursiveFileModel;
import com.seafile.seadroid2.framework.data.db.entities.RepoModel;
import com.seafile.seadroid2.framework.data.db.entities.FileTransferEntity;
import com.seafile.seadroid2.framework.data.model.enums.TransferAction;
import com.seafile.seadroid2.framework.data.model.enums.TransferResult;
import com.seafile.seadroid2.framework.data.model.enums.TransferStatus;
import com.seafile.seadroid2.framework.http.IO;
import com.seafile.seadroid2.framework.notification.DownloadNotificationHelper;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.ui.file.FileService;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Collectors;


/**
 * Worker Tag:
 *
 * @see BackgroundJobManagerImpl#TAG_ALL
 * @see BackgroundJobManagerImpl#TAG_TRANSFER
 * @see BackgroundJobManagerImpl#TAG_TRANSFER_DOWNLOAD_SCAN
 * @see BackgroundJobManagerImpl#TAG_TRANSFER_DOWNLOAD_FILES_SCAN
 */
public class DownloadFileScanWorker extends TransferWorker {
    public static final UUID UID = UUID.nameUUIDFromBytes(DownloadFileScanWorker.class.getSimpleName().getBytes());

    private final DownloadNotificationHelper notificationManager;

    public DownloadFileScanWorker(@NonNull Context context, @NonNull WorkerParameters workerParams) {
        super(context, workerParams);

        notificationManager = new DownloadNotificationHelper(context);
    }

    @NonNull
    @Override
    public Result doWork() {
        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        if (account == null) {
            return Result.success();
        }

        // just only for cancel db.
        String[] rids = getInputData().getStringArray(DATA_CANCEL_IDS);
        String transferId = getInputData().getString(DATA_TRANSFER_KEY);
        String[] direntIds = getInputData().getStringArray(DATA_DIRENT_LIST_KEY);

        if (!TextUtils.isEmpty(transferId) || (direntIds != null && direntIds.length > 0)) {
            notificationManager.showNotification(R.string.download_waiting);
        }

        if (rids != null && rids.length > 0) {
            removeDownload(rids);
        }

        //
        //download single dirent
        if (!TextUtils.isEmpty(transferId)) {
            List<FileTransferEntity> dbList = AppDatabase.getInstance().fileTransferDAO().getByUid(transferId);

            if (CollectionUtils.isEmpty(dbList)) {
                return Result.success();
            }

            FileTransferEntity dbEntity = dbList.get(0);
            //do not download if transfer is in progress or waiting.
            if (dbEntity.transfer_status == TransferStatus.IN_PROGRESS || dbEntity.transfer_status == TransferStatus.WAITING) {
                return Result.success();
            }

            dbEntity.transfer_status = TransferStatus.IN_PROGRESS;
            dbEntity.transferred_size = 0;
            dbEntity.transfer_result = TransferResult.NO_RESULT;
            AppDatabase.getInstance().fileTransferDAO().update(dbEntity);
        }


        //download multiple dirents
        if (direntIds != null && direntIds.length > 0) {
            List<String> ids = Arrays.asList(direntIds);

            List<DirentModel> direntModels = AppDatabase.getInstance().direntDao().getListByIdsSync(ids);
            if (CollectionUtils.isEmpty(direntModels)) {
                return Result.success();
            }

            for (DirentModel direntModel : direntModels) {
                try {
                    if (!direntModel.isDir()) {
                        insertIntoDbWhenDirentIsFile(account, direntModel);
                    } else {
                        List<DirentRecursiveFileModel> list = fetchRecursiveFiles(direntModel);
                        insertIntoDbWhenDirentIsDir(account, direntModel, list);
                    }
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
        }

        //dismiss notification
        if (!TextUtils.isEmpty(transferId) || (direntIds != null && direntIds.length > 0)) {
            notificationManager.cancel();
        }

        //start upload worker
        BackgroundJobManagerImpl.getInstance().startFileDownloadWorker();

        //success
        return Result.success();
    }

    private void removeDownload(String[] ids) {
        //Removed from the database
        List<FileTransferEntity> dbList = AppDatabase
                .getInstance()
                .fileTransferDAO()
                .getListByUidsSync(Arrays.asList(ids));

        if (CollectionUtils.isEmpty(dbList)) {
            return;
        }

        for (FileTransferEntity fileTransferEntity : dbList) {
            AppDatabase.getInstance().fileTransferDAO().deleteOne(fileTransferEntity);

            if (fileTransferEntity.transfer_action == TransferAction.DOWNLOAD) {
                FileUtils.delete(fileTransferEntity.target_path);
                SLogs.d("deleted file: " + fileTransferEntity.target_path);
            }
        }

    }

    /**
     *
     */
    private void insertIntoDbWhenDirentIsFile(Account account, DirentModel direntModel) throws IOException {

        List<RepoModel> repoModels = AppDatabase.getInstance().repoDao().getByIdSync(direntModel.repo_id);
        if (CollectionUtils.isEmpty(repoModels)) {
            SLogs.d("convertDirentModel2This: repoModel is null, when repoId = " + direntModel.repo_id);
            return;
        }

        RepoModel repoModel = repoModels.get(0);
        DirentFileModel direntFileModel = fetchFile(direntModel);
        if (direntFileModel == null) {
            return;
        }

        FileTransferEntity transferEntity = FileTransferEntity.convertDirentModel2This(repoModel.canLocalDecrypt(), direntModel);

        //newest file id
        transferEntity.file_id = direntFileModel.id;
        transferEntity.file_size = direntFileModel.size;
        transferEntity.target_path = DataManager.getLocalRepoFile(account, transferEntity).getAbsolutePath();


        List<FileTransferEntity> existsList = AppDatabase.getInstance().fileTransferDAO().getListByFullPathsSync(direntModel.repo_id, CollectionUtils.newArrayList(transferEntity.full_path), TransferAction.DOWNLOAD);

        if (!CollectionUtils.isEmpty(existsList)) {
            FileTransferEntity existEntity = existsList.get(0);
            if (TransferStatus.SUCCEEDED == existEntity.transfer_status){
                if (TextUtils.equals(existsList.get(0).file_id, transferEntity.file_id)) {
                    //it's the same file，do not insert into db.
                    SLogs.d("file download: skip file(local exists): " + transferEntity.full_path);
                    ToastUtils.showLong(R.string.download_finished);
                    return;
                }
            }
        }

        transferEntity.transfer_status = TransferStatus.WAITING;
        transferEntity.transfer_result = TransferResult.NO_RESULT;
        transferEntity.transferred_size = 0;

        //insert
        AppDatabase.getInstance().fileTransferDAO().insert(transferEntity);
    }

    /**
     * insert into db
     */
    private void insertIntoDbWhenDirentIsDir(Account account, DirentModel direntModel, List<DirentRecursiveFileModel> list) {

        if (CollectionUtils.isEmpty(list)) {
            return;
        }

        List<RepoModel> repoModels = AppDatabase.getInstance().repoDao().getByIdSync(direntModel.repo_id);
        if (CollectionUtils.isEmpty(repoModels)) {
            SLogs.d("convertDirentModel2This: repoModel is null, when repoId = " + direntModel.repo_id);
            return;
        }

        RepoModel repoModel = repoModels.get(0);
        List<FileTransferEntity> transferEntityList = new ArrayList<>();

        for (DirentRecursiveFileModel model : list) {
            FileTransferEntity transferEntity = FileTransferEntity.convertDirentRecursiveModel2This(repoModel, model);

            //newest file id
            transferEntity.file_id = model.id;
            transferEntity.file_size = model.size;
            transferEntity.target_path = DataManager.getLocalRepoFile(account, transferEntity).getAbsolutePath();

            transferEntityList.add(transferEntity);
        }

        List<String> fullPaths = transferEntityList.stream().map(m -> m.full_path).collect(Collectors.toList());
        List<FileTransferEntity> existsList = AppDatabase.getInstance().fileTransferDAO().getListByFullPathsSync(direntModel.repo_id, fullPaths, TransferAction.DOWNLOAD);

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

        if (!newList.isEmpty()) {
            AppDatabase.getInstance().fileTransferDAO().insertAll(newList);
        }
    }

    /**
     * fetch file detail from server
     */
    private DirentFileModel fetchFile(DirentModel direntModel) throws IOException {
        retrofit2.Response<DirentFileModel> res = IO.getInstanceWithLoggedIn().execute(FileService.class).getFileDetailCall(direntModel.repo_id, direntModel.full_path).execute();
        if (!res.isSuccessful()) {
            return null;
        }

        return res.body();
    }

    /**
     * get recursive files from server
     */
    private List<DirentRecursiveFileModel> fetchRecursiveFiles(DirentModel direntModel) throws IOException {
        retrofit2.Response<List<DirentRecursiveFileModel>> res = IO.getInstanceWithLoggedIn().execute(FileService.class).getDirRecursiveFileCall(direntModel.repo_id, direntModel.full_path).execute();
        if (!res.isSuccessful()) {
            return Collections.emptyList();
        }

        return res.body();
    }

}
