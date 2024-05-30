package com.seafile.seadroid2.framework.worker;

import static com.seafile.seadroid2.config.Constants.PERIODIC_SCAN_INTERVALS;

import android.annotation.SuppressLint;
import android.content.Context;
import android.text.TextUtils;

import androidx.annotation.NonNull;
import androidx.room.RoomDatabase;
import androidx.work.Data;
import androidx.work.WorkerParameters;

import com.blankj.utilcode.util.CollectionUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.framework.data.model.enums.TransferDataSource;
import com.seafile.seadroid2.framework.datastore.StorageManager;
import com.seafile.seadroid2.framework.data.db.AppDatabase;
import com.seafile.seadroid2.framework.data.db.entities.RepoModel;
import com.seafile.seadroid2.framework.data.db.entities.FileTransferEntity;
import com.seafile.seadroid2.framework.data.model.enums.TransferAction;
import com.seafile.seadroid2.framework.data.model.enums.TransferResult;
import com.seafile.seadroid2.framework.data.model.enums.TransferStatus;
import com.seafile.seadroid2.framework.datastore.sp.FolderBackupManager;
import com.seafile.seadroid2.framework.notification.FolderBackupNotificationHelper;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.ui.folder_backup.RepoConfig;

import java.io.File;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Deque;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Collectors;


/**
 * Worker Tag:
 *
 * @see BackgroundJobManagerImpl#TAG_ALL
 * @see BackgroundJobManagerImpl#TAG_TRANSFER
 */
public class FolderBackupScannerWorker extends TransferWorker {
    public static final UUID UID = UUID.nameUUIDFromBytes(FolderBackupScannerWorker.class.getSimpleName().getBytes());

    private FolderBackupNotificationHelper notificationHelper;

    public FolderBackupScannerWorker(@NonNull Context context, @NonNull WorkerParameters workerParams) {
        super(context, workerParams);

        notificationHelper = new FolderBackupNotificationHelper(context);
    }

    @NonNull
    @Override
    public Result doWork() {
        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        if (account == null) {
            return Result.success();
        }

        String[] ids = getInputData().getStringArray(TransferWorker.DATA_CANCEL_IDS);
        if (ids != null && ids.length > 0) {

            removeFromDB(ids);

            return Result.success();
        }


        boolean canScan = checkCanScan();
        if (!canScan) {
            SLogs.d("UploadFolderBackupScanWorker: do not start the folder scan task this time");

            return Result.success();
        }

        List<String> backupPaths = FolderBackupManager.readBackupPaths();
        RepoConfig repoConfig = FolderBackupManager.readRepoConfig();
        if (CollectionUtils.isEmpty(backupPaths) || repoConfig == null) {
            return Result.success();
        }

        //
        String nTitle = getApplicationContext().getString(R.string.settings_folder_backup_info_title);
        nTitle += " - " + getApplicationContext().getString(R.string.is_scanning);

        notificationHelper.showNotification(nTitle);
        notificationHelper.cancel(3000);

        try {
            //send a scan event
            sendProgressEvent(TransferEvent.EVENT_SCANNING);

            //do
            doBackupPathWork(account, repoConfig, backupPaths);

        } finally {
            FolderBackupManager.writeLastScanTime(System.currentTimeMillis());

            //start upload worker
            BackgroundJobManagerImpl.getInstance().startFolderUploadWorker();
        }

        //Send a completion event
        Data data = new Data.Builder()
                .putString(TransferWorker.KEY_DATA_EVENT, TransferEvent.EVENT_SCAN_END)
                .putString(TransferWorker.KEY_DATA_TYPE, String.valueOf(TransferDataSource.FILE_BACKUP))
                .build();
        return Result.success(data);
    }

    private boolean checkCanScan() {
        boolean isOpenBackup = FolderBackupManager.readBackupSwitch();
        if (!isOpenBackup) {
            return false;
        }

        boolean isForce = getInputData().getBoolean(TransferWorker.DATA_FORCE_TRANSFER_KEY, false);
        if (isForce) {
            return true;
        }

        long lastScanTime = FolderBackupManager.readLastScanTime();
        if (lastScanTime != 0) {
            long now = System.currentTimeMillis();
            if (now - lastScanTime < PERIODIC_SCAN_INTERVALS) {
                return false;
            }
        }

        return true;
    }

    private void removeFromDB(String[] ids) {
//        Removed from the database
        List<String> idList = Arrays.asList(ids);
        List<FileTransferEntity> dbList = AppDatabase.getInstance().fileTransferDAO().getListByUidsSync(idList);

        if (CollectionUtils.isEmpty(dbList)) {
            return;
        }

        for (FileTransferEntity fileTransferEntity : dbList) {
            AppDatabase.getInstance().fileTransferDAO().deleteOne(fileTransferEntity);
        }

        //
        BackgroundJobManagerImpl.getInstance().startFolderUploadWorker();
    }

    private void doBackupPathWork(Account account, RepoConfig repoConfig, List<String> backupPathsList) {

        List<RepoModel> repoModels = AppDatabase.getInstance().repoDao().getByIdSync(repoConfig.getRepoID());

        if (CollectionUtils.isEmpty(repoModels)) {
            return;
        }

        RepoModel repoModel = repoModels.get(0);

        List<FileTransferEntity> transferEntityList = CollectionUtils.newArrayList();
        for (String backupPath : backupPathsList) {

            //iterate over local files
            List<File> localFiles = traverseFiles(backupPath);

            for (File file : localFiles) {
                FileTransferEntity fEntity = FileTransferEntity.convert2ThisForUploadFileSyncWorker(account, repoModel, file, backupPath);
                if (fEntity != null) {
                    transferEntityList.add(fEntity);
                }
            }
        }

        List<String> fullPaths = transferEntityList.stream().map(m -> m.full_path).collect(Collectors.toList());
        List<FileTransferEntity> existsList = readExistsListFromDB(repoConfig.getRepoID(), fullPaths);

        List<FileTransferEntity> newList = CollectionUtils.newArrayList();

        //compare
        if (!CollectionUtils.isEmpty(existsList)) {
            for (FileTransferEntity transferEntity : transferEntityList) {
                Optional<FileTransferEntity> optional = existsList.stream().filter(f -> TextUtils.equals(f.full_path, transferEntity.full_path)).findFirst();
                if (optional.isPresent()) {
                    FileTransferEntity dbEntity = optional.get();

                    if (dbEntity.data_status == -1) {
                        // has been deleted in db.
                        SLogs.d("folder backup scan: skip file(deleted): " + transferEntity.target_path);

                    } else if (TextUtils.equals(dbEntity.file_md5, transferEntity.file_md5)) {
                        //it's the same file，do not insert into db.

                    } else {
                        transferEntity.transfer_action = TransferAction.UPLOAD;
                        transferEntity.transfer_result = TransferResult.NO_RESULT;
                        transferEntity.transfer_status = TransferStatus.WAITING;
                        transferEntity.file_strategy = ExistingFileStrategy.REPLACE;
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
        if (!CollectionUtils.isEmpty(newList)) {
            AppDatabase.getInstance().fileTransferDAO().insertAll(newList);
        }
    }

    @SuppressLint("RestrictedApi")
    private List<FileTransferEntity> readExistsListFromDB(String repoId, List<String> fullPaths) {
        List<FileTransferEntity> existsList = CollectionUtils.newArrayList();

        int pageSize = RoomDatabase.MAX_BIND_PARAMETER_CNT;

        if (fullPaths.size() > pageSize) {

            //paginate the data
            for (int pageNumber = 1; pageNumber <= (fullPaths.size() + pageSize - 1) / pageSize; pageNumber++) {
                int fromIndex = (pageNumber - 1) * pageSize;
                int toIndex = Math.min(pageNumber * pageSize, fullPaths.size());
                List<String> pageListData = fullPaths.subList(fromIndex, toIndex);

                List<FileTransferEntity> tempExistsList = AppDatabase
                        .getInstance()
                        .fileTransferDAO()
                        .getListByFullPathsSync(repoId, pageListData, TransferAction.UPLOAD);
                existsList.addAll(tempExistsList);
            }
        } else {
            List<FileTransferEntity> tempExistsList = AppDatabase
                    .getInstance()
                    .fileTransferDAO()
                    .getListByFullPathsSync(repoId, fullPaths, TransferAction.UPLOAD);
            existsList.addAll(tempExistsList);
        }

        return existsList;
    }

    private List<File> traverseFiles(String backupPath) {
        return traverseFiles(CollectionUtils.newArrayList(backupPath));
    }

    private List<File> traverseFiles(List<String> backupPathsList) {
        Deque<File> stack = new ArrayDeque<>();

        for (String path : backupPathsList) {
            stack.push(new File(path));
        }

        //skip folder: /storage/emulated/0/Android/media/com.seafile.seadroid2.debug/
        String lPath = StorageManager.getInstance().getMediaDir().getAbsolutePath();
        lPath = Utils.getParentPath(lPath);

        List<File> filePathList = new ArrayList<>();

        boolean isSkipHiddenFile = FolderBackupManager.isFolderBackupSkipHiddenFiles();

        while (!stack.isEmpty()) {
            File currentDir = stack.pop();
            File[] files = currentDir.listFiles();

            if (files == null) {
                continue;
            }

            for (File file : files) {
                if (isSkipHiddenFile) {
                    if (!file.isHidden()) {
                        if (file.isDirectory()) {
                            stack.push(file);
                        } else {

                            String fPath = file.getAbsolutePath();
                            if (!fPath.startsWith(lPath)) {
                                filePathList.add(file);
                            }
                        }
                    }
                } else {
                    if (file.isDirectory()) {
                        stack.push(file);
                    } else {
                        String fPath = file.getAbsolutePath();
                        if (!fPath.startsWith(lPath)) {
                            filePathList.add(file);
                        }
                    }
                }
            }
        }
        return filePathList;
    }
}
