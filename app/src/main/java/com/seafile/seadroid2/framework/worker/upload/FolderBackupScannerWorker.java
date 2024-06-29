package com.seafile.seadroid2.framework.worker.upload;

import static com.seafile.seadroid2.config.Constants.PERIODIC_SCAN_INTERVALS;

import android.content.Context;
import android.text.TextUtils;

import androidx.annotation.NonNull;
import androidx.work.Data;
import androidx.work.ForegroundInfo;
import androidx.work.WorkerParameters;

import com.blankj.utilcode.util.CollectionUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.config.Constants;
import com.seafile.seadroid2.framework.data.db.AppDatabase;
import com.seafile.seadroid2.framework.data.db.entities.FileTransferEntity;
import com.seafile.seadroid2.framework.data.db.entities.RepoModel;
import com.seafile.seadroid2.framework.data.model.enums.TransferAction;
import com.seafile.seadroid2.framework.data.model.enums.TransferDataSource;
import com.seafile.seadroid2.framework.data.model.enums.TransferResult;
import com.seafile.seadroid2.framework.data.model.enums.TransferStatus;
import com.seafile.seadroid2.framework.datastore.StorageManager;
import com.seafile.seadroid2.framework.datastore.sp.FolderBackupManager;
import com.seafile.seadroid2.framework.notification.FolderBackupScanNotificationHelper;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.framework.worker.BackgroundJobManagerImpl;
import com.seafile.seadroid2.framework.worker.ExistingFileStrategy;
import com.seafile.seadroid2.framework.worker.TransferEvent;
import com.seafile.seadroid2.framework.worker.TransferWorker;
import com.seafile.seadroid2.ui.folder_backup.RepoConfig;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Deque;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Collectors;


/**
 * <p>
 * FileMonitor is {@link com.seafile.seadroid2.framework.file_monitor.SupportFileAlterationMonitor}
 * </p>
 * <p>
 * Worker Tag:
 *
 * @see BackgroundJobManagerImpl#TAG_ALL
 * @see BackgroundJobManagerImpl#TAG_TRANSFER
 */
public class FolderBackupScannerWorker extends TransferWorker {
    public static final UUID UID = UUID.nameUUIDFromBytes(FolderBackupScannerWorker.class.getSimpleName().getBytes());

    private final FolderBackupScanNotificationHelper notificationHelper;

    public FolderBackupScannerWorker(@NonNull Context context, @NonNull WorkerParameters workerParams) {
        super(context, workerParams);

        notificationHelper = new FolderBackupScanNotificationHelper(context);
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
            List<String> idList = Arrays.asList(ids);
            //
            removeFromDB(idList);

            BackgroundJobManagerImpl.getInstance().startFolderUploadWorker();

            return Result.success();
        }

        boolean canScan = checkCanScan();
        if (!canScan) {
            SLogs.i("The folder scan task was not started");

            BackgroundJobManagerImpl.getInstance().startFolderUploadWorker();

            return Result.success(getScanEndData());
        }

        List<String> backupPaths = FolderBackupManager.readBackupPaths();
        RepoConfig repoConfig = FolderBackupManager.readRepoConfig();
        if (CollectionUtils.isEmpty(backupPaths) || repoConfig == null) {

            return Result.success(getScanEndData());
        }

        //
        String title = getApplicationContext().getString(R.string.settings_folder_backup_info_title);
        String subTitle = getApplicationContext().getString(R.string.is_scanning);

        ForegroundInfo foregroundInfo = notificationHelper.getForegroundNotification(title, subTitle);
        showForegroundAsync(foregroundInfo);
//        notificationHelper.showNotification(nTitle);

        try {
            //send a scan event
            sendEvent(TransferEvent.EVENT_SCANNING, TransferDataSource.FOLDER_BACKUP);

            //do
            traverseBackupPath(account, repoConfig, backupPaths);

        } finally {
            //
//            notificationHelper.cancel();

            FolderBackupManager.writeLastScanTime(System.currentTimeMillis());

            //start upload worker
            BackgroundJobManagerImpl.getInstance().startFolderUploadWorker();
        }

        return Result.success(getScanEndData());
    }

    private Data getScanEndData() {
        return new Data.Builder()
                .putString(TransferWorker.KEY_DATA_EVENT, TransferEvent.EVENT_SCAN_END)
                .putString(TransferWorker.KEY_DATA_TYPE, String.valueOf(TransferDataSource.FOLDER_BACKUP))
                .build();
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

    private void removeFromDB(List<String> ids) {
        List<FileTransferEntity> dbList = AppDatabase.getInstance().fileTransferDAO().getListByUidsSync(ids);
        if (CollectionUtils.isEmpty(dbList)) {
            return;
        }

        for (FileTransferEntity fileTransferEntity : dbList) {
            AppDatabase.getInstance().fileTransferDAO().deleteOne(fileTransferEntity);
        }
    }

    private void traverseBackupPath(Account account, RepoConfig repoConfig, List<String> backupPathsList) {

        List<RepoModel> repoModels = AppDatabase.getInstance().repoDao().getByIdSync(repoConfig.getRepoID());

        if (CollectionUtils.isEmpty(repoModels)) {
            return;
        }

        RepoModel repoModel = repoModels.get(0);

        for (String backupPath : backupPathsList) {
            long lastTime = FolderBackupManager.readBackupPathLastScanTime(backupPath);

            //iterate over local files
            List<File> localFiles = traverseFiles(backupPath, lastTime);

            //write last scan time
            FolderBackupManager.writeBackupPathLastScanTime(backupPath);

            if (CollectionUtils.isEmpty(localFiles)) {
                SLogs.e("没有新增、更新的文件: " + backupPath);
                continue;
            }
            SLogs.e("新文件: " + localFiles.size());

            int pageSize = 99;
            if (localFiles.size() < pageSize) {
                pageSize = localFiles.size();
            }

            for (int pageNumber = 1; pageNumber <= (localFiles.size() + pageSize - 1) / pageSize; pageNumber++) {
                int fromIndex = (pageNumber - 1) * pageSize;
                int toIndex = Math.min(pageNumber * pageSize, localFiles.size());
                List<File> subFiles = localFiles.subList(fromIndex, toIndex);

                compareToLocalAndInsert(account, repoModel, backupPath, subFiles);
            }
        }
    }

    private void compareToLocalAndInsert(Account account, RepoModel repoModel, String backupPath, List<File> subFiles) {
        if (CollectionUtils.isEmpty(subFiles)) {
            return;
        }

        List<String> fullPaths = subFiles.stream().map(File::getAbsolutePath).collect(Collectors.toList());

        List<FileTransferEntity> tempExistsList = AppDatabase
                .getInstance()
                .fileTransferDAO()
                .getListByFullPathsSync(repoModel.repo_id, fullPaths, TransferAction.UPLOAD);

        List<FileTransferEntity> tList = CollectionUtils.newArrayList();
        for (File file : subFiles) {
            FileTransferEntity fEntity = FileTransferEntity.convert2ThisForUploadFileSyncWorker(account, repoModel, file, backupPath);
            if (fEntity != null) {
                tList.add(fEntity);
            }
        }

        if (CollectionUtils.isEmpty(tempExistsList)) {
            AppDatabase.getInstance().fileTransferDAO().insertAll(tList);
            return;
        }

        List<FileTransferEntity> newList = CollectionUtils.newArrayList();

        int i = 0;

        for (FileTransferEntity transferEntity : tList) {
            i++;

            Optional<FileTransferEntity> optional = tempExistsList.stream().filter(f -> TextUtils.equals(f.full_path, transferEntity.full_path)).findFirst();
            if (!optional.isPresent()) {
                newList.add(transferEntity);
                SLogs.d(i + " :folder backup scan: new file(local empty): " + transferEntity.target_path);
                continue;
            }

            FileTransferEntity dbEntity = optional.get();
            if (dbEntity.data_status == Constants.DataStatus.DELETED) {
                // has been deleted in db.
                SLogs.d(i + " :folder backup scan: skip file(deleted): " + transferEntity.target_path);

            } else if (TextUtils.equals(dbEntity.file_md5, transferEntity.file_md5)) {
                //it's the same file，do not insert into db.
                SLogs.d(i + " :folder backup scan: skip file(same file): " + transferEntity.target_path);

            } else {
                SLogs.d(i + " :folder backup scan: new file: " + transferEntity.target_path);

                transferEntity.transfer_action = TransferAction.UPLOAD;
                transferEntity.transfer_result = TransferResult.NO_RESULT;
                transferEntity.transfer_status = TransferStatus.WAITING;
                transferEntity.file_strategy = ExistingFileStrategy.REPLACE;
                newList.add(transferEntity);
            }
        }

        if (CollectionUtils.isEmpty(newList)) {
            AppDatabase.getInstance().fileTransferDAO().insertAll(newList);
        }

    }


    private List<File> traverseFiles(String path, long lastScanTime) {
        Deque<File> stack = new ArrayDeque<>();
        stack.push(new File(path));

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
                if (isSkipHiddenFile && file.isHidden()) {
                    continue;
                }

                if (file.isDirectory()) {
                    stack.push(file);
                } else {
                    processFile(file, filePathList, lPath, lastScanTime);
                }
            }
        }

        return filePathList;
    }

    private void processFile(File file, List<File> filePathList, String lPath, long lastScanTime) {
        String fPath = file.getAbsolutePath();

        if (fPath.startsWith(lPath)) {
            return;
        }

        if (lastScanTime > 0) {
            try {
                BasicFileAttributes attr = Files.readAttributes(file.toPath(), BasicFileAttributes.class);
                long created = attr.creationTime().toMillis();
                long modified = attr.lastModifiedTime().toMillis();

                if (created > lastScanTime || modified > lastScanTime) {
                    filePathList.add(file);
                }
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        } else {
            filePathList.add(file);
        }
    }
}
