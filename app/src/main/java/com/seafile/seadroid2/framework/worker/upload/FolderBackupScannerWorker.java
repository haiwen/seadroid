package com.seafile.seadroid2.framework.worker.upload;

import static com.seafile.seadroid2.config.Constants.PERIODIC_SCAN_INTERVALS;

import android.app.ForegroundServiceStartNotAllowedException;
import android.content.Context;
import android.os.Build;
import android.text.TextUtils;

import androidx.annotation.NonNull;
import androidx.work.Data;
import androidx.work.ForegroundInfo;
import androidx.work.NetworkType;
import androidx.work.WorkerParameters;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.NetworkUtils;
import com.google.common.base.Stopwatch;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.config.Constants;
import com.seafile.seadroid2.enums.TransferDataSource;
import com.seafile.seadroid2.enums.TransferResult;
import com.seafile.seadroid2.framework.data.db.AppDatabase;
import com.seafile.seadroid2.framework.data.db.entities.DirentModel;
import com.seafile.seadroid2.framework.data.db.entities.FileTransferEntity;
import com.seafile.seadroid2.framework.data.db.entities.RepoModel;
import com.seafile.seadroid2.framework.data.model.dirents.DirentRecursiveFileModel;
import com.seafile.seadroid2.framework.data.model.repo.DirentWrapperModel;
import com.seafile.seadroid2.framework.datastore.StorageManager;
import com.seafile.seadroid2.framework.datastore.sp_livedata.AlbumBackupSharePreferenceHelper;
import com.seafile.seadroid2.framework.datastore.sp_livedata.FolderBackupSharePreferenceHelper;
import com.seafile.seadroid2.framework.http.HttpIO;
import com.seafile.seadroid2.framework.notification.FolderBackupScanNotificationHelper;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.framework.worker.BackgroundJobManagerImpl;
import com.seafile.seadroid2.framework.worker.ExistingFileStrategy;
import com.seafile.seadroid2.framework.worker.TransferEvent;
import com.seafile.seadroid2.framework.worker.TransferWorker;
import com.seafile.seadroid2.ui.folder_backup.RepoConfig;
import com.seafile.seadroid2.ui.repo.RepoService;

import org.apache.commons.lang3.StringUtils;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Deque;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.TimeUnit;
import java.util.function.Function;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import retrofit2.Call;


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

    private final FolderBackupScanNotificationHelper notificationManager;

    public FolderBackupScannerWorker(@NonNull Context context, @NonNull WorkerParameters workerParams) {
        super(context, workerParams);

        notificationManager = new FolderBackupScanNotificationHelper(context);
    }

    private void showNotification() {

        //
        String title = getApplicationContext().getString(R.string.settings_folder_backup_info_title);
        String subTitle = getApplicationContext().getString(R.string.is_scanning);

        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.S) {
            try {
                ForegroundInfo foregroundInfo = notificationManager.getForegroundNotification(title,subTitle);
                showForegroundAsync(foregroundInfo);
            } catch (ForegroundServiceStartNotAllowedException e) {
                SLogs.e(e.getMessage());
            }
        } else {
            ForegroundInfo foregroundInfo = notificationManager.getForegroundNotification(title,subTitle);
            showForegroundAsync(foregroundInfo);
        }
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
            return Result.success();
        }

        boolean isTurnOn = FolderBackupSharePreferenceHelper.readBackupSwitch();
        if (!isTurnOn) {
            SLogs.i("The folder scan task was not started, because the switch is off");
            return Result.success(getOutData());
        }

        RepoConfig repoConfig = FolderBackupSharePreferenceHelper.readRepoConfig();
        if (repoConfig == null || StringUtils.isEmpty(repoConfig.getRepoId())) {
            SLogs.i("The folder scan task was not started, because the repo is not selected");
            return Result.success(getOutData());
        }

        List<String> backupPaths = FolderBackupSharePreferenceHelper.readBackupPathsAsList();
        if (CollectionUtils.isEmpty(backupPaths)) {
            SLogs.i("The folder scan task was not started, because the folder path is not selected");
            return Result.success(getOutData());
        }

        boolean isForce = getInputData().getBoolean(TransferWorker.DATA_FORCE_TRANSFER_KEY, false);
        if (isForce) {
            FolderBackupSharePreferenceHelper.resetLastScanTime();
        }

        showNotification();

        try {
            //send a scan event
            sendEvent(TransferDataSource.FOLDER_BACKUP, TransferEvent.EVENT_SCANNING);

            //do
            traverseBackupPath(account, repoConfig, backupPaths);

        } catch (IOException e) {
            SLogs.e("FolderBackupScannerWorker has occurred error", e);
        } finally {
            FolderBackupSharePreferenceHelper.writeLastScanTime(System.currentTimeMillis());
        }

        //get total count: WAITING, IN_PROGRESS, FAILED
        long totalPendingCount = getCurrentPendingCount(account, TransferDataSource.FOLDER_BACKUP);
        String content = null;
        if (totalPendingCount > 0) {
            boolean isAllowUpload = checkNetworkTypeIfAllowStartUploadWorker();
            if (!isAllowUpload) {
                content = TransferResult.WAITING.name();
            }
        }

        return Result.success(getOutData(content));
    }

    private Data getOutData() {
        return getOutData(null);
    }

    private Data getOutData(String content) {
        return new Data.Builder()
                .putString(TransferWorker.KEY_DATA_STATUS, TransferEvent.EVENT_SCAN_END)
                .putString(TransferWorker.KEY_DATA_SOURCE, TransferDataSource.FOLDER_BACKUP.name())
                .putString(TransferWorker.KEY_DATA_RESULT, content)
                .build();
    }

    private boolean checkNetworkTypeIfAllowStartUploadWorker() {
        boolean isAllowData = FolderBackupSharePreferenceHelper.readDataPlanAllowed();
        if (isAllowData) {
            return true;
        }

        //如果不允许数据上传，但是当前是数据网络
        return !NetworkUtils.isMobileData();
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


    private List<DirentRecursiveFileModel> getDirentWrapper(String repoId, String parentPath) throws IOException {
        //get parent dirent list from remote
        Call<List<DirentRecursiveFileModel>> direntWrapperModelCall = HttpIO.getCurrentInstance().execute(RepoService.class).getDirRecursiveFileCall(repoId, parentPath);
        retrofit2.Response<List<DirentRecursiveFileModel>> res = direntWrapperModelCall.execute();
        if (!res.isSuccessful()) {
            SLogs.e("FolderBackupScannerWorker -> getDirRecursiveFileCall() -> request dirents failed");
            return null;
        }

        List<DirentRecursiveFileModel> tempWrapperList = res.body();
        if (tempWrapperList == null) {
            SLogs.e("FolderBackupScannerWorker -> getDirRecursiveFileCall() -> request dirents is null");
            return null;
        }

        return tempWrapperList;
    }

    private void traverseBackupPath(Account account, RepoConfig repoConfig, List<String> backupPathsList) throws IOException {

        List<RepoModel> repoModels = AppDatabase.getInstance().repoDao().getByIdSync(repoConfig.getRepoId());

        if (CollectionUtils.isEmpty(repoModels)) {
            return;
        }

        //skip folder: /storage/emulated/0/Android/media/com.seafile.seadroid2(.debug)/
        String ignorePath = StorageManager.getInstance().getMediaDir().getAbsolutePath();
        ignorePath = Utils.getParentPath(ignorePath);

        RepoModel repoModel = repoModels.get(0);
        long lastTime = FolderBackupSharePreferenceHelper.readLastScanTime();

        Stopwatch stopwatch = Stopwatch.createStarted();
        for (String backupPath : backupPathsList) {

            backupPath = Utils.pathJoin("/", backupPath, "/");

            List<FileTransferEntity> newList = compare(account, repoModel, backupPath, lastTime, ignorePath);
            if (CollectionUtils.isEmpty(newList)) {
                SLogs.e("folder backup：no new/update files: " + backupPath);
                continue;
            }

            SLogs.e("folder backup：need to upload files count: " + newList.size());
            AppDatabase.getInstance().fileTransferDAO().insertAll(newList);
        }

        stopwatch.stop();
        long diff = stopwatch.elapsed(TimeUnit.MILLISECONDS);
        SLogs.e("folder backup scan time：" + stopwatch);
        long now = System.currentTimeMillis();
        FolderBackupSharePreferenceHelper.writeLastScanTime(now - diff);
    }

    private List<FileTransferEntity> compare(Account account, RepoModel repoModel, String backupPath, long lastScanTime, String ignorePath) throws IOException {

        // ../sdcard/../Download/a/
        String parentAbsPath = Utils.getParentPath(backupPath);
        parentAbsPath = Utils.pathJoin("/", parentAbsPath, "/");

        //
        String parentPath = StringUtils.removeStart(backupPath, parentAbsPath);
        // Make sure there are slash symbols on the front and back
        parentPath = Utils.pathJoin("/", parentPath, "/");

        // iterate over local files
        List<File> localFiles = traverseFiles(backupPath, lastScanTime, ignorePath);

        if (CollectionUtils.isEmpty(localFiles)) {
            return null;
        }

        // get local file transfer list
        List<FileTransferEntity> dbList = AppDatabase
                .getInstance()
                .fileTransferDAO()
                .getFullListByParentPathSync(repoModel.repo_id, parentPath, TransferDataSource.FOLDER_BACKUP);

        Map<String, FileTransferEntity> dbTransferMap = null;
        if (!CollectionUtils.isEmpty(dbList)) {
            dbTransferMap = dbList.stream().collect(Collectors.toMap(FileTransferEntity::getFullFileName, Function.identity()));
        }

        // get remote dirent list
        List<DirentRecursiveFileModel> direntWrapperModel = getDirentWrapper(repoModel.repo_id, parentPath);
        Map<String, DirentRecursiveFileModel> remoteDirentMap = null;
        if (!CollectionUtils.isEmpty(direntWrapperModel)) {
            remoteDirentMap = direntWrapperModel.stream().filter(f -> !f.isDir()).collect(Collectors.toMap(DirentRecursiveFileModel::getFullFileName, Function.identity()));
        }

        List<FileTransferEntity> newList = CollectionUtils.newArrayList();

        for (File localFile : localFiles) {

            // localFile is /storage/emulated/0/aaa/bbb/ccc.docx
            // backupPath is /storage/emulated/0/aaa/
            // parentBackupPath is /storage/emulated/0/
            // fullFileName = /aaa/bbb/ccc.docx
            String fullFileName = StringUtils.removeStart(localFile.getAbsolutePath(), parentAbsPath);
            fullFileName = Utils.pathJoin("/", fullFileName);

            FileTransferEntity tempTransferEntity = FileTransferEntity.convert2ThisForUploadFileSyncWorker(account, localFile, backupPath);
            if (tempTransferEntity == null) {
                SLogs.d("folder backup scan: skip file(file is a folder): " + fullFileName);
                continue;
            }

            if (dbTransferMap != null && remoteDirentMap != null) {

                FileTransferEntity dbTransferEntity = dbTransferMap.getOrDefault(fullFileName, null);
                DirentRecursiveFileModel remoteDirentModel = remoteDirentMap.getOrDefault(fullFileName, null);
                //
                if (dbTransferEntity != null && remoteDirentModel != null) {
                    if (TextUtils.equals(dbTransferEntity.file_id, remoteDirentModel.id)) {
                        // it's the same file, do not insert into db.
                        SLogs.d("folder backup scan: skip file(same file): " + tempTransferEntity.target_path);
                    } else {
                        // update file
                        tempTransferEntity.file_strategy = ExistingFileStrategy.REPLACE;
                        newList.add(tempTransferEntity);
                    }
                } else if (dbTransferEntity != null) {
                    if (dbTransferEntity.data_status == Constants.DataStatus.DELETED) {
                        // has been deleted in db.
                        SLogs.d("folder backup scan: skip file(deleted): " + dbTransferEntity.target_path);
                    } else if (TextUtils.equals(dbTransferEntity.file_md5, tempTransferEntity.file_md5)) {
                        // Maybe the network is wrong
                        // check if this file has changed. it's the same file, will not be uploaded again
                        SLogs.d("folder backup scan: skip file(same file): " + tempTransferEntity.target_path);
                    } else {
                        SLogs.d("folder backup scan: need to update file: " + tempTransferEntity.target_path);
                        tempTransferEntity.file_strategy = ExistingFileStrategy.APPEND;
                        newList.add(tempTransferEntity);
                    }
                } else if (remoteDirentModel != null) {
                    //there is no upload record locally, but there is a file with the same name remotely
                    tempTransferEntity.file_strategy = ExistingFileStrategy.REPLACE;
                    newList.add(tempTransferEntity);
                } else {
                    tempTransferEntity.file_strategy = ExistingFileStrategy.APPEND;
                    newList.add(tempTransferEntity);
                }
            } else if (remoteDirentMap != null) {
                // local transferred db is empty, it means that the local database may have been deleted
                // re-upload, make sure local database is kept in sync

                tempTransferEntity.file_strategy = ExistingFileStrategy.REPLACE;
                newList.add(tempTransferEntity);

            } else if (dbTransferMap != null) {
                // remote dir is empty.
                // it could mean that the remote folder has already been manually deleted

                FileTransferEntity dbTransferEntity = dbTransferMap.getOrDefault(fullFileName, null);
                if (dbTransferEntity == null) {
                    tempTransferEntity.file_strategy = ExistingFileStrategy.APPEND;
                    newList.add(tempTransferEntity);
                } else if (dbTransferEntity.data_status == Constants.DataStatus.DELETED) {
                    // has been deleted in db.
                    SLogs.d("folder backup scan: skip file(deleted): " + dbTransferEntity.target_path);
                } else if (TextUtils.equals(dbTransferEntity.file_md5, tempTransferEntity.file_md5)) {
                    // todo ???
                    // Check if this file has changed. it's the same file, will not be uploaded again
                    SLogs.d("folder backup scan: skip file(same file): " + tempTransferEntity.target_path);
                } else {
                    SLogs.d("folder backup scan: need to update file: " + dbTransferEntity.target_path);
                    tempTransferEntity.file_strategy = ExistingFileStrategy.REPLACE;
                    newList.add(dbTransferEntity);
                }
            } else {
                // The new file
                SLogs.d("folder backup scan: new file: " + tempTransferEntity.target_path);
                tempTransferEntity.file_strategy = ExistingFileStrategy.APPEND;
                newList.add(tempTransferEntity);
            }
        }

        return newList;
    }

    private Pattern getPattern(String prefix, String suffix) {
        if (TextUtils.isEmpty(prefix)) {
            // file format is '.txt'
        }

        if (TextUtils.isEmpty(suffix)) {
            // file format is 'a'
            suffix = "";
        }

        return Pattern.compile(Pattern.quote(prefix) + "( \\(\\d+\\))?" + Pattern.quote(suffix));
    }

    private List<File> traverseFiles(String path, long lastScanTime, String ignorePath) {
        Deque<File> stack = new ArrayDeque<>();
        stack.push(new File(path));

        List<File> filePathList = new ArrayList<>();
        boolean isSkipHiddenFile = FolderBackupSharePreferenceHelper.isFolderBackupSkipHiddenFiles();
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
                    processFile(file, filePathList, ignorePath, lastScanTime);
                }
            }
        }

        return filePathList;
    }

    private void processFile(File file, List<File> filePathList, String ignorePath, long lastScanTime) {
        String fPath = file.getAbsolutePath();

        if (fPath.startsWith(ignorePath)) {
            return;
        }

        if (TextUtils.isEmpty(file.getName())) {
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
