package com.seafile.seadroid2.framework.worker.upload;

import android.app.ForegroundServiceStartNotAllowedException;
import android.content.Context;
import android.os.Build;
import android.text.TextUtils;

import androidx.annotation.NonNull;
import androidx.work.ForegroundInfo;
import androidx.work.WorkerParameters;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.ToastUtils;
import com.google.common.base.Stopwatch;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.enums.SaveTo;
import com.seafile.seadroid2.enums.TransferDataSource;
import com.seafile.seadroid2.enums.TransferResult;
import com.seafile.seadroid2.framework.db.AppDatabase;
import com.seafile.seadroid2.framework.db.entities.FileBackupStatusEntity;
import com.seafile.seadroid2.framework.db.entities.RepoModel;
import com.seafile.seadroid2.framework.model.dirents.DirentRecursiveFileModel;
import com.seafile.seadroid2.framework.worker.queue.TransferModel;
import com.seafile.seadroid2.framework.datastore.StorageManager;
import com.seafile.seadroid2.framework.datastore.sp_livedata.FolderBackupSharePreferenceHelper;
import com.seafile.seadroid2.framework.http.HttpIO;
import com.seafile.seadroid2.framework.notification.FolderBackupScanNotificationHelper;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.framework.worker.BackgroundJobManagerImpl;
import com.seafile.seadroid2.framework.worker.GlobalTransferCacheList;
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
import java.util.Deque;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.TimeUnit;
import java.util.function.Function;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import retrofit2.Call;


/**
 * <p>
 * File Monitor is {@link com.seafile.seadroid2.framework.file_monitor.SupportFileAlterationMonitor}
 * </p>
 * <p>
 * Worker Tag:
 *
 * @see BackgroundJobManagerImpl#TAG_ALL
 * @see BackgroundJobManagerImpl#TAG_TRANSFER
 */
public class FolderBackupScanWorker extends BaseScanWorker {
    private final String TAG = "FolderBackupScanWorker";
    public static final UUID UID = UUID.nameUUIDFromBytes(FolderBackupScanWorker.class.getSimpleName().getBytes());

    private final FolderBackupScanNotificationHelper notificationManager;
    private RepoConfig repoConfig;
    private Account account;

    public FolderBackupScanWorker(@NonNull Context context, @NonNull WorkerParameters workerParams) {
        super(context, workerParams);

        notificationManager = new FolderBackupScanNotificationHelper(context);
    }

    @Override
    public TransferDataSource getDataSource() {
        return TransferDataSource.FOLDER_BACKUP;
    }

    private void showNotification() {

        //
        String title = getApplicationContext().getString(R.string.settings_folder_backup_info_title);
        String subTitle = getApplicationContext().getString(R.string.is_scanning);

        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.S) {
            try {
                ForegroundInfo foregroundInfo = notificationManager.getForegroundNotification(title, subTitle);
                showForegroundAsync(foregroundInfo);
            } catch (ForegroundServiceStartNotAllowedException e) {
                SLogs.e(e.getMessage());
            }
        } else {
            ForegroundInfo foregroundInfo = notificationManager.getForegroundNotification(title, subTitle);
            showForegroundAsync(foregroundInfo);
        }
    }

    @NonNull
    @Override
    public Result doWork() {
        SLogs.d(TAG, "doWork()", "started execution");
        account = SupportAccountManager.getInstance().getCurrentAccount();
        if (account == null) {
            return returnSuccess();
        }

        boolean isTurnOn = FolderBackupSharePreferenceHelper.readBackupSwitch();
        if (!isTurnOn) {
            SLogs.d(TAG, "doWork()", "The folder scan task was not started, because the switch is off");
            return returnSuccess();
        }

        repoConfig = FolderBackupSharePreferenceHelper.readRepoConfig();
        if (repoConfig == null || StringUtils.isEmpty(repoConfig.getRepoId())) {
            SLogs.d(TAG, "doWork()", "The folder scan task was not started, because the repo is not selected");
            return returnSuccess();
        }

        List<String> backupPaths = FolderBackupSharePreferenceHelper.readBackupPathsAsList();
        if (CollectionUtils.isEmpty(backupPaths)) {
            SLogs.d(TAG, "doWork()", "The folder scan task was not started, because the folder path is not selected");

            return returnSuccess();
        }

        boolean isForce = getInputData().getBoolean(TransferWorker.DATA_FORCE_TRANSFER_KEY, false);
        if (isForce) {
            FolderBackupSharePreferenceHelper.resetLastScanTime();
        }

        showNotification();

        try {
            //send a scan event
            sendWorkerEvent(TransferDataSource.FOLDER_BACKUP, TransferEvent.EVENT_SCANNING);

            //do
            traverseBackupPath(backupPaths);

        } catch (IOException e) {
            SLogs.e("FolderBackupScannerWorker has occurred error", e);
        } finally {
            FolderBackupSharePreferenceHelper.writeLastScanTime(System.currentTimeMillis());
        }

        int totalPendingCount = GlobalTransferCacheList.FOLDER_BACKUP_QUEUE.getPendingCount();
        String content = null;
        if (totalPendingCount > 0) {
            boolean isAllowUpload = checkNetworkTypeIfAllowStartUploadWorker();
            if (!isAllowUpload) {
                content = TransferResult.WAITING.name();
            }
        }

        sendFinishScanEvent(content, totalPendingCount);
        return Result.success();
    }


    private List<DirentRecursiveFileModel> getDirentWrapper(String repoId, String parentPath) throws IOException {
        //get parent dirent list from remote
        Call<List<DirentRecursiveFileModel>> direntWrapperModelCall = HttpIO.getCurrentInstance().execute(RepoService.class).getDirRecursiveFileCall(repoId, parentPath);
        retrofit2.Response<List<DirentRecursiveFileModel>> res = direntWrapperModelCall.execute();
        if (!res.isSuccessful()) {
            SLogs.d(TAG, "request dirents failed");
            return null;
        }

        List<DirentRecursiveFileModel> tempWrapperList = res.body();
        if (tempWrapperList == null) {
            SLogs.d(TAG, "request dirents is null");
            return null;
        }

        return tempWrapperList;
    }

    private void traverseBackupPath(List<String> backupPathsList) throws IOException {

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

            compare(repoModel, backupPath, lastTime, ignorePath);
            SLogs.d(TAG, "traverseBackupPath()", "folder backup path: " + backupPath);
            SLogs.d(TAG, "traverseBackupPath()", "folder backup：need to upload files count: " + GlobalTransferCacheList.FOLDER_BACKUP_QUEUE.getTotalCount());
        }

        stopwatch.stop();
        long diff = stopwatch.elapsed(TimeUnit.MILLISECONDS);
        SLogs.d(TAG, "traverseBackupPath()", "folder backup scan time：" + stopwatch);
        long now = System.currentTimeMillis();
        FolderBackupSharePreferenceHelper.writeLastScanTime(now - diff);
    }

    private void compare(RepoModel repoModel, String backupPath, long lastScanTime, String ignorePath) throws IOException {

        // backupPath: /storage/emulated/0/aaa/bbb/
        // parentAbsPath: /storage/emulated/0/aaa/
        String parentAbsPath = Utils.getParentPath(backupPath);
        parentAbsPath = Utils.pathJoin("/", parentAbsPath, "/");

        // parentPath: /bbb/
        String parentPath = StringUtils.removeStart(backupPath, parentAbsPath);
        parentPath = Utils.pathJoin("/", parentPath, "/");

        // iterate over local files
        List<File> localFiles = traverseFiles(backupPath, lastScanTime, ignorePath);
        if (CollectionUtils.isEmpty(localFiles)) {
            return;
        }

        // get local file transfer list
        List<FileBackupStatusEntity> dbList = AppDatabase
                .getInstance()
                .fileTransferDAO()
                .getFullListByParentPathSync(repoModel.repo_id, parentPath, TransferDataSource.FOLDER_BACKUP);

        // local db
        Map<String, FileBackupStatusEntity> dbTransferMap;
        if (!CollectionUtils.isEmpty(dbList)) {
            dbTransferMap = dbList.stream().collect(Collectors.toMap(FileBackupStatusEntity::getFullPathFileName, Function.identity()));
        } else {
            dbTransferMap = new IdentityHashMap<>();
        }

        // remote dirent list
        List<DirentRecursiveFileModel> remoteList = getDirentWrapper(repoModel.repo_id, parentPath);

        for (File localFile : localFiles) {

            // backupPath is /storage/emulated/0/aaa/bbb/
            // localFile is  /storage/emulated/0/aaa/bbb/ccc.docx
            // parentAbsPath is /storage/emulated/0/aaa/
            // parentPath is /bbb/
            // fullPathFileName = /bbb/ccc.docx
            String fullPathFileName = StringUtils.removeStart(localFile.getAbsolutePath(), parentAbsPath);
            fullPathFileName = Utils.pathJoin("/", fullPathFileName);

            FileBackupStatusEntity dbTransferEntity = dbTransferMap.getOrDefault(fullPathFileName, null);
            if (dbTransferEntity != null) {
                SLogs.d(TAG, "compare()", "folder backup: skip file: " + fullPathFileName + ", because it has been uploaded");
                continue;
            }

            if (!CollectionUtils.isEmpty(remoteList)) {
                String filename = localFile.getName();
                String prefix, suffix;
                if (filename.contains(".")) {
                    prefix = filename.substring(0, filename.lastIndexOf("."));
                    suffix = filename.substring(filename.lastIndexOf("."));
                } else {
                    prefix = filename;
                    suffix = "";
                }

                String fileParentPath = fullPathFileName.replace(filename, "");
                if (!fileParentPath.startsWith("/")) {
                    fileParentPath = "/" + fileParentPath;
                }
                if (!fileParentPath.endsWith("/")) {
                    fileParentPath = fileParentPath + "/";
                }

                Pattern pattern = getPattern(prefix, suffix);

                String finalFileParentPath = fileParentPath;
                Optional<DirentRecursiveFileModel> firstOp = remoteList.stream()
                        .filter(f -> pattern.matcher(f.name).matches() && TextUtils.equals(f.getParent_dir(), finalFileParentPath))
                        .findFirst();

                if (firstOp.isPresent()) {
                    //skip: the document with the same name exists in the remote repository
                    SLogs.d(TAG, "compare()", "folder backup: skip file: " + fullPathFileName + ", because the same name exists remotely");
                    continue;
                }
            }

            SLogs.d(TAG, "compare()", "folder backup: new file: " + localFile.getAbsolutePath());
            TransferModel transferModel = TransferModel.convert(localFile, backupPath);
            transferModel.related_account = account.getSignature();
            transferModel.repo_id = repoConfig.getRepoId();
            transferModel.repo_name = repoConfig.getRepoName();
            transferModel.data_source = TransferDataSource.FOLDER_BACKUP;
            transferModel.save_to = SaveTo.DB;
            transferModel.setId(transferModel.genStableId());
            GlobalTransferCacheList.FOLDER_BACKUP_QUEUE.put(transferModel);
        }
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
