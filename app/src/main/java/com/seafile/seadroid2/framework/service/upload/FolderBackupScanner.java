package com.seafile.seadroid2.framework.service.upload;

import android.content.Context;
import android.text.TextUtils;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.NetworkUtils;
import com.google.common.base.Stopwatch;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.enums.FeatureDataSource;
import com.seafile.seadroid2.enums.SaveTo;
import com.seafile.seadroid2.enums.TransferDataSource;
import com.seafile.seadroid2.enums.TransferResult;
import com.seafile.seadroid2.framework.datastore.StorageManager;
import com.seafile.seadroid2.framework.datastore.sp_livedata.FolderBackupSharePreferenceHelper;
import com.seafile.seadroid2.framework.db.AppDatabase;
import com.seafile.seadroid2.framework.db.entities.FileBackupStatusEntity;
import com.seafile.seadroid2.framework.db.entities.RepoModel;
import com.seafile.seadroid2.framework.http.HttpIO;
import com.seafile.seadroid2.framework.model.dirents.DirentRecursiveFileModel;
import com.seafile.seadroid2.framework.service.ParentEventTransfer;
import com.seafile.seadroid2.framework.util.SafeLogs;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.framework.worker.GlobalTransferCacheList;
import com.seafile.seadroid2.framework.worker.TransferEvent;
import com.seafile.seadroid2.framework.worker.queue.TransferModel;
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
import java.util.concurrent.TimeUnit;
import java.util.function.Function;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import retrofit2.Call;

public class FolderBackupScanner extends ParentEventTransfer {
    private final String TAG = "FolderBackupScanner";

    public FolderBackupScanner(Context context) {
        super(context);
    }

    protected SeafException returnSuccess() {
        send(FeatureDataSource.FOLDER_BACKUP, TransferEvent.EVENT_SCAN_COMPLETE);
        return SeafException.SUCCESS;
    }

    public SeafException scan(boolean isForce) {
        SafeLogs.d(TAG, "started execution");
        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        if (account == null) {
            return returnSuccess();
        }

        boolean isTurnOn = FolderBackupSharePreferenceHelper.readBackupSwitch();
        if (!isTurnOn) {
            SafeLogs.d(TAG, "The folder scan task was not started, because the switch is off");
            return returnSuccess();
        }

        RepoConfig repoConfig = FolderBackupSharePreferenceHelper.readRepoConfig();
        if (repoConfig == null || StringUtils.isEmpty(repoConfig.getRepoId())) {
            SafeLogs.d(TAG, "The folder scan task was not started, because the repo is not selected");
            return returnSuccess();
        }

        List<String> backupPaths = FolderBackupSharePreferenceHelper.readBackupPathsAsList();
        if (CollectionUtils.isEmpty(backupPaths)) {
            SafeLogs.d(TAG, "The folder scan task was not started, because the folder path is not selected");
            return returnSuccess();
        }

        if (!NetworkUtils.isConnected()) {
            SafeLogs.d(TAG, "network is not connected");
            return returnSuccess();
        }

        boolean isAllowDataPlan = FolderBackupSharePreferenceHelper.readDataPlanAllowed();
        if (!isAllowDataPlan) {
            if (NetworkUtils.isMobileData()) {
                SafeLogs.d(TAG, "data plan is not allowed", "current network type: ", NetworkUtils.getNetworkType().name());
                return returnSuccess();
            }

            SafeLogs.d(TAG, "data plan is not allowed", "current network type: ", NetworkUtils.getNetworkType().name());
        } else {
            SafeLogs.d(TAG, "data plan is allowed", "current network type: ", NetworkUtils.getNetworkType().name());
        }

        if (isForce) {
            FolderBackupSharePreferenceHelper.resetLastScanTime();
        }

        //send a scan event
        send(FeatureDataSource.FOLDER_BACKUP, TransferEvent.EVENT_SCANNING);

        //scan
        SeafException seafException = traverseBackupPath(backupPaths, account, repoConfig);

        if (seafException != SeafException.SUCCESS) {
            SafeLogs.e(TAG, "scan failed");
            send(FeatureDataSource.FOLDER_BACKUP, TransferEvent.EVENT_SCAN_COMPLETE);
            return seafException;
        }

        int totalPendingCount = GlobalTransferCacheList.FOLDER_BACKUP_QUEUE.getPendingCount();
        String content = null;
        if (totalPendingCount > 0) {
            boolean isAllowUpload = checkNetworkTypeIfAllowStartUploadWorker();
            if (!isAllowUpload) {
                content = TransferResult.WAITING.name();
            }
        }

        sendCompleteEvent(FeatureDataSource.FOLDER_BACKUP, content, totalPendingCount);
        return SeafException.SUCCESS;
    }

    protected boolean checkNetworkTypeIfAllowStartUploadWorker() {
        boolean isAllowData = FolderBackupSharePreferenceHelper.readDataPlanAllowed();
        if (isAllowData) {
            return true;
        }

        return !NetworkUtils.isMobileData();
    }


    private List<DirentRecursiveFileModel> getDirentWrapper(String repoId, String parentPath) throws SeafException {
        //get parent dirent list from remote
        retrofit2.Response<List<DirentRecursiveFileModel>> res;
        try {
            Call<List<DirentRecursiveFileModel>> direntWrapperModelCall = HttpIO.getCurrentInstance().execute(RepoService.class).getDirRecursiveFileCall(repoId, parentPath);
            res = direntWrapperModelCall.execute();
        } catch (IOException ioException) {
            SafeLogs.e(TAG, "getDirentWrapper(): " + ioException.getMessage());
            throw SeafException.NETWORK_EXCEPTION;
        }

        if (!res.isSuccessful()) {
            SafeLogs.d(TAG, "request dirents failed");
            if (res.code() == 404) {
                SafeLogs.d(TAG, "request dirents failed, 404");
                return null;
            }
            throw SeafException.NETWORK_EXCEPTION;
        }

        List<DirentRecursiveFileModel> tempWrapperList = res.body();
        if (tempWrapperList == null) {
            SafeLogs.d(TAG, "request dirents is null");
            throw SeafException.NETWORK_EXCEPTION;
        }

        return tempWrapperList;
    }

    private SeafException traverseBackupPath(List<String> backupPathsList, Account account, RepoConfig repoConfig) {

        List<RepoModel> repoModels = AppDatabase.getInstance().repoDao().getByIdSync(repoConfig.getRepoId());

        if (CollectionUtils.isEmpty(repoModels)) {
            return SeafException.SUCCESS;
        }

        //skip folder: /storage/emulated/0/Android/media/com.seafile.seadroid2(.debug)/
        String ignorePath = StorageManager.getInstance().getMediaDir().getAbsolutePath();
        ignorePath = Utils.getParentPath(ignorePath);

        RepoModel repoModel = repoModels.get(0);

        try {
            long lastTime = FolderBackupSharePreferenceHelper.readLastScanTime();

            Stopwatch stopwatch = Stopwatch.createStarted();
            for (String backupPath : backupPathsList) {

                backupPath = Utils.pathJoin("/", backupPath, "/");

                compare(repoConfig, account, repoModel, backupPath, lastTime, ignorePath);
                SafeLogs.d(TAG, "traverseBackupPath()", backupPath);
            }

            SafeLogs.e(TAG, "traverseBackupPath()", "need to upload files count: " + GlobalTransferCacheList.FOLDER_BACKUP_QUEUE.getTotalCount());

            stopwatch.stop();
            long diff = stopwatch.elapsed(TimeUnit.MILLISECONDS);
            SafeLogs.d(TAG, "traverseBackupPath()", "folder backup scan time：" + stopwatch);
            long now = System.currentTimeMillis();
            FolderBackupSharePreferenceHelper.writeLastScanTime(now - diff);

            return SeafException.SUCCESS;
        } catch (SeafException seafException) {
            SafeLogs.e(TAG, "traverseBackupPath(): " + seafException);
            return seafException;
        }
    }

    private void compare(RepoConfig repoConfig, Account account, RepoModel repoModel, String backupPath, long lastScanTime, String ignorePath) throws SeafException {

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
                SafeLogs.d(TAG, "compare()", "folder backup: skip file: " + fullPathFileName + ", because it has been uploaded");
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
                    SafeLogs.d(TAG, "compare()", "folder backup: skip file: " + fullPathFileName + ", because the same name exists remotely");
                    continue;
                }
            }

            SafeLogs.d(TAG, "compare()", "folder backup: new file: " + localFile.getAbsolutePath());
            TransferModel transferModel = TransferModel.convert(localFile, backupPath);
            transferModel.related_account = account.getSignature();
            transferModel.repo_id = repoConfig.getRepoId();
            transferModel.repo_name = repoConfig.getRepoName();
            transferModel.data_source = FeatureDataSource.FOLDER_BACKUP;
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
