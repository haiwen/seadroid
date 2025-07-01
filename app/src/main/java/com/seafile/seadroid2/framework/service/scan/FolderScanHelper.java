package com.seafile.seadroid2.framework.service.scan;

import android.text.TextUtils;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.NetworkUtils;
import com.google.common.base.Stopwatch;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.enums.FeatureDataSource;
import com.seafile.seadroid2.enums.SaveTo;
import com.seafile.seadroid2.enums.TransferDataSource;
import com.seafile.seadroid2.framework.datastore.StorageManager;
import com.seafile.seadroid2.framework.datastore.sp_livedata.FolderBackupSharePreferenceHelper;
import com.seafile.seadroid2.framework.db.AppDatabase;
import com.seafile.seadroid2.framework.db.entities.FileBackupStatusEntity;
import com.seafile.seadroid2.framework.db.entities.RepoModel;
import com.seafile.seadroid2.framework.http.HttpIO;
import com.seafile.seadroid2.framework.model.dirents.DirentRecursiveFileModel;
import com.seafile.seadroid2.framework.service.TransferService;
import com.seafile.seadroid2.framework.util.SafeLogs;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.framework.worker.GlobalTransferCacheList;
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
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;
import java.util.function.Function;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import retrofit2.Call;
import retrofit2.Response;

public class FolderScanHelper {
    public static final String TAG = "FolderScanHelper";

    public static boolean checkNetworkTypeIfAllowStartUpload() {
        boolean isAllowData = FolderBackupSharePreferenceHelper.readDataPlanAllowed();
        if (isAllowData) {
            return true;
        }

        return !NetworkUtils.isMobileData();
    }

    private static List<DirentRecursiveFileModel> getDirentWrapper(String repoId, String parentPath) throws SeafException {
        //get parent dirent list from remote
        Response<List<DirentRecursiveFileModel>> res;
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

    public static int traverseBackupPathFileCount(List<String> backupPathsList, Account account, RepoConfig repoConfig) {
        int backupableCount = 0;
        List<RepoModel> repoModels = AppDatabase.getInstance().repoDao().getByIdSync(repoConfig.getRepoId());

        if (CollectionUtils.isEmpty(repoModels)) {
            return backupableCount;
        }

        //skip folder: /storage/emulated/0/Android/media/com.seafile.seadroid2(.debug)/
        String ignorePath = StorageManager.getInstance().getMediaDir().getAbsolutePath();
        ignorePath = Utils.getParentPath(ignorePath);

        RepoModel repoModel = repoModels.get(0);
        Stopwatch stopwatch = null;
        try {
            long lastTime = 0L;
            stopwatch = Stopwatch.createStarted();

            for (String backupPath : backupPathsList) {
                backupPath = Utils.pathJoin("/", backupPath, "/");

                int c = compareCount(repoConfig, account, repoModel, backupPath, lastTime, ignorePath);
                backupableCount += c;
                SafeLogs.d(TAG, "traverseBackupPath()", backupPath);
            }

            SafeLogs.e(TAG, "traverseBackupPath()", "need to upload files count: " + GlobalTransferCacheList.FOLDER_BACKUP_QUEUE.getTotalCount());

        } catch (SeafException seafException) {
            SafeLogs.e(TAG, "traverseBackupPath(): " + seafException);
        } finally {
            if (stopwatch != null) {
                stopwatch.stop();
                long diff = stopwatch.elapsed(TimeUnit.MILLISECONDS);
                SafeLogs.d(TAG, "traverseBackupPath()", "folder backup scan time：" + stopwatch);
                long now = System.currentTimeMillis();
                FolderBackupSharePreferenceHelper.writeLastScanTime(now - diff);
            }
        }
        return backupableCount;
    }

    public static SeafException traverseBackupPath(List<String> backupPathsList, Account account, RepoConfig repoConfig) {

        List<RepoModel> repoModels = AppDatabase.getInstance().repoDao().getByIdSync(repoConfig.getRepoId());

        if (CollectionUtils.isEmpty(repoModels)) {
            return SeafException.SUCCESS;
        }

        //skip folder: /storage/emulated/0/Android/media/com.seafile.seadroid2(.debug)/
        String ignorePath = StorageManager.getInstance().getMediaDir().getAbsolutePath();
        ignorePath = Utils.getParentPath(ignorePath);

        RepoModel repoModel = repoModels.get(0);


        SeafException retException;
        try {
            long lastTime = 0L; // scan all files in the folder

            for (String backupPath : backupPathsList) {

                backupPath = Utils.pathJoin("/", backupPath, "/");

                compare(repoConfig, account, repoModel, backupPath, lastTime, ignorePath);
                SafeLogs.d(TAG, "traverseBackupPath()", backupPath);
            }

            SafeLogs.e(TAG, "traverseBackupPath()", "need to upload files count: " + GlobalTransferCacheList.FOLDER_BACKUP_QUEUE.getTotalCount());

            retException = SeafException.SUCCESS;
        } catch (SeafException seafException) {
            SafeLogs.e(TAG, "traverseBackupPath(): " + seafException);
            retException = seafException;
        }
        return retException;
    }

    private static int compareCount(RepoConfig repoConfig, Account account, RepoModel repoModel, String backupPath, long lastScanTime, String ignorePath) throws SeafException {
        int backupableCount = 0;

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
            return backupableCount;
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
                SafeLogs.d(TAG, "compareCount()", "folder backup: skip file: " + fullPathFileName + ", because it has been uploaded");
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
                    SafeLogs.d(TAG, "compareCount()", "folder backup: skip file: " + fullPathFileName + ", because the same name exists remotely");
                    continue;
                }
            }

            backupableCount++;

            // As long as there is one file that needs to be uploaded, the folder is considered to be backupable
            break;
        }

        return backupableCount;
    }

    private static void compare(RepoConfig repoConfig, Account account, RepoModel repoModel, String backupPath, long lastScanTime, String ignorePath) throws SeafException {

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

    private static Pattern getPattern(String prefix, String suffix) {
        if (TextUtils.isEmpty(prefix)) {
            // file format is '.txt'
        }

        if (TextUtils.isEmpty(suffix)) {
            // file format is 'a'
            suffix = "";
        }

        return Pattern.compile(Pattern.quote(prefix) + "( \\(\\d+\\))?" + Pattern.quote(suffix));
    }

    private static List<File> traverseFiles(String path, long lastScanTime, String ignorePath) {
        File pathFile = new File(path);
        if (!pathFile.exists() || !pathFile.canRead()) {
            SafeLogs.d(TAG, "traverseFiles(): " + path + " is not exist or can not read");
            return new ArrayList<>();
        }

        Deque<File> stack = new ArrayDeque<>();
        stack.push(pathFile);

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

    private static void processFile(File file, List<File> filePathList, String ignorePath, long lastScanTime) {
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
