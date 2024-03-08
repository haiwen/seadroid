package com.seafile.seadroid2.worker;

import android.annotation.SuppressLint;
import android.content.Context;
import android.text.TextUtils;
import android.webkit.MimeTypeMap;

import androidx.annotation.NonNull;
import androidx.room.RoomDatabase;
import androidx.work.WorkerParameters;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.FileUtils;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.data.db.AppDatabase;
import com.seafile.seadroid2.data.db.entities.RepoModel;
import com.seafile.seadroid2.data.db.entities.FileTransferEntity;
import com.seafile.seadroid2.data.model.enums.TransferAction;
import com.seafile.seadroid2.data.model.enums.TransferFeature;
import com.seafile.seadroid2.data.model.enums.TransferResult;
import com.seafile.seadroid2.data.model.enums.TransferStatus;
import com.seafile.seadroid2.ui.folder_backup.RepoConfig;
import com.seafile.seadroid2.ui.selector.folder_selector.StringTools;
import com.seafile.seadroid2.util.FileTools;
import com.seafile.seadroid2.util.sp.FolderBackupConfigSPs;
import com.seafile.seadroid2.util.sp.SettingsManager;

import java.io.File;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

public class UploadFileSyncWorker extends BaseWorker {
    public UploadFileSyncWorker(@NonNull Context context, @NonNull WorkerParameters workerParams) {
        super(context, workerParams);
    }

    @NonNull
    @Override
    public Result doWork() {
        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        if (account == null) {
            return Result.success();
        }

        String backupPaths = FolderBackupConfigSPs.getBackupPathsByCurrentAccount();
        RepoConfig repoConfig = FolderBackupConfigSPs.getBackupConfigByAccount(account.getSignature());

        if (TextUtils.isEmpty(backupPaths) || repoConfig == null) {
            return Result.success();
        }

        //
        insertWorkIntoDb(account, repoConfig, backupPaths);

        //start upload worker
        BackgroundJobManagerImpl.getInstance().startFilesUploadJob();

        return Result.success();
    }

    private void insertWorkIntoDb(Account account, RepoConfig repoConfig, String backupPaths) {
        RepoModel repoModel = AppDatabase.getInstance().repoDao().getOneByIdSync(repoConfig.getRepoID());
        if (repoModel == null) {
            return;
        }

        List<String> backupPathsList = StringTools.getJsonToList(backupPaths);

        //
        List<File> files = traverseFiles(backupPathsList);

        List<FileTransferEntity> transferEntityList = CollectionUtils.newArrayList();
        boolean isJumpHiddenFile = SettingsManager.getInstance().isFolderBackupJumpHiddenFiles();
        for (File file : files) {
            if (isJumpHiddenFile && file.isHidden()) {
                continue;
            }

            long now = System.currentTimeMillis();

            FileTransferEntity entity = new FileTransferEntity();
            entity.full_path = file.getAbsolutePath();
            entity.parent_path = FileTools.getParentPath(entity.full_path);

            for (String backupPath : backupPathsList) {

            }

            // /storage/emulated/0/Downloads
            // /storage/emulated/0/Downloads/common/Maid.js
            // /storage/emulated/0/Downloads/autojs-master/test.js

            // /storage/emulated/0/digu/daxb


            //todo 不是target_path路径都是父目录
            String[] split = entity.parent_path.split("/");
            entity.target_path = "/"+split[split.length - 1]; //the last dir name of parent_path

            entity.file_name = file.getName();
            entity.file_size = file.length();
            entity.file_format = FileUtils.getFileExtension(entity.full_path);
            entity.file_md5 = FileUtils.getFileMD5ToString(entity.full_path).toLowerCase();
            entity.mime_type = MimeTypeMap.getSingleton().getMimeTypeFromExtension(entity.file_format);

            entity.is_block = repoModel.encrypted;
            entity.repo_id = repoModel.repo_id;
            entity.repo_name = repoModel.repo_name;
            entity.related_account = account.getSignature();
            entity.data_source = TransferFeature.FOLDER_BACKUP;
            entity.created_at = now;
            entity.modified_at = file.lastModified();
            entity.action_end_at = 0;
            entity.is_update = false;
            entity.is_copy_to_local = false;
            entity.transfer_action = TransferAction.UPLOAD;
            entity.transfer_result = TransferResult.NO_RESULT;
            entity.transfer_status = TransferStatus.TRANSFER_WAITING;

            entity.uid = entity.getUID();

            transferEntityList.add(entity);
        }

        List<String> fullPaths = transferEntityList.stream().map(m -> m.full_path).collect(Collectors.toList());
        List<FileTransferEntity> existsList = AppDatabase.getInstance().fileTransferDAO().getListByFullPaths(account.getSignature(), fullPaths, TransferAction.UPLOAD);

        List<FileTransferEntity> newList = CollectionUtils.newArrayList();

        //compare
        if (!CollectionUtils.isEmpty(existsList)) {
            for (FileTransferEntity transferEntity : transferEntityList) {
                Optional<FileTransferEntity> optional = existsList.stream().filter(f -> TextUtils.equals(f.full_path, transferEntity.full_path)).findFirst();
                if (optional.isPresent()) {
                    FileTransferEntity dbEntity = optional.get();
                    //Whether it's the same file
                    if (TextUtils.equals(dbEntity.file_md5, transferEntity.file_md5)) {
                        //it's the same file，do not insert into db.
                    } else {
                        transferEntity.transfer_action = TransferAction.UPLOAD;
                        transferEntity.transfer_result = TransferResult.NO_RESULT;
                        transferEntity.transfer_status = TransferStatus.TRANSFER_WAITING;
                        transferEntity.is_update = true;
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
    }

    @SuppressLint("RestrictedApi")
    private List<FileTransferEntity> getList(List<FileTransferEntity> transferEntityList) {
        List<FileTransferEntity> existsList = CollectionUtils.newArrayList();

        if (transferEntityList.size() > RoomDatabase.MAX_BIND_PARAMETER_CNT) {

        }
        return existsList;
    }

    private List<File> traverseFiles(List<String> backupPathsList) {
        Deque<File> stack = new ArrayDeque<>();

        for (String path : backupPathsList) {
            stack.push(new File(path));
        }

        List<File> filePathList = new ArrayList<>();

        while (!stack.isEmpty()) {
            File currentDir = stack.pop();
            File[] files = currentDir.listFiles();

            if (files != null) {
                for (File file : files) {
                    if (file.isDirectory()) {
                        stack.push(file);
                    } else {
                        filePathList.add(file);
                    }
                }
            }
        }
        return filePathList;
    }
}
