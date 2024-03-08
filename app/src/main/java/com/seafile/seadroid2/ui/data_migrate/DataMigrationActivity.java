package com.seafile.seadroid2.ui.data_migrate;

import android.database.Cursor;
import android.database.sqlite.SQLiteDatabase;
import android.os.Bundle;
import android.view.KeyEvent;
import android.webkit.MimeTypeMap;

import androidx.appcompat.app.AppCompatActivity;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.FileUtils;
import com.blankj.utilcode.util.ToastUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.account.SupportDataManager;
import com.seafile.seadroid2.data.DatabaseHelper;
import com.seafile.seadroid2.data.SeafRepo;
import com.seafile.seadroid2.data.StorageManager;
import com.seafile.seadroid2.data.db.AppDatabase;
import com.seafile.seadroid2.data.db.entities.CertEntity;
import com.seafile.seadroid2.data.db.entities.EncKeyCacheEntity;
import com.seafile.seadroid2.data.db.entities.FileTransferEntity;
import com.seafile.seadroid2.data.db.entities.FolderBackupMonitorEntity;
import com.seafile.seadroid2.data.db.entities.RepoDirMappingEntity;
import com.seafile.seadroid2.data.db.entities.RepoModel;
import com.seafile.seadroid2.data.model.enums.TransferAction;
import com.seafile.seadroid2.data.model.enums.TransferFeature;
import com.seafile.seadroid2.data.model.enums.TransferResult;
import com.seafile.seadroid2.data.model.enums.TransferStatus;
import com.seafile.seadroid2.data.model.repo.RepoWrapperModel;
import com.seafile.seadroid2.databinding.ActivityDataMigrationBinding;
import com.seafile.seadroid2.io.http.IO;
import com.seafile.seadroid2.monitor.MonitorDBHelper;
import com.seafile.seadroid2.ssl.CertsDBHelper;
import com.seafile.seadroid2.ui.camera_upload.CameraUploadDBHelper;
import com.seafile.seadroid2.ui.folder_backup.FolderBackupDBHelper;
import com.seafile.seadroid2.ui.folder_backup.RepoConfig;
import com.seafile.seadroid2.ui.repo.RepoService;
import com.seafile.seadroid2.util.SLogs;
import com.seafile.seadroid2.util.Utils;
import com.seafile.seadroid2.util.sp.FolderBackupConfigSPs;
import com.seafile.seadroid2.util.sp.SPs;
import com.seafile.seadroid2.util.sp.SettingsManager;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Consumer;

import io.reactivex.Completable;
import io.reactivex.android.schedulers.AndroidSchedulers;
import io.reactivex.disposables.CompositeDisposable;
import io.reactivex.functions.Action;
import io.reactivex.schedulers.Schedulers;
import retrofit2.Response;

/**
 * Migrating data from sqlite database to Room
 */
public class DataMigrationActivity extends AppCompatActivity {
    private ActivityDataMigrationBinding binding;
    private final CompositeDisposable compositeDisposable = new CompositeDisposable();


    private final List<RepoModel> accountRepoList = new ArrayList<>();

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        binding = ActivityDataMigrationBinding.inflate(getLayoutInflater());
        setContentView(binding.getRoot());

        binding.text.setText(R.string.wait);

        List<Account> accounts = SupportAccountManager.getInstance().getAccountList();
        if (CollectionUtils.isEmpty(accounts)) {
            ToastUtils.showLong("当前用户未登录");
            return;
        }

        new Thread(new Runnable() {
            @Override
            public void run() {

                try {
                    syncAccount();

                    startMigration();
                } catch (IOException e) {
                    throw new RuntimeException(e);
                }
            }
        }).start();
    }

    private void syncAccount() throws IOException {
        List<Account> accounts = SupportAccountManager.getInstance().getAccountList();
        if (CollectionUtils.isEmpty(accounts)) {
            return;
        }

        for (Account account : accounts) {
            Response<RepoWrapperModel> response = IO.getNewInstance(account.server, account.token)
                    .execute(RepoService.class)
                    .getReposCall()
                    .execute();
            if (!response.isSuccessful()) {
                return;
            }

            RepoWrapperModel wrapperModel = response.body();
            if (CollectionUtils.isEmpty(wrapperModel.repos)) {
                return;
            }

            for (RepoModel repoModel : wrapperModel.repos) {
                repoModel.related_account = account.getSignature();
            }

            accountRepoList.addAll(wrapperModel.repos);
        }

        if (CollectionUtils.isEmpty(accountRepoList)) {
            return;
        }

        AppDatabase.getInstance().repoDao().insertAllSync(accountRepoList);

    }

    //TODO 应当让用户选择某个账户

    private void startMigration() {
        if (accountRepoList.isEmpty()) {
            //TODO 空了？
            return;
        }

        //
//        queryRepoFileDB();


        queryPhotoDB();

        //folder backup
        queryFolderBackupDB();
        queryRepoConfigOfFolderBackDB();
        queryFolderBackPathsOfFolderBack();

        queryDataDB();
        queryMonitorDB();
        queryCertsDB();

        finishMigration();
    }

    private boolean checkTableExists(SQLiteDatabase database, String tableName) {
        String rawQuery = "SELECT name FROM sqlite_master WHERE type='table' AND name='" + tableName + "';";
        Cursor cursor = database.rawQuery(rawQuery, null);
        boolean isExists = false;
        if (cursor.moveToFirst()) {
            isExists = true;
        } else {
            //no PhotoCache table
        }
        cursor.close();
        return isExists;
    }

    private void queryRepoFileDB() {

        List<SeafRepo> seafRepos = SupportDataManager.getInstance().getDataManager().getReposFromCache();
        if (CollectionUtils.isEmpty(seafRepos)) {
            return;
        }


        List<RepoModel> repoModels = new ArrayList<>();
        for (SeafRepo seafRepo : seafRepos) {
            RepoModel repoModel = new RepoModel();
            repoModel.type = seafRepo.type;
            repoModel.repo_id = seafRepo.repo_id;
            repoModel.repo_name = seafRepo.repo_name;
            repoModel.group_id = seafRepo.group_id;
            repoModel.group_name = seafRepo.group_name;
            repoModel.owner_name = seafRepo.owner_name;
            repoModel.owner_email = seafRepo.owner_email;
            repoModel.owner_contact_email = seafRepo.owner_contact_email;

            Optional<RepoModel> repoModelOp = accountRepoList.stream().filter(repo -> repo.repo_id.equals(seafRepo.repo_id)).findFirst();
            repoModelOp.ifPresent(repoModel1 -> repoModel.related_account = repoModel1.related_account);

            repoModel.modifier_email = seafRepo.modifier_email;
            repoModel.modifier_name = seafRepo.modifier_name;
            repoModel.modifier_contact_email = seafRepo.modifier_contact_email;
            repoModel.last_modified = seafRepo.last_modified;
            repoModel.size = seafRepo.size;
            repoModel.permission = seafRepo.permission;
            repoModel.is_admin = seafRepo.is_admin;
            repoModel.salt = seafRepo.salt;
            repoModel.status = seafRepo.status;
            repoModel.monitored = seafRepo.monitored;
            repoModel.starred = seafRepo.starred;
            repoModel.encrypted = seafRepo.encrypted;
            repoModels.add(repoModel);
        }
        AppDatabase.getInstance().repoDao().insertAll(repoModels);
    }

    /**
     * album backup
     */
    private void queryPhotoDB() {
        String table = "PhotoCache";

        CameraUploadDBHelper dbHelper = CameraUploadDBHelper.getInstance();
        SQLiteDatabase database = dbHelper.getWritableDatabase();
        boolean isExists = checkTableExists(database, table);
        if (!isExists) {
            return;
        }

        Cursor c = database.query(
                table,
                null,
                null,
                null,
                null,   // don't group the rows
                null,   // don't filter by row groups
                null    // The sort order
        );


        List<FileTransferEntity> list = CollectionUtils.newArrayList();

        try {
            c.moveToFirst();


            String targetRepoId = SettingsManager.getInstance().getCameraUploadRepoId();
            String targetRepoName = SettingsManager.getInstance().getCameraUploadRepoName();

            Optional<RepoModel> repoModelOp = accountRepoList.stream().filter(repo -> repo.repo_id.equals(targetRepoId)).findFirst();


            while (!c.isAfterLast()) {
                FileTransferEntity transferEntity = new FileTransferEntity();
                int idIndex = c.getColumnIndexOrThrow("id");
                int dateIndex = c.getColumnIndexOrThrow("date_added");
                int filePathIndex = c.getColumnIndexOrThrow("file");


                String filePath = c.getString(filePathIndex);
                long action_end_time_stamp = c.getLong(dateIndex);

                transferEntity.v = 0;

                if (repoModelOp.isPresent()) {
                    RepoModel backUpRepoModel = repoModelOp.get();
                    transferEntity.repo_id = backUpRepoModel.repo_id;
                    transferEntity.repo_name = backUpRepoModel.repo_name;
                    transferEntity.is_block = backUpRepoModel.encrypted;
                    transferEntity.related_account = backUpRepoModel.related_account;
                } else {
                    transferEntity.is_block = false;
                    transferEntity.repo_id = null;
                    transferEntity.repo_name = null;
                    transferEntity.related_account = null;
                }

                transferEntity.is_copy_to_local = false;
                transferEntity.is_update = false;

                transferEntity.full_path = filePath;
                transferEntity.parent_path = Utils.getParentPath(filePath);
                transferEntity.file_name = FileUtils.getFileName(filePath);
                transferEntity.file_size = FileUtils.getFileLength(filePath);
                transferEntity.transferred_size = transferEntity.file_size;
                transferEntity.file_format = FileUtils.getFileExtension(filePath);
                transferEntity.mime_type = MimeTypeMap.getSingleton().getMimeTypeFromExtension(transferEntity.file_format);
                transferEntity.file_md5 = FileUtils.getFileMD5ToString(transferEntity.full_path);
                transferEntity.created_at = action_end_time_stamp;
                transferEntity.modified_at = action_end_time_stamp;
                transferEntity.action_end_at = action_end_time_stamp;

                transferEntity.data_source = TransferFeature.ALBUM_BACKUP;
                transferEntity.transfer_action = TransferAction.UPLOAD;
                transferEntity.transfer_status = TransferStatus.TRANSFER_SUCCEEDED;
                transferEntity.transfer_result = TransferResult.TRANSMITTED;

                transferEntity.uid = transferEntity.getUID();

                c.moveToNext();

                list.add(transferEntity);
            }
        } finally {
            c.close();
        }

        SLogs.d("--------------------" + table);
        for (FileTransferEntity entity : list) {
            SLogs.d(entity.toString());
        }

        AppDatabase.getInstance().fileTransferDAO().insertAll(list);
        SLogs.d("--------------------" + table + " -> 完成");
    }

    /**
     * folder backup
     */
    private void queryFolderBackupDB() {
        //FolderBackupInfo
        String table = "FolderBackupInfo";

        FolderBackupDBHelper dbHelper = FolderBackupDBHelper.getDatabaseHelper();
        SQLiteDatabase database = dbHelper.getWritableDatabase();
        boolean isExists = checkTableExists(database, table);
        if (!isExists) {
            return;
        }

        Cursor c = database.query(
                table,
                null,
                null,
                null,
                null,   // don't group the rows
                null,   // don't filter by row groups
                null    // The sort order
        );

        List<FileTransferEntity> list = CollectionUtils.newArrayList();

        try {
            c.moveToFirst();

            while (!c.isAfterLast()) {

                FileTransferEntity transferEntity = new FileTransferEntity();
                int idIndex = c.getColumnIndexOrThrow("id");
                int repoIdIndex = c.getColumnIndexOrThrow("repo_id");
                int repoNameIndex = c.getColumnIndexOrThrow("repo_name");
                int parentFolderIndex = c.getColumnIndexOrThrow("parent_folder");
                int fileNameIndex = c.getColumnIndexOrThrow("file_name");
                int filePathIndex = c.getColumnIndexOrThrow("file_path");
                int fileSizeIndex = c.getColumnIndexOrThrow("file_size");

                transferEntity.v = 0;
                transferEntity.repo_id = c.getString(repoIdIndex);
                transferEntity.repo_name = c.getString(repoNameIndex);

                Optional<RepoModel> repoModelOp = accountRepoList.stream().filter(repo -> repo.repo_id.equals(transferEntity.repo_id)).findFirst();
                if (repoModelOp.isPresent()) {
                    RepoModel rm = repoModelOp.get();
                    transferEntity.is_block = rm.encrypted;
                    transferEntity.related_account = rm.related_account;
                }


                transferEntity.is_copy_to_local = false;
                transferEntity.is_update = false;

                transferEntity.full_path = c.getString(filePathIndex);
                //TODO 检查是否已斜杠开头
                transferEntity.parent_path = c.getString(parentFolderIndex);
                transferEntity.file_name = c.getString(fileNameIndex);
                transferEntity.file_size = Long.parseLong(c.getString(fileSizeIndex));
                transferEntity.transferred_size = transferEntity.file_size;
                transferEntity.file_format = FileUtils.getFileExtension(transferEntity.full_path);
                transferEntity.mime_type = MimeTypeMap.getSingleton().getMimeTypeFromExtension(transferEntity.file_format);
                transferEntity.file_md5 = FileUtils.getFileMD5ToString(transferEntity.full_path);

                long nowMills = System.currentTimeMillis();
                transferEntity.created_at = nowMills;
                transferEntity.modified_at = nowMills;
                transferEntity.action_end_at = nowMills;

                transferEntity.data_source = TransferFeature.FOLDER_BACKUP;
                transferEntity.transfer_action = TransferAction.UPLOAD;
                transferEntity.transfer_result = TransferResult.TRANSMITTED;
                transferEntity.transfer_status = TransferStatus.TRANSFER_SUCCEEDED;

                transferEntity.uid = transferEntity.getUID();

                c.moveToNext();

                list.add(transferEntity);
            }
        } finally {
            c.close();
        }

        SLogs.d("--------------------" + table);

        for (FileTransferEntity entity : list) {
            SLogs.d(entity.toString());
        }

        AppDatabase.getInstance().fileTransferDAO().insertAll(list);
        SLogs.d("--------------------" + table + " -> 完成");
    }

    private void queryRepoConfigOfFolderBackDB() {
        String table = "RepoConfig";

        FolderBackupDBHelper dbHelper = FolderBackupDBHelper.getDatabaseHelper();
        SQLiteDatabase database = dbHelper.getWritableDatabase();
        boolean isRepoConfigExists = checkTableExists(database, table);
        if (!isRepoConfigExists) {
            return;
        }

        Cursor c = database.query(
                table,
                null,
                null,
                null,
                null,   // don't group the rows
                null,   // don't filter by row groups
                null    // The sort order
        );

        try {
            c.moveToFirst();
            while (!c.isAfterLast()) {
                int idIndex = c.getColumnIndexOrThrow("id");

                //f4f550ea33e14f82aab7da71be0d13fa@auth.local
                int emailIndex = c.getColumnIndexOrThrow("mail");//this field is not 'email'
                int repoIdIndex = c.getColumnIndexOrThrow("repo_id");
                int repoNameIndex = c.getColumnIndexOrThrow("repo_name");

                String repo_id = c.getString(repoIdIndex);
                String repo_name = c.getString(repoNameIndex);
                String related_account = c.getString(emailIndex);

                //TODO 找当当前用户的 signature
                RepoConfig repoConfig = new RepoConfig(repo_id, repo_name, related_account, related_account);
                FolderBackupConfigSPs.setBackupRepoConfig(repoConfig);

                c.moveToNext();
            }
        } finally {
            c.close();
        }
    }

    private void queryFolderBackPathsOfFolderBack() {
        String backupPaths = FolderBackupConfigSPs.getBackupPaths();
        FolderBackupConfigSPs.saveBackupPathsByCurrentAccount(backupPaths);
        SPs.remove(FolderBackupConfigSPs.FOLDER_BACKUP_PATHS);
    }

    private void queryDataDB() {
        //FileCache
        //StarredFileCache
        //RepoDir
        //DirentsCache
        //EncKey

        queryFileCacheOfDataDB();
        queryStarredFileCacheOfDataDB();
        queryRepoDirOfDataDB();
        queryDirentsCacheOfDataDB();
        queryEncKeyOfDataDB();
    }


    /**
     * file cache
     */
    private void queryFileCacheOfDataDB() {
        String table = "FileCache";

        DatabaseHelper dbHelper = DatabaseHelper.getDatabaseHelper();
        SQLiteDatabase database = dbHelper.getWritableDatabase();
        boolean isExists = checkTableExists(database, table);
        if (!isExists) {
            return;
        }

        Cursor c = database.query(
                table,
                null,
                null,
                null,
                null,   // don't group the rows
                null,   // don't filter by row groups
                null    // The sort order
        );

        List<FileTransferEntity> list = CollectionUtils.newArrayList();

        try {
            c.moveToFirst();

            Map<String, RepoModel> repoModelMap = new HashMap<>();

            while (!c.isAfterLast()) {

                FileTransferEntity transferEntity = new FileTransferEntity();
                int idIndex = c.getColumnIndexOrThrow("id");
                int fileIdIndex = c.getColumnIndexOrThrow("fileid");
                int pathIndex = c.getColumnIndexOrThrow("path");
                int repoIdIndex = c.getColumnIndexOrThrow("repo_id");
                int repoNameIndex = c.getColumnIndexOrThrow("repo_name");
                int accountIndex = c.getColumnIndexOrThrow("account");


                transferEntity.v = 0;
                transferEntity.repo_id = c.getString(repoIdIndex);
                transferEntity.repo_name = c.getString(repoNameIndex);
                transferEntity.file_id = c.getString(fileIdIndex);

                Optional<RepoModel> repoModelOp = accountRepoList.stream().filter(repo -> repo.repo_id.equals(transferEntity.repo_id)).findFirst();
                if (repoModelOp.isPresent()) {
                    RepoModel rm = repoModelOp.get();
                    transferEntity.is_block = rm.encrypted;
                    transferEntity.related_account = rm.related_account;
                }

                transferEntity.is_copy_to_local = false;
                transferEntity.is_update = false;

                String path = c.getString(pathIndex);

                transferEntity.transfer_result = TransferResult.TRANSMITTED;
                transferEntity.transfer_status = TransferStatus.TRANSFER_SUCCEEDED;
                if (path.startsWith("/")) {
                    transferEntity.transfer_action = TransferAction.DOWNLOAD;
                    transferEntity.data_source = TransferFeature.DOWNLOAD;
                    transferEntity.full_path = path;
                } else {
                    transferEntity.full_path = "/" + path;
                    transferEntity.transfer_action = TransferAction.UPLOAD;
                    if (path.startsWith("My Photos")) {
                        transferEntity.data_source = TransferFeature.ALBUM_BACKUP;
                    } else {
                        transferEntity.data_source = TransferFeature.FOLDER_BACKUP;
                    }
                }

                transferEntity.parent_path = Utils.getParentPath(transferEntity.full_path);
                transferEntity.file_name = FileUtils.getFileName(transferEntity.full_path);

                if (TransferFeature.DOWNLOAD == transferEntity.data_source) {
                    String mediaPath = StorageManager.getInstance().getMediaDir().getAbsolutePath();
                    String absPath = Utils.pathJoin(mediaPath, transferEntity.related_account, transferEntity.repo_name, path);
                    transferEntity.file_size = FileUtils.getFileLength(absPath);
                    transferEntity.transferred_size = transferEntity.file_size;
                    transferEntity.file_md5 = FileUtils.getFileMD5ToString(absPath);
                    transferEntity.target_path = absPath;
                } else if (TransferFeature.FOLDER_BACKUP == transferEntity.data_source) {
                    //queryFolderBackupDB() 方法已实现数据迁移
                } else if (TransferFeature.ALBUM_BACKUP == transferEntity.data_source) {
                    //My Photos/a/b.png
                    //文件属性数据为空
                }

                transferEntity.file_format = FileUtils.getFileExtension(transferEntity.full_path);
                transferEntity.mime_type = MimeTypeMap.getSingleton().getMimeTypeFromExtension(transferEntity.file_format);

                transferEntity.created_at = 0;
                transferEntity.modified_at = 0;
                transferEntity.action_end_at = 0;


                transferEntity.uid = transferEntity.getUID();

                c.moveToNext();

                list.add(transferEntity);
            }
        } finally {
            c.close();
        }

        SLogs.d("--------------------" + table);
        for (FileTransferEntity entity : list) {
            SLogs.d(entity.toString());
        }

        AppDatabase.getInstance().fileTransferDAO().insertAll(list);
        SLogs.d("--------------------" + table + " -> 完成");

    }

    /**
     *
     */
    private void queryStarredFileCacheOfDataDB() {
        String table = "StarredFileCache";

        DatabaseHelper dbHelper = DatabaseHelper.getDatabaseHelper();
        SQLiteDatabase database = dbHelper.getWritableDatabase();
        boolean isExists = checkTableExists(database, table);
        if (!isExists) {
            return;
        }

//        Cursor c = database.query(
//                table,
//                null,
//                null,
//                null,
//                null,   // don't group the rows
//                null,   // don't filter by row groups
//                null    // The sort order
//        );

//        List<StarredFileCacheEntity> list = CollectionUtils.newArrayList();
//
//        try {
//            c.moveToFirst();
//            while (!c.isAfterLast()) {
//                StarredFileCacheEntity item = new StarredFileCacheEntity();
//                int idIndex = c.getColumnIndexOrThrow("id");
//                int contentIndex = c.getColumnIndexOrThrow("content");
//                int accountIndex = c.getColumnIndexOrThrow("account");
//
//
//                item.id = c.getLong(idIndex);
//                item.content = c.getString(contentIndex);
//                item.related_account = c.getString(accountIndex);
//
//                c.moveToNext();
//
//                list.add(item);
//            }
//        } finally {
//            c.close();
//        }

    }

    private void queryRepoDirOfDataDB() {
        String table = "RepoDir";

        DatabaseHelper dbHelper = DatabaseHelper.getDatabaseHelper();
        SQLiteDatabase database = dbHelper.getWritableDatabase();
        boolean isExists = checkTableExists(database, table);
        if (!isExists) {
            return;
        }

        Cursor c = database.query(
                table,
                null,
                null,
                null,
                null,   // don't group the rows
                null,   // don't filter by row groups
                null    // The sort order
        );

        List<RepoDirMappingEntity> list = CollectionUtils.newArrayList();

        try {
            c.moveToFirst();
            while (!c.isAfterLast()) {
                RepoDirMappingEntity item = new RepoDirMappingEntity();
                int idIndex = c.getColumnIndexOrThrow("id");
                int repoIdIndex = c.getColumnIndexOrThrow("repo_id");
                int repoDirIndex = c.getColumnIndexOrThrow("repo_dir");
                int accountIndex = c.getColumnIndexOrThrow("account");


                item.id = c.getLong(idIndex);
                item.repo_id = c.getString(repoIdIndex);
                item.repo_dir = c.getString(repoDirIndex);
                item.related_account = c.getString(accountIndex);

                c.moveToNext();

                list.add(item);
            }
        } finally {
            c.close();
        }

        SLogs.d("--------------------" + table);
        for (RepoDirMappingEntity entity : list) {
            SLogs.d(entity.toString());
        }

        AppDatabase.getInstance().repoDirMappingDAO().insertAll(list);
        SLogs.d("--------------------" + table + " -> 完成");
    }

    private void queryDirentsCacheOfDataDB() {
        String table = "DirentsCache";

        DatabaseHelper dbHelper = DatabaseHelper.getDatabaseHelper();
        SQLiteDatabase database = dbHelper.getWritableDatabase();
        boolean isExists = checkTableExists(database, table);
        if (!isExists) {
            return;
        }

//        Cursor c = database.query(
//                table,
//                null,
//                null,
//                null,
//                null,   // don't group the rows
//                null,   // don't filter by row groups
//                null    // The sort order
//        );
//
//        List<DirentsCacheEntity> list = CollectionUtils.newArrayList();
//
//        try {
//            c.moveToFirst();
//            while (!c.isAfterLast()) {
//                DirentsCacheEntity item = new DirentsCacheEntity();
//                int idIndex = c.getColumnIndexOrThrow("id");
//                int repoIdIndex = c.getColumnIndexOrThrow("repo_id");
//                int pathIndex = c.getColumnIndexOrThrow("path");
//                int dirIdIndex = c.getColumnIndexOrThrow("dir_id");
//
//                item.id = c.getLong(idIndex);
//                item.repo_id = c.getString(repoIdIndex);
//                item.path = c.getString(pathIndex);
//                item.dir_id = c.getString(dirIdIndex);
//                item.related_account = currentAccount.getSignature();
//
//                c.moveToNext();
//
//                list.add(item);
//            }
//        } finally {
//            c.close();
//        }
//
    }

    private void queryEncKeyOfDataDB() {
        String table = "EncKey";

        DatabaseHelper dbHelper = DatabaseHelper.getDatabaseHelper();
        SQLiteDatabase database = dbHelper.getWritableDatabase();
        boolean isExists = checkTableExists(database, table);
        if (!isExists) {
            return;
        }

        Cursor c = database.query(
                table,
                null,
                null,
                null,
                null,   // don't group the rows
                null,   // don't filter by row groups
                null    // The sort order
        );

        List<EncKeyCacheEntity> list = CollectionUtils.newArrayList();

        try {
            c.moveToFirst();
            while (!c.isAfterLast()) {
                EncKeyCacheEntity item = new EncKeyCacheEntity();
                int idIndex = c.getColumnIndexOrThrow("id");
                int repoIdIndex = c.getColumnIndexOrThrow("repo_id");
                int encKeyIndex = c.getColumnIndexOrThrow("enc_key");
                int encIvIndex = c.getColumnIndexOrThrow("enc_iv");

                item.id = c.getLong(idIndex);
                item.repo_id = c.getString(repoIdIndex);
                item.enc_key = c.getString(encKeyIndex);
                item.enc_iv = c.getString(encIvIndex);

                Optional<RepoModel> repoModelOp = accountRepoList.stream().filter(repo -> repo.repo_id.equals(item.repo_id)).findFirst();
                if (repoModelOp.isPresent()) {
                    RepoModel rm = repoModelOp.get();
                    item.related_account = rm.related_account;
                }

                c.moveToNext();

                list.add(item);
            }
        } finally {
            c.close();
        }

        SLogs.d("--------------------" + table);
        for (EncKeyCacheEntity entity : list) {
            SLogs.d(entity.toString());
        }

        AppDatabase.getInstance().encKeyCacheDAO().insertAllSync(list);
        SLogs.d("--------------------" + table + " -> 完成");

    }

    private void queryCertsDB() {
        //Certs
        String table = "Certs";

        CertsDBHelper dbHelper = CertsDBHelper.getDatabaseHelper();
        SQLiteDatabase database = dbHelper.getWritableDatabase();
        boolean isExists = checkTableExists(database, table);
        if (!isExists) {
            return;
        }

        Cursor c = database.query(
                table,
                null,
                null,
                null,
                null,   // don't group the rows
                null,   // don't filter by row groups
                null    // The sort order
        );

        List<CertEntity> list = CollectionUtils.newArrayList();

        try {
            c.moveToFirst();
            while (!c.isAfterLast()) {
                CertEntity item = new CertEntity();
                int urlIndex = c.getColumnIndexOrThrow("url");
                int certIndex = c.getColumnIndexOrThrow("cert");

                item.url = c.getString(urlIndex);
                item.cert = c.getString(certIndex);

                c.moveToNext();

                list.add(item);
            }
        } finally {
            c.close();
        }

        SLogs.d("--------------------" + table);
        for (CertEntity entity : list) {
            SLogs.d(entity.toString());
        }

        AppDatabase.getInstance().certDAO().insertAll(list);
        SLogs.d("--------------------" + table + " -> 完成");

    }

    //TODO
    private void queryMonitorDB() {
        //AutoUpdateInfo
        String table = "AutoUpdateInfo";

        MonitorDBHelper dbHelper = MonitorDBHelper.getInstance();
        SQLiteDatabase database = dbHelper.getWritableDatabase();
        boolean isExists = checkTableExists(database, table);
        if (!isExists) {
            return;
        }

        Cursor c = database.query(
                table,
                null,
                null,
                null,
                null,   // don't group the rows
                null,   // don't filter by row groups
                null    // The sort order
        );

        List<FolderBackupMonitorEntity> list = CollectionUtils.newArrayList();

        try {
            c.moveToFirst();
            while (!c.isAfterLast()) {
                FolderBackupMonitorEntity item = new FolderBackupMonitorEntity();
                int idIndex = c.getColumnIndexOrThrow("id");
                int repoIdIndex = c.getColumnIndexOrThrow("repo_id");
                int repoNameIndex = c.getColumnIndexOrThrow("repo_name");
                int accountIndex = c.getColumnIndexOrThrow("account");
                int parentDirIndex = c.getColumnIndexOrThrow("parent_dir");
                int localPathIndex = c.getColumnIndexOrThrow("local_path");
                int versionIndex = c.getColumnIndexOrThrow("version");


                item.id = c.getLong(idIndex);
                item.repo_id = c.getString(repoIdIndex);
                item.repo_name = c.getString(repoNameIndex);
                item.related_account = c.getString(accountIndex);
                item.parent_dir = c.getString(parentDirIndex);
                item.local_path = c.getString(localPathIndex);
                item.version = c.getString(versionIndex);

                c.moveToNext();

                list.add(item);
            }
        } finally {
            c.close();
        }

        SLogs.d("--------------------" + table);
        list.forEach(new Consumer<FolderBackupMonitorEntity>() {
            @Override
            public void accept(FolderBackupMonitorEntity entity) {
                SLogs.d(entity.toString());
            }
        });

        Completable completable = AppDatabase.getInstance().folderBackupMonitorDAO().insertAll(list);
        compositeDisposable.add(completable
                .subscribeOn(Schedulers.io())
                .observeOn(AndroidSchedulers.mainThread())
                .subscribe(new Action() {
                    @Override
                    public void run() throws Exception {
                        SLogs.d("--------------------" + table + " -> 完成");
                    }
                }));
    }

    private void finishMigration() {
        SLogs.d("finishMigration");
    }

    @Override
    public boolean onKeyDown(int keyCode, KeyEvent event) {
        if (keyCode == KeyEvent.KEYCODE_BACK) {
            return true; //do not back
        }
        return super.onKeyDown(keyCode, event);
    }

    @Override
    public void onBackPressed() {
        //do not super
    }
}
