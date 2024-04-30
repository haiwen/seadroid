package com.seafile.seadroid2.ui.data_migrate;

import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.database.Cursor;
import android.database.sqlite.SQLiteDatabase;
import android.os.Bundle;
import android.text.TextUtils;
import android.view.KeyEvent;
import android.webkit.MimeTypeMap;

import androidx.activity.OnBackPressedCallback;
import androidx.appcompat.app.AppCompatActivity;
import androidx.preference.PreferenceManager;

import com.blankj.utilcode.util.ActivityUtils;
import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.FileUtils;
import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.AccountInfo;
import com.seafile.seadroid2.account.Authenticator;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.config.Constants;
import com.seafile.seadroid2.databinding.ActivityDataMigrationBinding;
import com.seafile.seadroid2.framework.data.DatabaseHelper;
import com.seafile.seadroid2.framework.data.db.AppDatabase;
import com.seafile.seadroid2.framework.data.db.entities.CertEntity;
import com.seafile.seadroid2.framework.data.db.entities.EncKeyCacheEntity;
import com.seafile.seadroid2.framework.data.db.entities.FileTransferEntity;
import com.seafile.seadroid2.framework.data.db.entities.FolderBackupMonitorEntity;
import com.seafile.seadroid2.framework.data.db.entities.RepoModel;
import com.seafile.seadroid2.framework.data.model.enums.TransferAction;
import com.seafile.seadroid2.framework.data.model.enums.TransferDataSource;
import com.seafile.seadroid2.framework.data.model.enums.TransferResult;
import com.seafile.seadroid2.framework.data.model.enums.TransferStatus;
import com.seafile.seadroid2.framework.data.model.repo.RepoWrapperModel;
import com.seafile.seadroid2.framework.datastore.DataStoreKeys;
import com.seafile.seadroid2.framework.datastore.DataStoreManager;
import com.seafile.seadroid2.framework.datastore.StorageManager;
import com.seafile.seadroid2.framework.datastore.sp.AlbumBackupManager;
import com.seafile.seadroid2.framework.datastore.sp.AppDataManager;
import com.seafile.seadroid2.framework.datastore.sp.FolderBackupManager;
import com.seafile.seadroid2.framework.datastore.sp.SettingsManager;
import com.seafile.seadroid2.framework.http.IO;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.framework.worker.ExistingFileStrategy;
import com.seafile.seadroid2.monitor.MonitorDBHelper;
import com.seafile.seadroid2.ssl.CertsDBHelper;
import com.seafile.seadroid2.ui.account.AccountService;
import com.seafile.seadroid2.ui.account.AccountsActivity;
import com.seafile.seadroid2.ui.camera_upload.CameraUploadDBHelper;
import com.seafile.seadroid2.ui.camera_upload.CameraUploadManager;
import com.seafile.seadroid2.ui.folder_backup.FolderBackupDBHelper;
import com.seafile.seadroid2.ui.folder_backup.RepoConfig;
import com.seafile.seadroid2.ui.main.MainActivity;
import com.seafile.seadroid2.ui.repo.RepoService;
import com.seafile.seadroid2.ui.selector.folder_selector.StringTools;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.function.Consumer;

import io.reactivex.Completable;
import io.reactivex.android.schedulers.AndroidSchedulers;
import io.reactivex.disposables.CompositeDisposable;
import io.reactivex.functions.Action;
import io.reactivex.schedulers.Schedulers;
import retrofit2.Call;
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

        List<Account> accounts = SupportAccountManager.getInstance().getAccountList();
        if (CollectionUtils.isEmpty(accounts)) {
            finishMigration();
            navTo();
            return;
        }

        getOnBackPressedDispatcher().addCallback(new OnBackPressedCallback(true) {
            @Override
            public void handleOnBackPressed() {
                //do nothing
            }
        });

        new Thread(new Runnable() {
            @Override
            public void run() {

                try {
                    syncAccount();

                    startMigration();

                    finishMigration();
                } catch (IOException e) {
                    throw new RuntimeException(e);
                } finally {
                    navTo();
                }
            }
        }).start();
    }

    private void navTo() {
        Account curAccount = SupportAccountManager.getInstance().getCurrentAccount();
        if (curAccount == null || !curAccount.hasValidToken()) {
            Intent newIntent = new Intent(this, AccountsActivity.class);
            newIntent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
            newIntent.addFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
            ActivityUtils.startActivity(newIntent);
        } else {
            ActivityUtils.startActivity(MainActivity.class);
        }

        finish();
    }

    private void syncAccount() throws IOException {
        List<Account> accounts = SupportAccountManager.getInstance().getAccountList();
        if (CollectionUtils.isEmpty(accounts)) {
            return;
        }

        for (Account account : accounts) {
            if (TextUtils.isEmpty(account.token)) {
                continue;
            }

            Call<AccountInfo> accountInfoCall = IO.getInstanceByAccount(account).execute(AccountService.class).getAccountInfoCall();
            Response<AccountInfo> accountRes = accountInfoCall.execute();
            if (accountRes.isSuccessful()) {
                AccountInfo accountInfo = accountRes.body();
                if (accountInfo != null) {
                    //update android account avatar url
                    final android.accounts.Account newAccount = new android.accounts.Account(account.getSignature(), Constants.Account.ACCOUNT_TYPE);
                    SupportAccountManager.getInstance().setUserData(newAccount, Authenticator.KEY_AVATAR_URL, accountInfo.getAvatarUrl());
                }
            }


            Response<RepoWrapperModel> response = IO.getInstanceByAccount(account)
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

    private void startMigration() {
        if (accountRepoList.isEmpty()) {
            return;
        }
        migrationSP();

        //
//        queryRepoFileDB();

        queryAlbumSPConfig();
        queryPhotoDB();

        //folder backup
        queryFolderBackupDB();
        queryRepoConfigOfFolderBackDB();
        queryFolderBackPathsOfFolderBack();

        queryDataDB();
        queryMonitorDB();
        queryCertsDB();
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

    private void migrationSP() {
        SharedPreferences sharedPref = getSharedPreferences(DataStoreKeys.LATEST_ACCOUNT, Context.MODE_PRIVATE);
        //
        int storageDir = sharedPref.getInt(SettingsManager.SHARED_PREF_STORAGE_DIR, Integer.MIN_VALUE);
        DataStoreManager.getCommonInstance().writeInteger(SettingsManager.SHARED_PREF_STORAGE_DIR, storageDir);
        SLogs.d("migrated: storageDir -> " + storageDir);

        //privacy
        int privacy = sharedPref.getInt(SettingsManager.PRIVACY_POLICY_CONFIRMED, Integer.MIN_VALUE);
        DataStoreManager.getCommonInstance().writeInteger(SettingsManager.PRIVACY_POLICY_CONFIRMED, privacy);
        SLogs.d("migrated: privacy -> " + privacy);


        //com.seafile.seadroid.account_name
        String curAccount = sharedPref.getString(DataStoreKeys.ACCOUNT_CURRENT_OLD, null);
        DataStoreManager.getCommonInstance().writeString(DataStoreKeys.KEY_CURRENT_ACCOUNT, curAccount);
        SLogs.d("migrated: curAccount -> " + curAccount);

    }

    private void queryRepoFileDB() {

//        List<SeafRepo> seafRepos = SupportDataManager.getInstance().getDataManager().getReposFromCache();
//        if (CollectionUtils.isEmpty(seafRepos)) {
//            return;
//        }
//
//
//        List<RepoModel> repoModels = new ArrayList<>();
//        for (SeafRepo seafRepo : seafRepos) {
//            RepoModel repoModel = new RepoModel();
//            repoModel.type = seafRepo.type;
//            repoModel.repo_id = seafRepo.repo_id;
//            repoModel.repo_name = seafRepo.repo_name;
//            repoModel.group_id = seafRepo.group_id;
//            repoModel.group_name = seafRepo.group_name;
//            repoModel.owner_name = seafRepo.owner_name;
//            repoModel.owner_email = seafRepo.owner_email;
//            repoModel.owner_contact_email = seafRepo.owner_contact_email;
//
//            Optional<RepoModel> repoModelOp = accountRepoList.stream().filter(repo -> repo.repo_id.equals(seafRepo.repo_id)).findFirst();
//            repoModelOp.ifPresent(repoModel1 -> repoModel.related_account = repoModel1.related_account);
//
//            repoModel.modifier_email = seafRepo.modifier_email;
//            repoModel.modifier_name = seafRepo.modifier_name;
//            repoModel.modifier_contact_email = seafRepo.modifier_contact_email;
//            repoModel.last_modified = seafRepo.last_modified;
//            repoModel.size = seafRepo.size;
//            repoModel.permission = seafRepo.permission;
//            repoModel.is_admin = seafRepo.is_admin;
//            repoModel.salt = seafRepo.salt;
//            repoModel.status = seafRepo.status;
//            repoModel.monitored = seafRepo.monitored;
//            repoModel.starred = seafRepo.starred;
//            repoModel.encrypted = seafRepo.encrypted;
//            repoModels.add(repoModel);
//        }
//        AppDatabase.getInstance().repoDao().insertAll(repoModels);
    }

    private void queryAlbumSPConfig() {
        SharedPreferences sharedPref = getSharedPreferences(DataStoreKeys.LATEST_ACCOUNT, Context.MODE_PRIVATE);

        String repoId = sharedPref.getString(SettingsManager.SHARED_PREF_CAMERA_UPLOAD_REPO_ID, null);
        String repoName = sharedPref.getString(SettingsManager.SHARED_PREF_CAMERA_UPLOAD_REPO_NAME, null);
        Account account = CameraUploadManager.getInstance().getCameraAccount();
        if (repoId != null && account != null) {
            RepoConfig config = new RepoConfig(repoId, repoName, account.email, account.getSignature());
            AlbumBackupManager.writeRepoConfig(config);
            SLogs.d("migrated: repoConfig -> " + config.toString());

        }

        //PreferenceManager
        SharedPreferences pm = PreferenceManager.getDefaultSharedPreferences(SeadroidApplication.getAppContext());

        //video
        boolean isVideoAllowed = pm.getBoolean(SettingsManager.CAMERA_UPLOAD_ALLOW_VIDEOS_SWITCH_KEY, false);
        AlbumBackupManager.writeAllowVideoSwitch(isVideoAllowed);
        SLogs.d("migrated: isVideoAllowed -> " + isVideoAllowed);

        //data plan
        boolean isDataPlanAllowed = pm.getBoolean(SettingsManager.CAMERA_UPLOAD_ALLOW_DATA_PLAN_SWITCH_KEY, false);
        AlbumBackupManager.writeAllowDataPlanSwitch(isDataPlanAllowed);
        SLogs.d("migrated: isDataPlanAllowed -> " + isDataPlanAllowed);

        //custom
        boolean isCustomAlbum = pm.getBoolean(SettingsManager.CAMERA_UPLOAD_CUSTOM_BUCKETS_KEY, false);
        AlbumBackupManager.writeCustomAlbumSwitch(isCustomAlbum);
        SLogs.d("migrated: isCustomAlbum -> " + isCustomAlbum);

        //bucket ids
        String bucketIdsStr = sharedPref.getString(SettingsManager.SHARED_PREF_CAMERA_UPLOAD_BUCKETS, "");
        List<String> ids = Arrays.asList(TextUtils.split(bucketIdsStr, ","));
        AlbumBackupManager.writeBucketIds(ids);
        SLogs.d("migrated: bucketIds -> " + bucketIdsStr);
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

            SharedPreferences sharedPref = getSharedPreferences(DataStoreKeys.LATEST_ACCOUNT, Context.MODE_PRIVATE);
            String targetRepoId = sharedPref.getString(SettingsManager.SHARED_PREF_CAMERA_UPLOAD_REPO_ID, null);
            String targetRepoName = sharedPref.getString(SettingsManager.SHARED_PREF_CAMERA_UPLOAD_REPO_NAME, null);

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
                    transferEntity.related_account = backUpRepoModel.related_account;
                } else {
                    transferEntity.repo_id = null;
                    transferEntity.repo_name = null;
                    transferEntity.related_account = null;
                }

                transferEntity.is_copy_to_local = false;
                transferEntity.file_strategy = ExistingFileStrategy.AUTO;

                transferEntity.full_path = filePath;
                transferEntity.setParent_path(Utils.getParentPath(filePath));
                transferEntity.file_name = FileUtils.getFileName(filePath);
                transferEntity.file_size = FileUtils.getFileLength(filePath);
                transferEntity.transferred_size = transferEntity.file_size;
                transferEntity.file_format = FileUtils.getFileExtension(filePath);
                transferEntity.mime_type = MimeTypeMap.getSingleton().getMimeTypeFromExtension(transferEntity.file_format);
                transferEntity.file_md5 = FileUtils.getFileMD5ToString(transferEntity.full_path).toLowerCase();

                transferEntity.created_at = action_end_time_stamp;
                transferEntity.modified_at = action_end_time_stamp;
                transferEntity.action_end_at = action_end_time_stamp;

                transferEntity.data_source = TransferDataSource.ALBUM_BACKUP;
                transferEntity.transfer_action = TransferAction.UPLOAD;
                transferEntity.transfer_status = TransferStatus.SUCCEEDED;
                transferEntity.transfer_result = TransferResult.TRANSMITTED;

                transferEntity.file_id = null;
                transferEntity.file_original_modified_at = FileUtils.getFileLastModified(transferEntity.full_path);

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

                int idIndex = c.getColumnIndexOrThrow("id");
                int repoIdIndex = c.getColumnIndexOrThrow("repo_id");
                int repoNameIndex = c.getColumnIndexOrThrow("repo_name");
                int parentFolderIndex = c.getColumnIndexOrThrow("parent_folder");
                int fileNameIndex = c.getColumnIndexOrThrow("file_name");
                int filePathIndex = c.getColumnIndexOrThrow("file_path");
                int fileSizeIndex = c.getColumnIndexOrThrow("file_size");

                FileTransferEntity transferEntity = new FileTransferEntity();

                transferEntity.v = 0;
                transferEntity.repo_id = c.getString(repoIdIndex);
                transferEntity.repo_name = c.getString(repoNameIndex);

                Optional<RepoModel> repoModelOp = accountRepoList.stream().filter(repo -> repo.repo_id.equals(transferEntity.repo_id)).findFirst();
                if (repoModelOp.isPresent()) {
                    RepoModel rm = repoModelOp.get();
//                    transferEntity.is_block = rm.encrypted;
                    transferEntity.related_account = rm.related_account;
                }


                transferEntity.is_copy_to_local = false;
                transferEntity.file_strategy = ExistingFileStrategy.AUTO;

                transferEntity.full_path = c.getString(filePathIndex);
                transferEntity.setParent_path(c.getString(parentFolderIndex));


                transferEntity.file_name = c.getString(fileNameIndex);
                transferEntity.target_path = transferEntity.getParent_path() + transferEntity.file_name;


                transferEntity.file_size = Long.parseLong(c.getString(fileSizeIndex));
                transferEntity.transferred_size = transferEntity.file_size;
                transferEntity.file_format = FileUtils.getFileExtension(transferEntity.full_path);
                transferEntity.file_original_modified_at = FileUtils.getFileLastModified(transferEntity.full_path);
                transferEntity.mime_type = MimeTypeMap.getSingleton().getMimeTypeFromExtension(transferEntity.file_format);

                transferEntity.file_md5 = FileUtils.getFileMD5ToString(transferEntity.full_path).toLowerCase();

                //null
                transferEntity.file_id = null;

                long nowMills = System.currentTimeMillis();
                transferEntity.created_at = nowMills;
                transferEntity.modified_at = nowMills;
                transferEntity.action_end_at = nowMills;

                transferEntity.data_source = TransferDataSource.FOLDER_BACKUP;
                transferEntity.transfer_action = TransferAction.UPLOAD;
                transferEntity.transfer_result = TransferResult.TRANSMITTED;
                transferEntity.transfer_status = TransferStatus.SUCCEEDED;

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

                Optional<RepoModel> repoModelOp = accountRepoList.stream().filter(repo -> repo.repo_id.equals(repo_id)).findFirst();
                if (repoModelOp.isPresent()) {

                    RepoModel rm = repoModelOp.get();

                    //Only the first piece of data is recovered
                    if (TextUtils.isEmpty(FolderBackupManager.getCurrentAccount())) {
                        FolderBackupManager.setCurrentAccount(rm.related_account);

                        RepoConfig repoConfig = new RepoConfig(repo_id, repo_name, related_account, related_account);
                        FolderBackupManager.writeRepoConfig(repoConfig);
                    }

                }

                c.moveToNext();
            }
        } finally {
            c.close();
        }
    }

    private void queryFolderBackPathsOfFolderBack() {
        if (TextUtils.isEmpty(FolderBackupManager.getCurrentAccount())) {
            return;
        }

        SharedPreferences sharedPref = getSharedPreferences(DataStoreKeys.LATEST_ACCOUNT, Context.MODE_PRIVATE);
        String pathStr = sharedPref.getString(SettingsManager.FOLDER_BACKUP_PATHS, "");

        if (!TextUtils.isEmpty(pathStr)) {
            List<String> selectFolderPaths = StringTools.getJsonToList(pathStr);

            Account account = SupportAccountManager.getInstance().getCurrentAccount();
            if (account == null) {
                //Not logged in or opened for the first time
                List<Account> accountList = SupportAccountManager.getInstance().getAccountList();
                if (!accountList.isEmpty()) {
                    account = accountList.get(0);
                }
            }

            if (account != null) {
                FolderBackupManager.writeBackupPaths(selectFolderPaths);
                SLogs.d("migrated: selectFolderPaths -> " + selectFolderPaths.size());
            }
        }

        //PreferenceManager
        SharedPreferences pm = PreferenceManager.getDefaultSharedPreferences(SeadroidApplication.getAppContext());

        //folder_backup_switch
        boolean folder_backup_switch = pm.getBoolean(SettingsManager.FOLDER_BACKUP_SWITCH_KEY, false);
        FolderBackupManager.writeBackupSwitch(folder_backup_switch);
        SLogs.d("migrated: folder_backup_switch -> " + folder_backup_switch);

        //mode
        String network = pm.getString(SettingsManager.FOLDER_BACKUP_MODE, "WIFI");
        FolderBackupManager.writeNetworkMode(network);//PreferenceManager -> DataStore
        SLogs.d("migrated: networkMode -> " + network);

        //true
        FolderBackupManager.writeSkipHiddenFiles(true);
        SLogs.d("migrated: jumpHidden -> " + true);
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
//                    transferEntity.is_block = rm.encrypted;
                    transferEntity.related_account = rm.related_account;
                }

                transferEntity.is_copy_to_local = false;
                transferEntity.file_strategy = ExistingFileStrategy.AUTO;

                String path = c.getString(pathIndex);

                transferEntity.transfer_result = TransferResult.TRANSMITTED;
                transferEntity.transfer_status = TransferStatus.SUCCEEDED;
                if (path.startsWith("/")) {
                    transferEntity.transfer_action = TransferAction.DOWNLOAD;
                    transferEntity.data_source = TransferDataSource.DOWNLOAD;
                    transferEntity.full_path = path;
                } else {
                    transferEntity.full_path = "/" + path;
                    transferEntity.transfer_action = TransferAction.UPLOAD;
                    if (path.startsWith("My Photos")) {
                        transferEntity.data_source = TransferDataSource.ALBUM_BACKUP;
                    } else {
                        transferEntity.data_source = TransferDataSource.FOLDER_BACKUP;
                    }
                }

                transferEntity.setParent_path(Utils.getParentPath(transferEntity.full_path));
                transferEntity.file_name = FileUtils.getFileName(transferEntity.full_path);

                if (TransferDataSource.DOWNLOAD == transferEntity.data_source) {
                    String mediaPath = StorageManager.getInstance().getMediaDir().getAbsolutePath();
                    String absPath = Utils.pathJoin(mediaPath, transferEntity.related_account, transferEntity.repo_name, path);
                    transferEntity.file_size = FileUtils.getFileLength(absPath);
                    transferEntity.transferred_size = transferEntity.file_size;
                    transferEntity.file_md5 = FileUtils.getFileMD5ToString(absPath).toLowerCase();
                    transferEntity.target_path = absPath;
                } else if (TransferDataSource.FOLDER_BACKUP == transferEntity.data_source) {
                    //queryFolderBackupDB() 方法已实现数据迁移
                } else if (TransferDataSource.ALBUM_BACKUP == transferEntity.data_source) {
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

        Set<String> sets = DataStoreManager.getCommonInstance().readSetString(DataStoreKeys.DS_REPO_DIR_MAPPING);


        try {
            c.moveToFirst();
            while (!c.isAfterLast()) {
                int idIndex = c.getColumnIndexOrThrow("id");
                int repoIdIndex = c.getColumnIndexOrThrow("repo_id");
                int repoDirIndex = c.getColumnIndexOrThrow("repo_dir");
                int accountIndex = c.getColumnIndexOrThrow("account");


//                item.id = c.getLong(idIndex);
                String repo_id = c.getString(repoIdIndex);
                String repo_dir = c.getString(repoDirIndex);
//                item.related_account = c.getString(accountIndex);

                sets.add(repo_id + DataStoreKeys.SEPARATOR + repo_dir);

                c.moveToNext();
            }
        } finally {
            c.close();
        }

//        AppDatabase.getInstance().repoDirMappingDAO().insertAll(list);
        SLogs.d("--------------------" + table + " -> 完成");

        if (!sets.isEmpty()) {
            DataStoreManager.getCommonInstance().writeSetString(DataStoreKeys.DS_REPO_DIR_MAPPING, sets);
        }

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

                item.repo_id = c.getString(repoIdIndex);
                item.enc_key = c.getString(encKeyIndex);
                item.enc_iv = c.getString(encIvIndex);
                item.expire_time_long = System.currentTimeMillis();// now : expire

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
        AppDataManager.setMigratedWhenV300(1);
        SLogs.d("finishMigration");
    }

    @Override
    public boolean onKeyDown(int keyCode, KeyEvent event) {
        if (keyCode == KeyEvent.KEYCODE_BACK) {
            return true; //do not back
        }
        return super.onKeyDown(keyCode, event);
    }
}
