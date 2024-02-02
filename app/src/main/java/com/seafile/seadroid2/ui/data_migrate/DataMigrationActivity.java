package com.seafile.seadroid2.ui.data_migrate;

import android.database.Cursor;
import android.database.sqlite.SQLiteDatabase;
import android.os.Bundle;
import android.view.KeyEvent;

import androidx.appcompat.app.AppCompatActivity;

import com.blankj.utilcode.util.CollectionUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.ui.camera_upload.CameraUploadDBHelper;
import com.seafile.seadroid2.data.DatabaseHelper;
import com.seafile.seadroid2.data.db.AppDatabase;
import com.seafile.seadroid2.data.db.entities.CertEntity;
import com.seafile.seadroid2.data.db.entities.DirentsCacheEntity;
import com.seafile.seadroid2.data.db.entities.EncKeyCacheEntity;
import com.seafile.seadroid2.data.db.entities.FileCacheEntity;
import com.seafile.seadroid2.data.db.entities.FolderBackupCacheEntity;
import com.seafile.seadroid2.data.db.entities.FolderBackupMonitorEntity;
import com.seafile.seadroid2.data.db.entities.PhotoCacheEntity;
import com.seafile.seadroid2.data.db.entities.RepoDirEntity;
import com.seafile.seadroid2.data.db.entities.StarredFileCacheEntity;
import com.seafile.seadroid2.databinding.ActivityDataMigrationBinding;
import com.seafile.seadroid2.ui.folder_backup.FolderBackupDBHelper;
import com.seafile.seadroid2.monitor.MonitorDBHelper;
import com.seafile.seadroid2.ssl.CertsDBHelper;
import com.seafile.seadroid2.ui.folder_backup.RepoConfig;
import com.seafile.seadroid2.util.SLogs;
import com.seafile.seadroid2.util.sp.FolderBackupConfigSPs;

import java.util.List;
import java.util.function.Consumer;

import io.reactivex.Completable;
import io.reactivex.android.schedulers.AndroidSchedulers;
import io.reactivex.disposables.CompositeDisposable;
import io.reactivex.functions.Action;
import io.reactivex.schedulers.Schedulers;

/**
 * Migrating data from sqlite database to Room
 */
public class DataMigrationActivity extends AppCompatActivity {
    private ActivityDataMigrationBinding binding;
    private final CompositeDisposable compositeDisposable = new CompositeDisposable();

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        binding = ActivityDataMigrationBinding.inflate(getLayoutInflater());
        setContentView(binding.getRoot());

        binding.text.setText(R.string.wait);

        startMigration();
    }

    private void startMigration() {
        queryPhotoDB();
        queryFolderBackupDB();
        queryRepoConfigOfFolderBackDB();
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

        List<PhotoCacheEntity> list = CollectionUtils.newArrayList();

        try {
            c.moveToFirst();
            while (!c.isAfterLast()) {
                PhotoCacheEntity item = new PhotoCacheEntity();
                int idIndex = c.getColumnIndexOrThrow("id");
                int fileIndex = c.getColumnIndexOrThrow("file");
                int dateIndex = c.getColumnIndexOrThrow("date_added");

                item.id = c.getLong(idIndex);
                item.file = c.getString(fileIndex);
                item.date_added = c.getLong(dateIndex);
                item.related_account = SupportAccountManager.getInstance().getCurrentAccount().getSignature();

                c.moveToNext();

                list.add(item);
            }
        } finally {
            c.close();
        }

        SLogs.d("--------------------" + table);
        list.forEach(new Consumer<PhotoCacheEntity>() {

            @Override
            public void accept(PhotoCacheEntity photoCacheEntity) {
                SLogs.d(photoCacheEntity.toString());
            }
        });

        Completable completable = AppDatabase.getInstance().photoCacheDao().insertAll(list);
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

    private void queryFolderBackupDB() {
        //FolderBackupInfo
        //RepoFig

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

        List<FolderBackupCacheEntity> list = CollectionUtils.newArrayList();

        try {
            c.moveToFirst();
            while (!c.isAfterLast()) {
                FolderBackupCacheEntity item = new FolderBackupCacheEntity();
                int idIndex = c.getColumnIndexOrThrow("id");
                int repoIdIndex = c.getColumnIndexOrThrow("repo_id");
                int repoNameIndex = c.getColumnIndexOrThrow("repo_name");
                int parentFolderIndex = c.getColumnIndexOrThrow("parent_folder");
                int fileNameIndex = c.getColumnIndexOrThrow("file_name");
                int filePathIndex = c.getColumnIndexOrThrow("file_path");
                int fileSizeIndex = c.getColumnIndexOrThrow("file_size");

                item.id = c.getLong(idIndex);
                item.repo_id = c.getString(repoIdIndex);
                item.repo_name = c.getString(repoNameIndex);
                item.parent_folder = c.getString(parentFolderIndex);
                item.file_name = c.getString(fileNameIndex);
                item.file_path = c.getString(filePathIndex);
                item.file_size = Long.parseLong(c.getString(fileSizeIndex));
                item.related_account = SupportAccountManager.getInstance().getCurrentAccount().getSignature();

                c.moveToNext();

                list.add(item);
            }
        } finally {
            c.close();
        }
        SLogs.d("--------------------" + table);

        list.forEach(new Consumer<FolderBackupCacheEntity>() {
            @Override
            public void accept(FolderBackupCacheEntity folderBackupCacheEntity) {
                SLogs.d(folderBackupCacheEntity.toString());
            }
        });

        Completable completable = AppDatabase.getInstance().folderBackupCacheDao().insertAll(list);
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
                int emailIndex = c.getColumnIndexOrThrow("mail");//not email,it is mail
                int repoIdIndex = c.getColumnIndexOrThrow("repo_id");
                int repoNameIndex = c.getColumnIndexOrThrow("repo_name");

                String repo_id = c.getString(repoIdIndex);
                String repo_name = c.getString(repoNameIndex);
                String related_account = c.getString(emailIndex);

                RepoConfig repoConfig = new RepoConfig(repo_id, repo_name, related_account);
                FolderBackupConfigSPs.setBackupRepoConfigByAccount(repoConfig);

                c.moveToNext();
            }
        } finally {
            c.close();
        }
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

        List<FileCacheEntity> list = CollectionUtils.newArrayList();

        try {
            c.moveToFirst();
            while (!c.isAfterLast()) {
                FileCacheEntity item = new FileCacheEntity();
                int idIndex = c.getColumnIndexOrThrow("id");
                int fileIdIndex = c.getColumnIndexOrThrow("fileid");
                int pathIndex = c.getColumnIndexOrThrow("path");
                int repoIdIndex = c.getColumnIndexOrThrow("repo_id");
                int repoNameIndex = c.getColumnIndexOrThrow("repo_name");
                int accountIndex = c.getColumnIndexOrThrow("account");


                item.id = c.getLong(idIndex);
                item.repo_id = c.getString(repoIdIndex);
                item.repo_name = c.getString(repoNameIndex);
                item.file_id = c.getString(fileIdIndex);
                item.path = c.getString(pathIndex);
                item.related_account = c.getString(accountIndex);

                c.moveToNext();

                list.add(item);
            }
        } finally {
            c.close();
        }

        SLogs.d("--------------------" + table);
        list.forEach(new Consumer<FileCacheEntity>() {
            @Override
            public void accept(FileCacheEntity entity) {
                SLogs.d(entity.toString());
            }
        });

        Completable completable = AppDatabase.getInstance().fileCacheDAO().insertAll(list);
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

    private void queryStarredFileCacheOfDataDB() {
        String table = "StarredFileCache";

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

        List<StarredFileCacheEntity> list = CollectionUtils.newArrayList();

        try {
            c.moveToFirst();
            while (!c.isAfterLast()) {
                StarredFileCacheEntity item = new StarredFileCacheEntity();
                int idIndex = c.getColumnIndexOrThrow("id");
                int contentIndex = c.getColumnIndexOrThrow("content");
                int accountIndex = c.getColumnIndexOrThrow("account");


                item.id = c.getLong(idIndex);
                item.content = c.getString(contentIndex);
                item.related_account = c.getString(accountIndex);

                c.moveToNext();

                list.add(item);
            }
        } finally {
            c.close();
        }

        SLogs.d("--------------------" + table);
        list.forEach(new Consumer<StarredFileCacheEntity>() {
            @Override
            public void accept(StarredFileCacheEntity entity) {
                SLogs.d(entity.toString());
            }
        });

        Completable completable = AppDatabase.getInstance().starredFileCacheDAO().insertAll(list);
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

        List<RepoDirEntity> list = CollectionUtils.newArrayList();

        try {
            c.moveToFirst();
            while (!c.isAfterLast()) {
                RepoDirEntity item = new RepoDirEntity();
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
        list.forEach(new Consumer<RepoDirEntity>() {
            @Override
            public void accept(RepoDirEntity entity) {
                SLogs.d(entity.toString());
            }
        });

        Completable completable = AppDatabase.getInstance().repoDirDAO().insertAll(list);
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

    private void queryDirentsCacheOfDataDB() {
        String table = "DirentsCache";

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

        List<DirentsCacheEntity> list = CollectionUtils.newArrayList();

        try {
            c.moveToFirst();
            while (!c.isAfterLast()) {
                DirentsCacheEntity item = new DirentsCacheEntity();
                int idIndex = c.getColumnIndexOrThrow("id");
                int repoIdIndex = c.getColumnIndexOrThrow("repo_id");
                int pathIndex = c.getColumnIndexOrThrow("path");
                int dirIdIndex = c.getColumnIndexOrThrow("dir_id");

                item.id = c.getLong(idIndex);
                item.repo_id = c.getString(repoIdIndex);
                item.path = c.getString(pathIndex);
                item.dir_id = c.getString(dirIdIndex);
                item.related_account = SupportAccountManager.getInstance().getCurrentAccount().getSignature();

                c.moveToNext();

                list.add(item);
            }
        } finally {
            c.close();
        }

        SLogs.d("--------------------" + table);
        list.forEach(new Consumer<DirentsCacheEntity>() {
            @Override
            public void accept(DirentsCacheEntity entity) {
                SLogs.d(entity.toString());
            }
        });

        Completable completable = AppDatabase.getInstance().direntsCacheDAO().insertAll(list);
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
                item.related_account = SupportAccountManager.getInstance().getCurrentAccount().getSignature();

                c.moveToNext();

                list.add(item);
            }
        } finally {
            c.close();
        }

        SLogs.d("--------------------" + table);
        list.forEach(new Consumer<EncKeyCacheEntity>() {
            @Override
            public void accept(EncKeyCacheEntity entity) {
                SLogs.d(entity.toString());
            }
        });

        Completable completable = AppDatabase.getInstance().encKeyCacheDAO().insertAll(list);
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
        list.forEach(new Consumer<CertEntity>() {
            @Override
            public void accept(CertEntity entity) {
                SLogs.d(entity.toString());
            }
        });

        Completable completable = AppDatabase.getInstance().certDAO().insertAll(list);
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
