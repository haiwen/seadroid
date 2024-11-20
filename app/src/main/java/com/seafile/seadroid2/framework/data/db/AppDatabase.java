package com.seafile.seadroid2.framework.data.db;

import androidx.annotation.NonNull;
import androidx.room.Database;
import androidx.room.Room;
import androidx.room.RoomDatabase;
import androidx.room.migration.Migration;
import androidx.sqlite.db.SupportSQLiteDatabase;

import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.framework.data.db.dao.DirentDAO;
import com.seafile.seadroid2.framework.data.db.dao.EncKeyCacheDAO;
import com.seafile.seadroid2.framework.data.db.dao.FileTransferDAO;
import com.seafile.seadroid2.framework.data.db.dao.FolderBackupMonitorDAO;
import com.seafile.seadroid2.framework.data.db.dao.PermissionDAO;
import com.seafile.seadroid2.framework.data.db.dao.RepoDAO;
import com.seafile.seadroid2.framework.data.db.dao.StarredDirentDAO;
import com.seafile.seadroid2.framework.data.db.entities.DirentModel;
import com.seafile.seadroid2.framework.data.db.entities.EncKeyCacheEntity;
import com.seafile.seadroid2.framework.data.db.entities.FolderBackupMonitorEntity;
import com.seafile.seadroid2.framework.data.db.entities.PermissionEntity;
import com.seafile.seadroid2.framework.data.db.entities.RepoModel;
import com.seafile.seadroid2.framework.data.db.entities.FileTransferEntity;
import com.seafile.seadroid2.framework.data.db.entities.StarredModel;
import com.seafile.seadroid2.framework.util.SLogs;

@Database(entities = {
        RepoModel.class,
        DirentModel.class,
        FolderBackupMonitorEntity.class,
        EncKeyCacheEntity.class,
        FileTransferEntity.class,
        StarredModel.class,
        PermissionEntity.class,
}, version = 3, exportSchema = false)
public abstract class AppDatabase extends RoomDatabase {
    private static final String DATABASE_NAME = "seafile_room.db";
    private static volatile AppDatabase _instance;
    public static AppDatabase getInstance() {
        if (_instance == null) {
            synchronized (AppDatabase.class) {
                if (_instance == null) {
                    _instance = Room
                            .databaseBuilder(SeadroidApplication.getAppContext(), AppDatabase.class, DATABASE_NAME)
                            .addMigrations(MIGRATION_1_2)
                            .addMigrations(MIGRATION_2_3)
                            .build();
                }
            }
        }
        return _instance;
    }

    static final Migration MIGRATION_1_2 = new Migration(1, 2) {
        @Override
        public void migrate(@NonNull SupportSQLiteDatabase database) {
            //drop table
            database.execSQL("DROP TABLE IF EXISTS repos");

            // create table
            database.execSQL("CREATE TABLE repos (repo_id TEXT NOT NULL, repo_name TEXT, type TEXT, group_id INTEGER NOT NULL DEFAULT 0, group_name TEXT, owner_name TEXT, owner_email TEXT, owner_contact_email TEXT, modifier_email TEXT, modifier_name TEXT, modifier_contact_email TEXT, related_account TEXT, last_modified TEXT, encrypted INTEGER NOT NULL DEFAULT 0, size INTEGER NOT NULL DEFAULT 0, starred INTEGER NOT NULL DEFAULT 0, permission TEXT, monitored INTEGER NOT NULL DEFAULT 0, is_admin INTEGER NOT NULL DEFAULT 0, salt TEXT, status TEXT, last_modified_long INTEGER NOT NULL DEFAULT 0, root TEXT, magic TEXT, random_key TEXT, enc_version INTEGER NOT NULL DEFAULT 0, file_count INTEGER NOT NULL DEFAULT 0,v INTEGER NOT NULL DEFAULT 1,data_status INTEGER NOT NULL DEFAULT 0, PRIMARY KEY(repo_id, group_id))");
        }
    };


    static final Migration MIGRATION_2_3 = new Migration(2, 3) {

        @Override
        public void migrate(@NonNull SupportSQLiteDatabase database) {
            database.execSQL("DROP TABLE IF EXISTS cert_cache");

            database.execSQL("DROP TABLE IF EXISTS permissions");
            database.execSQL("CREATE TABLE permissions (" +
                    "id INTEGER NOT NULL DEFAULT 0, " +
                    "repo_id TEXT NOT NULL DEFAULT '', " +
                    "description TEXT, " +
                    "name TEXT DEFAULT '', " +
                    "'create' INTEGER NOT NULL DEFAULT 0, " +
                    "upload INTEGER NOT NULL DEFAULT 0, " +
                    "download INTEGER NOT NULL DEFAULT 0, " +
                    "preview INTEGER NOT NULL DEFAULT 0, " +
                    "copy INTEGER NOT NULL DEFAULT 0, " +
                    "'delete' INTEGER NOT NULL DEFAULT 0, " +
                    "modify INTEGER NOT NULL DEFAULT 0, " +
                    "download_external_link INTEGER NOT NULL DEFAULT 0, " +
                    "'v' INTEGER NOT NULL DEFAULT 1, " +
                    "data_status INTEGER NOT NULL DEFAULT 0, " +
                    "PRIMARY KEY(repo_id, id))");
        }
    };


    public abstract RepoDAO repoDao();

    public abstract DirentDAO direntDao();

    public abstract StarredDirentDAO starredDirentDAO();

    public abstract EncKeyCacheDAO encKeyCacheDAO();

    public abstract FolderBackupMonitorDAO folderBackupMonitorDAO();

    public abstract FileTransferDAO fileTransferDAO();

    public abstract PermissionDAO permissionDAO();
}
