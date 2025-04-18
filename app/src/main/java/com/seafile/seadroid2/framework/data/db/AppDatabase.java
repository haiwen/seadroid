package com.seafile.seadroid2.framework.data.db;

import android.database.Cursor;

import androidx.annotation.NonNull;
import androidx.room.Database;
import androidx.room.Room;
import androidx.room.RoomDatabase;
import androidx.room.migration.Migration;
import androidx.sqlite.db.SupportSQLiteDatabase;

import com.blankj.utilcode.util.EncryptUtils;
import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.framework.data.db.dao.DirentDAO;
import com.seafile.seadroid2.framework.data.db.dao.EncKeyCacheDAO;
import com.seafile.seadroid2.framework.data.db.dao.FileCacheStatusDAO;
import com.seafile.seadroid2.framework.data.db.dao.FileTransferDAO;
import com.seafile.seadroid2.framework.data.db.dao.FolderBackupMonitorDAO;
import com.seafile.seadroid2.framework.data.db.dao.PermissionDAO;
import com.seafile.seadroid2.framework.data.db.dao.RepoDAO;
import com.seafile.seadroid2.framework.data.db.dao.StarredDirentDAO;
import com.seafile.seadroid2.framework.data.db.entities.DirentModel;
import com.seafile.seadroid2.framework.data.db.entities.EncKeyCacheEntity;
import com.seafile.seadroid2.framework.data.db.entities.FileBackupStatusEntity;
import com.seafile.seadroid2.framework.data.db.entities.FileCacheStatusEntity;
import com.seafile.seadroid2.framework.data.db.entities.FileTransferEntity;
import com.seafile.seadroid2.framework.data.db.entities.FolderBackupMonitorEntity;
import com.seafile.seadroid2.framework.data.db.entities.PermissionEntity;
import com.seafile.seadroid2.framework.data.db.entities.RepoModel;
import com.seafile.seadroid2.framework.data.db.entities.StarredModel;
import com.seafile.seadroid2.framework.util.SLogs;

@Database(entities = {
        RepoModel.class,
        DirentModel.class,
        FolderBackupMonitorEntity.class,
        EncKeyCacheEntity.class,
        FileTransferEntity.class,
        FileBackupStatusEntity.class,
        FileCacheStatusEntity.class,
        StarredModel.class,
        PermissionEntity.class,
}, version = 8, exportSchema = false)
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
                            .addMigrations(MIGRATION_3_4)
                            .addMigrations(MIGRATION_4_5)
                            .addMigrations(MIGRATION_5_6)
                            .addMigrations(MIGRATION_6_7)
                            .addMigrations(MIGRATION_7_8)
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
    static final Migration MIGRATION_3_4 = new Migration(3, 4) {
        @Override
        public void migrate(@NonNull SupportSQLiteDatabase database) {
            // 1. 添加 transfer_result2 列
            database.execSQL("ALTER TABLE file_transfer_list ADD COLUMN result TEXT");

            // 2. 将 transfer_result 的枚举值迁移到 transfer_result2
            database.execSQL("UPDATE file_transfer_list SET result = transfer_result");
        }
    };
    static final Migration MIGRATION_4_5 = new Migration(4, 5) {
        @Override
        public void migrate(@NonNull SupportSQLiteDatabase database) {
            // 1. create table : add result field
            database.execSQL(
                    "CREATE TABLE file_transfer_list_new (" +
                            "'v' INTEGER NOT NULL DEFAULT 1, " +
                            "data_status INTEGER NOT NULL DEFAULT 0, " +
                            "uid TEXT PRIMARY KEY NOT NULL, " +  // 主键
                            "target_path TEXT, " +
                            "full_path TEXT, " +
                            "parent_path TEXT, " +
                            "is_auto_transfer INTEGER NOT NULL DEFAULT 1," +
                            "data_source TEXT, " +
                            "repo_id TEXT, " +
                            "repo_name TEXT, " +
                            "related_account TEXT, " +
                            "file_id TEXT, " +
                            "file_name TEXT, " +
                            "file_format TEXT, " +
                            "mime_type TEXT, " +
                            "file_size INTEGER NOT NULL DEFAULT 0, " +
                            "transferred_size INTEGER NOT NULL DEFAULT 0, " +
                            "file_md5 TEXT, " +
                            "file_strategy TEXT, " +
                            "is_copy_to_local INTEGER NOT NULL DEFAULT 0, " +
                            "created_at INTEGER NOT NULL DEFAULT 0, " +
                            "file_original_modified_at INTEGER NOT NULL DEFAULT 0, " +
                            "modified_at INTEGER NOT NULL DEFAULT 0, " +
                            "action_end_at INTEGER NOT NULL DEFAULT 0, " +
                            "transfer_action TEXT, " +
                            "transfer_status TEXT, " +
                            "result TEXT" +
                            ")"
            );

            // 2. 将旧表数据复制到新表（排除 transfer_result）
            database.execSQL(
                    "INSERT INTO file_transfer_list_new (" +
                            "data_status, v,uid, target_path, full_path, parent_path, is_auto_transfer, data_source, " +
                            "repo_id, repo_name, related_account, file_id, file_name, file_format, " +
                            "mime_type, file_size, transferred_size, file_md5, file_strategy, " +
                            "is_copy_to_local, created_at, file_original_modified_at, modified_at, " +
                            "action_end_at, transfer_action, transfer_status, result) " +
                            "SELECT " +
                            "data_status, v,uid, target_path, full_path, parent_path, is_auto_transfer, data_source, " +
                            "repo_id, repo_name, related_account, file_id, file_name, file_format, " +
                            "mime_type, file_size, transferred_size, file_md5, file_strategy, " +
                            "is_copy_to_local, created_at, file_original_modified_at, modified_at, " +
                            "action_end_at, transfer_action, transfer_status, result " +
                            "FROM file_transfer_list"
            );

            // 3. 删除旧表
            database.execSQL("DROP TABLE file_transfer_list");

            // 4. 重命名新表
            database.execSQL("ALTER TABLE file_transfer_list_new RENAME TO file_transfer_list");

            // 5. 重新创建索引（如果需要）
            database.execSQL(
                    "CREATE UNIQUE INDEX index_transfer_path ON file_transfer_list (uid, full_path, related_account)"
            );
        }
    };

    static final Migration MIGRATION_5_6 = new Migration(5, 6) {

        @Override
        public void migrate(@NonNull SupportSQLiteDatabase database) {
            database.execSQL("DROP TABLE IF EXISTS file_backup_status");

            database.execSQL("CREATE TABLE IF NOT EXISTS `file_backup_status` (" +
                    "'v' INTEGER NOT NULL DEFAULT 1, " +
                    "data_status INTEGER NOT NULL DEFAULT 0, " +
                    "uid TEXT PRIMARY KEY NOT NULL, " +  // 主键
                    "full_path TEXT, " +
                    "target_path TEXT, " +
                    "parent_path TEXT, " +
                    "data_source TEXT, " +
                    "repo_id TEXT, " +
                    "repo_name TEXT, " +
                    "related_account TEXT, " +
                    "file_id TEXT, " +
                    "file_name TEXT, " +
                    "file_format TEXT, " +
                    "mime_type TEXT, " +
                    "file_size INTEGER NOT NULL DEFAULT 0, " +
                    "file_md5 TEXT, " +
                    "created_at INTEGER NOT NULL DEFAULT 0, " +
                    "modified_at INTEGER NOT NULL DEFAULT 0, " +
                    "backup_status INTEGER NOT NULL DEFAULT 1" +
                    ")"
            );

            // 重建符合要求的唯一索引
            database.execSQL("CREATE UNIQUE INDEX IF NOT EXISTS `index_backup_path` " +
                    "ON `file_backup_status` (`uid`, `full_path`, `related_account`)");

            // 2. 分页处理旧数据（防止内存溢出）
            int offset = 0;
            int limit = 500;
            while (true) {
                Cursor cursor = database.query("SELECT * FROM file_transfer_list LIMIT ? OFFSET ?", new Object[]{limit, offset});

                if (!cursor.moveToFirst()) break;

                do {
                    String uid = cursor.getString(cursor.getColumnIndexOrThrow("uid"));
                    String target_path = cursor.getString(cursor.getColumnIndexOrThrow("target_path"));
                    String full_path = cursor.getString(cursor.getColumnIndexOrThrow("full_path"));
                    String parent_path = cursor.getString(cursor.getColumnIndexOrThrow("parent_path"));
                    String data_source = cursor.getString(cursor.getColumnIndexOrThrow("data_source"));
                    String repo_id = cursor.getString(cursor.getColumnIndexOrThrow("repo_id"));
                    String repo_name = cursor.getString(cursor.getColumnIndexOrThrow("repo_name"));
                    String related_account = cursor.getString(cursor.getColumnIndexOrThrow("related_account"));
                    String file_id = cursor.getString(cursor.getColumnIndexOrThrow("file_id"));
                    String file_name = cursor.getString(cursor.getColumnIndexOrThrow("file_name"));
                    String file_format = cursor.getString(cursor.getColumnIndexOrThrow("file_format"));
                    String mime_type = cursor.getString(cursor.getColumnIndexOrThrow("mime_type"));
                    long file_size = cursor.getLong(cursor.getColumnIndexOrThrow("file_size"));
                    String file_md5 = cursor.getString(cursor.getColumnIndexOrThrow("file_md5"));
                    long created_at = cursor.getLong(cursor.getColumnIndexOrThrow("created_at"));
                    long modified_at = cursor.getLong(cursor.getColumnIndexOrThrow("modified_at"));

                    String md5Str = related_account + data_source + repo_id + full_path;
                    SLogs.e(md5Str);

                    int backup_status = 1;
                    // Calculate the new UID (compatible with the old version of logic)
                    String newUid;
                    if (repo_id == null || repo_id.isEmpty()) {
                        newUid = uid;
                    } else {
                        newUid = EncryptUtils.encryptMD5ToString(md5Str).toLowerCase();
                    }

                    database.execSQL("INSERT INTO 'file_backup_status' " +
                                    "VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)",
                            new Object[]{
                                    2, 0, newUid, full_path, target_path, parent_path, data_source, repo_id,
                                    repo_name, related_account, file_id, file_name, file_format, mime_type,
                                    file_size, file_md5, created_at, modified_at, backup_status
                            });

                } while (cursor.moveToNext());

                offset += limit;
                cursor.close();
            }
        }
    };
    static final Migration MIGRATION_6_7 = new Migration(6, 7) {

        @Override
        public void migrate(@NonNull SupportSQLiteDatabase database) {
            //create download table
            database.execSQL("DROP TABLE IF EXISTS file_cache_status");

            database.execSQL("CREATE TABLE IF NOT EXISTS `file_cache_status` (" +
                    "'v' INTEGER NOT NULL DEFAULT 2, " +
                    "data_status INTEGER NOT NULL DEFAULT 0, " +
                    "uid TEXT PRIMARY KEY NOT NULL, " +  // 主键
                    "full_path TEXT, " +
                    "target_path TEXT, " +
                    "parent_path TEXT, " +
                    "repo_id TEXT, " +
                    "repo_name TEXT, " +
                    "related_account TEXT, " +
                    "file_id TEXT, " +
                    "file_name TEXT, " +
                    "file_format TEXT, " +
                    "mime_type TEXT, " +
                    "file_size INTEGER NOT NULL DEFAULT 0, " +
                    "file_md5 TEXT, " +
                    "created_at INTEGER NOT NULL DEFAULT 0, " +
                    "modified_at INTEGER NOT NULL DEFAULT 0" +
                    ")"
            );

            database.execSQL("CREATE UNIQUE INDEX IF NOT EXISTS `index_download_path` " +
                    "ON `file_cache_status` (`uid`, `full_path`, `related_account`)");

        }
    };
    static final Migration MIGRATION_7_8 = new Migration(7, 8) {
        @Override
        public void migrate(@NonNull SupportSQLiteDatabase database) {
            // Because the format of the data saved in the enc_key_cache table has changed,
            // need to delete all the saved data
            database.execSQL("DELETE FROM enc_key_cache WHERE 1=1;");
        }
    };

    public abstract RepoDAO repoDao();

    public abstract DirentDAO direntDao();

    public abstract StarredDirentDAO starredDirentDAO();

    public abstract EncKeyCacheDAO encKeyCacheDAO();

    @Deprecated
    public abstract FolderBackupMonitorDAO folderBackupMonitorDAO();

    public abstract FileTransferDAO fileTransferDAO();

    public abstract FileCacheStatusDAO fileCacheStatusDAO();

    public abstract PermissionDAO permissionDAO();
}
