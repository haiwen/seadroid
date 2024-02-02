package com.seafile.seadroid2.data.db;

import androidx.room.Database;
import androidx.room.Room;
import androidx.room.RoomDatabase;

import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.data.db.dao.CertCacheDAO;
import com.seafile.seadroid2.data.db.dao.DirentDAO;
import com.seafile.seadroid2.data.db.dao.DirentsCacheDAO;
import com.seafile.seadroid2.data.db.dao.EncKeyCacheDAO;
import com.seafile.seadroid2.data.db.dao.FileCacheDAO;
import com.seafile.seadroid2.data.db.dao.FolderBackupCacheDAO;
import com.seafile.seadroid2.data.db.dao.FolderBackupMonitorDAO;
import com.seafile.seadroid2.data.db.dao.ObjsDAO;
import com.seafile.seadroid2.data.db.dao.PhotoCacheDAO;
import com.seafile.seadroid2.data.db.dao.RepoDAO;
import com.seafile.seadroid2.data.db.dao.RepoDirDAO;
import com.seafile.seadroid2.data.db.dao.StarredFileCacheDAO;
import com.seafile.seadroid2.data.db.entities.CertEntity;
import com.seafile.seadroid2.data.db.entities.DirentModel;
import com.seafile.seadroid2.data.db.entities.DirentsCacheEntity;
import com.seafile.seadroid2.data.db.entities.EncKeyCacheEntity;
import com.seafile.seadroid2.data.db.entities.FileCacheEntity;
import com.seafile.seadroid2.data.db.entities.FolderBackupCacheEntity;
import com.seafile.seadroid2.data.db.entities.FolderBackupMonitorEntity;
import com.seafile.seadroid2.data.db.entities.ObjsModel;
import com.seafile.seadroid2.data.db.entities.PhotoCacheEntity;
import com.seafile.seadroid2.data.db.entities.RepoDirEntity;
import com.seafile.seadroid2.data.db.entities.RepoModel;
import com.seafile.seadroid2.data.db.entities.StarredFileCacheEntity;

@Database(entities = {
        RepoModel.class,
        DirentModel.class,
        ObjsModel.class,
        PhotoCacheEntity.class,
        FolderBackupCacheEntity.class,
        FolderBackupMonitorEntity.class,
        FileCacheEntity.class,
        StarredFileCacheEntity.class,
        RepoDirEntity.class,
        DirentsCacheEntity.class,
        EncKeyCacheEntity.class,
        CertEntity.class
}, version = 1, exportSchema = false)
public abstract class AppDatabase extends RoomDatabase {
    private static final String DATABASE_NAME = "seafile_room.db";
    private static volatile AppDatabase _instance;

    public static AppDatabase getInstance() {
        if (_instance == null) {
            synchronized (AppDatabase.class) {
                if (_instance == null) {
                    _instance = Room
                            .databaseBuilder(SeadroidApplication.getAppContext(), AppDatabase.class, DATABASE_NAME)
                            .build();
                }
            }
        }
        return _instance;
    }

    public abstract RepoDAO repoDao();

    public abstract DirentDAO direntDao();

    public abstract ObjsDAO objDao();

    public abstract PhotoCacheDAO photoCacheDao();

    public abstract FolderBackupCacheDAO folderBackupCacheDao();

    public abstract FileCacheDAO fileCacheDAO();

    public abstract StarredFileCacheDAO starredFileCacheDAO();

    public abstract DirentsCacheDAO direntsCacheDAO();

    public abstract EncKeyCacheDAO encKeyCacheDAO();

    public abstract CertCacheDAO certDAO();

    public abstract RepoDirDAO repoDirDAO();

    public abstract FolderBackupMonitorDAO folderBackupMonitorDAO();

}
