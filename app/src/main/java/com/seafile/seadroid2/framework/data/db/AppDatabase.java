package com.seafile.seadroid2.framework.data.db;

import androidx.room.Database;
import androidx.room.Room;
import androidx.room.RoomDatabase;

import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.framework.data.db.dao.CertCacheDAO;
import com.seafile.seadroid2.framework.data.db.dao.DirentDAO;
import com.seafile.seadroid2.framework.data.db.dao.EncKeyCacheDAO;
import com.seafile.seadroid2.framework.data.db.dao.FileTransferDAO;
import com.seafile.seadroid2.framework.data.db.dao.FolderBackupMonitorDAO;
import com.seafile.seadroid2.framework.data.db.dao.RepoDAO;
import com.seafile.seadroid2.framework.data.db.dao.StarredDirentDAO;
import com.seafile.seadroid2.framework.data.db.entities.CertEntity;
import com.seafile.seadroid2.framework.data.db.entities.DirentModel;
import com.seafile.seadroid2.framework.data.db.entities.EncKeyCacheEntity;
import com.seafile.seadroid2.framework.data.db.entities.FolderBackupMonitorEntity;
import com.seafile.seadroid2.framework.data.db.entities.RepoModel;
import com.seafile.seadroid2.framework.data.db.entities.FileTransferEntity;
import com.seafile.seadroid2.framework.data.db.entities.StarredModel;

@Database(entities = {
        RepoModel.class,
        DirentModel.class,
        FolderBackupMonitorEntity.class,
        EncKeyCacheEntity.class,
        CertEntity.class,
        FileTransferEntity.class,
        StarredModel.class
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
    public abstract StarredDirentDAO starredDirentDAO();

    public abstract EncKeyCacheDAO encKeyCacheDAO();

    public abstract CertCacheDAO certDAO();

    public abstract FolderBackupMonitorDAO folderBackupMonitorDAO();
    public abstract FileTransferDAO fileTransferDAO();

}
