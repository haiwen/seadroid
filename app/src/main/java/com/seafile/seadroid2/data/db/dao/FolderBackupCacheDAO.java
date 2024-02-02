package com.seafile.seadroid2.data.db.dao;

import androidx.room.Dao;
import androidx.room.Insert;
import androidx.room.OnConflictStrategy;
import androidx.room.Query;

import com.seafile.seadroid2.data.db.entities.FolderBackupCacheEntity;
import com.seafile.seadroid2.data.db.entities.PhotoCacheEntity;

import java.util.List;

import io.reactivex.Completable;
import io.reactivex.Single;

@Dao
public interface FolderBackupCacheDAO {
    @Insert(onConflict = OnConflictStrategy.REPLACE)
    Completable insert(FolderBackupCacheEntity entity);

    @Insert(onConflict = OnConflictStrategy.REPLACE)
    Completable insertAll(List<FolderBackupCacheEntity> entities);
    
    @Query("select * from folder_backup_cache where file_name = :fileName limit 1")
    Single<List<FolderBackupCacheEntity>> getByFileName(String fileName);
}
