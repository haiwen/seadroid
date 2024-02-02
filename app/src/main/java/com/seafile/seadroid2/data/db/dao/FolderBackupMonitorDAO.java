package com.seafile.seadroid2.data.db.dao;

import androidx.room.Dao;
import androidx.room.Insert;
import androidx.room.OnConflictStrategy;

import com.seafile.seadroid2.data.db.entities.FolderBackupMonitorEntity;
import com.seafile.seadroid2.data.db.entities.StarredFileCacheEntity;

import java.util.List;

import io.reactivex.Completable;

@Dao
public interface FolderBackupMonitorDAO {
    @Insert(onConflict = OnConflictStrategy.REPLACE)
    Completable insert(FolderBackupMonitorEntity entity);

    @Insert(onConflict = OnConflictStrategy.REPLACE)
    Completable insertAll(List<FolderBackupMonitorEntity> entities);

//    @Query("select * from repo_config_cache where repo_id = :repoId limit 1")
//    Single<List<RepoConfigCacheEntity>> getByRepoId(String repoId);
}
