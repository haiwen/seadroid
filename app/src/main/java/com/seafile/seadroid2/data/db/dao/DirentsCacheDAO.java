package com.seafile.seadroid2.data.db.dao;

import androidx.room.Dao;
import androidx.room.Insert;
import androidx.room.OnConflictStrategy;

import com.seafile.seadroid2.data.db.entities.DirentsCacheEntity;

import java.util.List;

import io.reactivex.Completable;

@Dao
public interface DirentsCacheDAO {
    @Insert(onConflict = OnConflictStrategy.REPLACE)
    Completable insert(DirentsCacheEntity entity);

    @Insert(onConflict = OnConflictStrategy.REPLACE)
    Completable insertAll(List<DirentsCacheEntity> entities);

//    @Query("select * from repo_config_cache where repo_id = :repoId limit 1")
//    Single<List<RepoConfigCacheEntity>> getByRepoId(String repoId);
}
