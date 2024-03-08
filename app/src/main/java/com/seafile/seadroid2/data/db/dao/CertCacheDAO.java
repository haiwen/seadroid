package com.seafile.seadroid2.data.db.dao;

import androidx.room.Dao;
import androidx.room.Insert;
import androidx.room.OnConflictStrategy;

import com.seafile.seadroid2.data.db.entities.CertEntity;
import com.seafile.seadroid2.data.db.entities.EncKeyCacheEntity;

import java.util.List;

import io.reactivex.Completable;

@Dao
public interface CertCacheDAO {
    @Insert(onConflict = OnConflictStrategy.REPLACE)
    Completable insert(CertEntity entity);

    @Insert(onConflict = OnConflictStrategy.REPLACE)
    Completable insertAllAsync(List<CertEntity> entities);

    @Insert(onConflict = OnConflictStrategy.REPLACE)
    void insertAll(List<CertEntity> entities);

//    @Query("select * from repo_config_cache where repo_id = :repoId limit 1")
//    Single<List<RepoConfigCacheEntity>> getByRepoId(String repoId);
}
