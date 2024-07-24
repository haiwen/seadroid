package com.seafile.seadroid2.framework.data.db.dao;

import androidx.room.Dao;
import androidx.room.Insert;
import androidx.room.OnConflictStrategy;
import androidx.room.Query;

import com.seafile.seadroid2.framework.data.db.entities.CertEntity;

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

    @Query("DELETE FROM cert_cache where url = :u")
    void deleteByUrl(String u);

    @Query("select * from cert_cache where url = :u limit 1")
    List<CertEntity> getListByUrl(String u);

//    @Query("select * from repo_config_cache where repo_id = :repoId limit 1")
//    Single<List<RepoConfigCacheEntity>> getByRepoId(String repoId);
}
