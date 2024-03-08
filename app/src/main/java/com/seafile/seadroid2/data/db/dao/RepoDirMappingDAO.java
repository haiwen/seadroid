package com.seafile.seadroid2.data.db.dao;

import androidx.room.Dao;
import androidx.room.Insert;
import androidx.room.OnConflictStrategy;
import androidx.room.Query;
import androidx.room.Update;

import com.seafile.seadroid2.data.db.entities.RepoDirMappingEntity;

import java.util.List;

import io.reactivex.Completable;

@Dao
public interface RepoDirMappingDAO {
    @Insert(onConflict = OnConflictStrategy.REPLACE)
    void insert(RepoDirMappingEntity entity);

    @Insert(onConflict = OnConflictStrategy.REPLACE)
    void insertAll(List<RepoDirMappingEntity> entities);

    @Query("select * from repo_dir_mapping where repo_id = :repo_id")
    List<RepoDirMappingEntity> getByRepoId(String repo_id);

    @Query("select * from repo_dir_mapping where repo_id = :repo_id")
    RepoDirMappingEntity getOneByRepoId(String repo_id);

    @Query("select * from repo_dir_mapping where related_account = :related_account and repo_dir = :name")
    RepoDirMappingEntity getOneByUniqueName(String related_account, String name);

    @Update(onConflict = OnConflictStrategy.REPLACE)
    void update(RepoDirMappingEntity entity);


//    @Query("select * from repo_config_cache where repo_id = :repoId limit 1")
//    Single<List<RepoConfigCacheEntity>> getByRepoId(String repoId);
}
