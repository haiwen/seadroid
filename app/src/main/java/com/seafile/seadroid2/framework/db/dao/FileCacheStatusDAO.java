package com.seafile.seadroid2.framework.db.dao;

import androidx.room.Dao;
import androidx.room.Delete;
import androidx.room.Insert;
import androidx.room.OnConflictStrategy;
import androidx.room.Query;
import androidx.room.Update;

import com.seafile.seadroid2.framework.db.entities.FileCacheStatusEntity;

import java.util.List;

import io.reactivex.Completable;
import io.reactivex.Single;

@Dao
public interface FileCacheStatusDAO {
    @Insert(onConflict = OnConflictStrategy.REPLACE)
    void insertAll(List<FileCacheStatusEntity> list);

    @Insert(onConflict = OnConflictStrategy.REPLACE)
    void insert(FileCacheStatusEntity entity);

    @Update
    void update(FileCacheStatusEntity entity);

    @Delete
    void delete(FileCacheStatusEntity entity);

    @Update
    Completable updateAsync(FileCacheStatusEntity entity);

    @Query("select * from file_cache_status where related_account = :account order by created_at")
    List<FileCacheStatusEntity> getByAccountSync(String account);


    @Query("select * from file_cache_status where related_account = :relate_account and target_path = :target_path order by created_at limit 1")
    List<FileCacheStatusEntity> getByTargetPathSync(String relate_account, String target_path);

    @Query("select * from file_cache_status where repo_id = :repoId and full_path = :fullPath order by created_at limit 1")
    Single<List<FileCacheStatusEntity>> getByFullPath(String repoId, String fullPath);

    @Query("select * from file_cache_status where repo_id = :repoId and full_path = :fullPath order by created_at limit 1")
    List<FileCacheStatusEntity> getByFullPathSync(String repoId, String fullPath);


    @Query("select * from file_cache_status where repo_id = :repoId and parent_path = :parentPath order by created_at")
    List<FileCacheStatusEntity> getByParentPathSync(String repoId, String parentPath);

    @Query("select * from file_cache_status where repo_id = :repoId and (parent_path = :prefixPath OR parent_path LIKE :prefixPath || '%') order by created_at")
    List<FileCacheStatusEntity> getByParentPathStartsWith(String repoId, String prefixPath);

    @Query("UPDATE file_cache_status SET repo_name = :newRepoName WHERE repo_id = :repoId")
    void updateRepoNameByRepoId(String repoId, String newRepoName);

}
