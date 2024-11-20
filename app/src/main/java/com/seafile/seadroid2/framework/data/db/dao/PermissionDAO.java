package com.seafile.seadroid2.framework.data.db.dao;

import androidx.room.Dao;
import androidx.room.Insert;
import androidx.room.OnConflictStrategy;
import androidx.room.Query;
import androidx.room.Update;

import com.seafile.seadroid2.framework.data.db.entities.FileTransferEntity;
import com.seafile.seadroid2.framework.data.db.entities.PermissionEntity;
import com.seafile.seadroid2.framework.data.db.entities.PermissionEntity;

import java.util.List;

import io.reactivex.Completable;
import io.reactivex.Single;

@Dao
public interface PermissionDAO {

    @Query("select * from permissions where repo_id = :repoId and id = :id limit 1")
    List<PermissionEntity> getByIdSync(String repoId, int id);

    @Query("select * from permissions where repo_id = :repoId and id = :id limit 1")
    Single<List<PermissionEntity>> getByIdAsync(String repoId, int id);

    @Query("select * from permissions where repo_id = :repoId")
    Single<List<PermissionEntity>> getByRepoIdAsync(String repoId);


    @Query("DELETE FROM permissions")
    void deleteAll();

    @Insert(onConflict = OnConflictStrategy.REPLACE)
    void insert(PermissionEntity PermissionEntity);

    @Insert(onConflict = OnConflictStrategy.REPLACE)
    void insertAllSync(List<PermissionEntity> list);

    @Insert(onConflict = OnConflictStrategy.REPLACE)
    Completable insertAllAsync(List<PermissionEntity> list);


}
