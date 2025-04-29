package com.seafile.seadroid2.framework.db.dao;

import androidx.room.Dao;
import androidx.room.Insert;
import androidx.room.OnConflictStrategy;
import androidx.room.Query;

import com.seafile.seadroid2.framework.db.entities.EncKeyCacheEntity;

import java.util.List;

import io.reactivex.Completable;
import io.reactivex.Single;

@Dao
public interface EncKeyCacheDAO {
    @Insert(onConflict = OnConflictStrategy.REPLACE)
    Completable insert(EncKeyCacheEntity entity);

    @Insert(onConflict = OnConflictStrategy.REPLACE)
    void insertSync(EncKeyCacheEntity entity);

    @Insert(onConflict = OnConflictStrategy.REPLACE)
    Completable insertAllAsync(List<EncKeyCacheEntity> entities);

    @Insert(onConflict = OnConflictStrategy.REPLACE)
    void insertAllSync(List<EncKeyCacheEntity> entities);

    @Query("select * from enc_key_cache where repo_id = :repoId limit 1")
    List<EncKeyCacheEntity> getListByRepoIdSync(String repoId);

    @Query("select * from enc_key_cache where repo_id = :repoId limit 1")
    Single<List<EncKeyCacheEntity>> getListByRepoIdAsync(String repoId);

    @Query("DELETE FROM enc_key_cache")
    Completable deleteAll();


    @Query("DELETE FROM enc_key_cache where related_account = :related_account")
    Completable deleteSpecialAccountData(String related_account);
}
