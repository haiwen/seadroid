package com.seafile.seadroid2.framework.db.dao;

import androidx.room.Dao;
import androidx.room.Insert;
import androidx.room.OnConflictStrategy;
import androidx.room.Query;

import com.seafile.seadroid2.framework.db.entities.GroupEntity;

import java.util.List;

import io.reactivex.Completable;
import io.reactivex.Single;

@Dao
public interface GroupDAO {
    // 查询所有 Group 数据
    @Query("SELECT * FROM 'groups'")
    Single<List<GroupEntity>> queryAll();

    @Query("DELETE FROM 'groups'")
    Completable deleteAll();

    @Insert(onConflict = OnConflictStrategy.REPLACE)
    Completable insertAll(List<GroupEntity> groups);

    @Insert(onConflict = OnConflictStrategy.REPLACE)
    void insertAllSync(List<GroupEntity> groups);
}
