package com.seafile.seadroid2.data.db.dao;

import androidx.room.Dao;
import androidx.room.Insert;
import androidx.room.OnConflictStrategy;
import androidx.room.Query;

import com.seafile.seadroid2.data.db.entities.ObjsModel;

import java.util.List;

import io.reactivex.Completable;
import io.reactivex.Single;

@Dao
public interface ObjsDAO {
    @Insert(onConflict = OnConflictStrategy.REPLACE)
    Completable insert(ObjsModel dirModel);

    @Query("select * from objs where path = :path limit 1")
    Single<List<ObjsModel>> getByPath(String path);

    @Query("select * from objs where path = :path and type = :type limit 1")
    Single<List<ObjsModel>> getByPath(String path, String type);

    @Query("update objs set decrypt_expire_time_long = :timestamp where path = :path")
    Completable updateDecryptExpireTimeByPath(String path, long timestamp);

}
