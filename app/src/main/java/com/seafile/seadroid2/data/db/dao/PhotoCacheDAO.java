package com.seafile.seadroid2.data.db.dao;

import androidx.room.Dao;
import androidx.room.Insert;
import androidx.room.OnConflictStrategy;
import androidx.room.Query;

import com.seafile.seadroid2.data.db.entities.ObjsModel;
import com.seafile.seadroid2.data.db.entities.PhotoCacheEntity;

import java.util.List;

import io.reactivex.Completable;
import io.reactivex.Single;

@Dao
public interface PhotoCacheDAO {
    @Insert(onConflict = OnConflictStrategy.REPLACE)
    Completable insert(PhotoCacheEntity entity);

    @Insert(onConflict = OnConflictStrategy.REPLACE)
    Completable insertAll(List<PhotoCacheEntity> entities);

    @Query("select * from photo_cache where file = :file limit 1")
    Single<List<PhotoCacheEntity>> getByfile(String file);
}
