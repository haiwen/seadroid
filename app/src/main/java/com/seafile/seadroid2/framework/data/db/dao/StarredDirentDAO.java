package com.seafile.seadroid2.framework.data.db.dao;

import androidx.room.Dao;
import androidx.room.Insert;
import androidx.room.OnConflictStrategy;
import androidx.room.Query;

import com.seafile.seadroid2.framework.data.db.entities.StarredModel;

import java.util.List;

import io.reactivex.Completable;

@Dao
public interface StarredDirentDAO {

    @Insert(onConflict = OnConflictStrategy.REPLACE)
    Completable insert(StarredModel entity);

    @Insert(onConflict = OnConflictStrategy.REPLACE)
    Completable insertAll(List<StarredModel> list);

    @Insert(onConflict = OnConflictStrategy.REPLACE)
    void insertAllSync(List<StarredModel> list);

    @Query("select * from starred_dirents where related_account = :related_account")
    List<StarredModel> getListByAccountSync(String related_account);

    @Query("DELETE FROM starred_dirents where related_account = :account")
    Completable deleteAllByAccount(String account);
}
