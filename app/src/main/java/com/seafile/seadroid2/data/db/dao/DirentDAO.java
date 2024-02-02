package com.seafile.seadroid2.data.db.dao;

import androidx.room.Dao;
import androidx.room.Insert;
import androidx.room.OnConflictStrategy;
import androidx.room.Query;

import com.seafile.seadroid2.data.db.entities.DirentModel;

import java.util.List;

import io.reactivex.Completable;
import io.reactivex.Single;

@Dao
public interface DirentDAO {
    @Query("select * from dirents where related_account_email = :cur_account_email")
    Single<List<DirentModel>> getAllByAccount(String cur_account_email);

    @Query("select * from dirents where parent_dir = :parent_dir and repo_id = :repo_id")
    Single<List<DirentModel>> getAllByParentPath(String repo_id, String parent_dir);

    @Query("select * from dirents where id = :id")
    Single<DirentModel> getDirentById(String id);

    @Insert(onConflict = OnConflictStrategy.REPLACE)
    Completable insertAll(List<DirentModel> list);

    @Query("DELETE FROM dirents where related_account_email = :cur_account_email")
    Completable deleteAllByAccount(String cur_account_email);

    @Query("DELETE FROM dirents where parent_dir = :parent_dir and repo_id = :repo_id")
    Completable deleteAllByPath(String repo_id, String parent_dir);

    @Query("DELETE FROM dirents")
    Completable deleteAll();
}
