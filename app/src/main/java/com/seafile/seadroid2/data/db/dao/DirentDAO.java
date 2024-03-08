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
    @Query("select * from dirents where related_account = :related_account")
    Single<List<DirentModel>> getAllByAccount(String related_account);

    @Query("select * from dirents where parent_dir = :parent_dir and repo_id = :repo_id")
    Single<List<DirentModel>> getAllByParentPath(String repo_id, String parent_dir);

    @Query("select * from dirents where parent_dir = :parent_dir and repo_id = :repo_id")
    List<DirentModel> getAllByParentPathSync(String repo_id, String parent_dir);

    @Query("select * from dirents where id = :id")
    Single<DirentModel> getDirentById(String id);

    @Insert(onConflict = OnConflictStrategy.REPLACE)
    Completable insertAll(List<DirentModel> list);

    @Insert(onConflict = OnConflictStrategy.REPLACE)
    void insertAllSync(List<DirentModel> list);

    @Query("DELETE FROM dirents where related_account = :cur_account")
    Completable deleteAllByAccount(String cur_account);

    @Query("DELETE FROM dirents where parent_dir = :parent_dir and repo_id = :repo_id")
    Completable deleteAllByParentPath(String repo_id, String parent_dir);

    @Query("DELETE FROM dirents where parent_dir = :parent_dir and repo_id = :repo_id")
    void deleteAllByParentPathSync(String repo_id, String parent_dir);

    @Query("DELETE FROM dirents")
    Completable deleteAll();
}
