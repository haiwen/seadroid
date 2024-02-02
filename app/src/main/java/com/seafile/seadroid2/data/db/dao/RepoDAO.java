package com.seafile.seadroid2.data.db.dao;

import androidx.room.Dao;
import androidx.room.Insert;
import androidx.room.OnConflictStrategy;
import androidx.room.Query;

import com.seafile.seadroid2.data.db.entities.RepoModel;

import java.util.List;

import io.reactivex.Completable;
import io.reactivex.Single;

@Dao
public interface RepoDAO {
    @Query("select * from repos where related_account_email = :cur_account_email")
    Single<List<RepoModel>> getAllByAccount(String cur_account_email);

    @Query("select * from repos where repo_id = :repo_id limit 1")
    Single<List<RepoModel>> getRepoById(String repo_id);

    @Insert(onConflict = OnConflictStrategy.REPLACE)
    Completable insertAll(List<RepoModel> list);

    @Query("DELETE FROM repos where related_account_email = :cur_account_email")
    Completable deleteAllByAccount(String cur_account_email);

    @Query("DELETE FROM repos")
    Completable deleteAll();
}
