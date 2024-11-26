package com.seafile.seadroid2.framework.data.db.dao;

import androidx.room.Dao;
import androidx.room.Insert;
import androidx.room.OnConflictStrategy;
import androidx.room.Query;
import androidx.room.Update;

import com.seafile.seadroid2.framework.data.db.entities.RepoModel;

import java.util.List;

import io.reactivex.Completable;
import io.reactivex.Single;

@Dao
public interface RepoDAO {
    @Query("select * from repos where related_account = :related_account")
    Single<List<RepoModel>> getListByAccount(String related_account);

    @Query("select * from repos where related_account = :related_account")
    List<RepoModel> getListByAccountSync(String related_account);

    @Query("select * from repos where repo_id = :repo_id limit 1")
    Single<List<RepoModel>> getRepoById(String repo_id);

    @Query("select * from repos where repo_id IN (:ids)")
    Single<List<RepoModel>> getRepoListByIds(List<String> ids);

    @Query("select * from repos where repo_id = :repo_id limit 1")
    List<RepoModel> getByIdSync(String repo_id);

    @Query("select * from repos where repo_id = :repo_id limit 1")
    Single<List<RepoModel>> getByIdAsync(String repo_id);


    @Update(onConflict = OnConflictStrategy.REPLACE)
    void update(RepoModel model);

    @Insert(onConflict = OnConflictStrategy.REPLACE)
    Completable insertAll(List<RepoModel> list);

    @Insert(onConflict = OnConflictStrategy.REPLACE)
    void insert(RepoModel repoModel);


    @Insert(onConflict = OnConflictStrategy.REPLACE)
    void insertAllSync(List<RepoModel> list);

    @Query("DELETE FROM repos where related_account = :cur_account_email")
    Completable deleteAllByAccount(String cur_account_email);

    @Query("DELETE FROM repos where repo_id IN (:ids)")
    Completable deleteAllByIds(List<String> ids);

    @Query("DELETE FROM repos")
    Completable deleteAll();
}
