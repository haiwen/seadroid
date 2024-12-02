package com.seafile.seadroid2.framework.data.db.dao;

import androidx.room.Dao;
import androidx.room.Delete;
import androidx.room.Insert;
import androidx.room.OnConflictStrategy;
import androidx.room.Query;
import androidx.room.Update;

import com.seafile.seadroid2.framework.data.db.entities.DirentModel;

import java.util.List;

import io.reactivex.Completable;
import io.reactivex.Single;

@Dao
public interface DirentDAO {
    @Query("select * from dirents where related_account = :related_account")
    Single<List<DirentModel>> getListByAccount(String related_account);

    @Query("select * from dirents where parent_dir = :parent_dir and repo_id = :repo_id")
    Single<List<DirentModel>> getListByParentPathAsync(String repo_id, String parent_dir);

    @Query("select * from dirents where parent_dir = :parent_dir and repo_id = :repo_id")
    List<DirentModel> getListByParentPathSync(String repo_id, String parent_dir);

    @Query("select * from dirents where type='file' and parent_dir = :parent_dir and repo_id = :repo_id")
    Single<List<DirentModel>> getFileListByParentPath(String repo_id, String parent_dir);

    /**
     * get special one by full_path
     */
    @Query("select * from dirents where full_path = :full_path and repo_id = :repo_id")
    Single<List<DirentModel>> getListByFullPathAsync(String repo_id, String full_path);

    @Query("select * from dirents where full_path = :full_path and repo_id = :repo_id limit 1")
    List<DirentModel> getListByFullPathSync(String repo_id, String full_path);

    @Query("select * from dirents where uid in ( :uids )")
    List<DirentModel> getListByIdsSync(List<String> uids);

    @Query("select * from dirents where uid in ( :uids )")
    Single<List<DirentModel>> getListByIdsAsync(List<String> uids);

    @Insert(onConflict = OnConflictStrategy.REPLACE)
    void insert(DirentModel model);

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

    @Delete()
    void delete(DirentModel direntModel);

    @Update
    void update(DirentModel entity);

}
