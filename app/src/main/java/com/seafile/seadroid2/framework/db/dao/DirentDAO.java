package com.seafile.seadroid2.framework.db.dao;

import androidx.room.Dao;
import androidx.room.Delete;
import androidx.room.Insert;
import androidx.room.OnConflictStrategy;
import androidx.room.Query;
import androidx.room.Update;

import com.seafile.seadroid2.framework.db.entities.DirentModel;
import com.seafile.seadroid2.framework.model.dirents.CachedDirentModel;

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
    Single<List<DirentModel>> getListByParentPathBySqlAsync(String repo_id, String parent_dir);



    @Query("select * from dirents where parent_dir = :parent_dir and repo_id = :repo_id")
    List<DirentModel> getListByParentPath(String repo_id, String parent_dir);

    @Query("select * from dirents where full_path = :full_path and repo_id = :repo_id and type = :type limit 1")
    List<DirentModel> getSpecialDirent(String repo_id, String full_path,String type);


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

    @Query("UPDATE dirents SET repo_name = :newRepoName WHERE repo_id = :repoId")
    void updateRepoNameByRepoId(String repoId, String newRepoName);



    String d_sql = """
                SELECT d.v,d.data_status,d.uid,d.full_path,d.name,d.parent_dir,d.id,d.type,d.mtime,d.permission,d.starred,
                d.dir_id,d.related_account,d.repo_id,d.repo_name,d.size,d.is_locked,d.is_freezed,d.locked_by_me,d.lock_time,
                d.lock_owner,d.lock_owner_name,d.lock_owner_contact_email,
                d.modifier_email,
                d.modifier_name,
                d.modifier_contact_email,
                d.encoded_thumbnail_src,
                d.last_modified_at,
                d.transfer_status,

                f.file_id AS local_file_id

            FROM dirents d
            LEFT JOIN file_cache_status f
            ON d.repo_id = f.repo_id AND d.full_path = f.full_path
            WHERE d.parent_dir = :parent_dir and d.repo_id = :repo_id
            """;

    @Query(d_sql)
    Single<List<CachedDirentModel>> getDirentsWithLocalFileId(String repo_id, String parent_dir);

    @Query(d_sql)
    List<CachedDirentModel> getDirentsWithLocalFileIdSync(String repo_id, String parent_dir);
}
