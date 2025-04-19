package com.seafile.seadroid2.framework.data.db.dao;

import androidx.room.Dao;
import androidx.room.Delete;
import androidx.room.Insert;
import androidx.room.OnConflictStrategy;
import androidx.room.Query;
import androidx.room.RawQuery;
import androidx.room.Update;
import androidx.sqlite.db.SupportSQLiteQuery;

import com.seafile.seadroid2.enums.TransferDataSource;
import com.seafile.seadroid2.framework.data.db.entities.FileBackupStatusEntity;

import java.util.List;

import io.reactivex.Completable;
import io.reactivex.Single;

@Dao
public interface FileTransferDAO {

    @Insert(onConflict = OnConflictStrategy.REPLACE)
    void insertAll(List<FileBackupStatusEntity> list);

    /**
     * insert or replace
     */
    @Insert(onConflict = OnConflictStrategy.REPLACE)
    void insert(FileBackupStatusEntity entity);

    @Query("DELETE FROM file_backup_status where uid = :uid")
    Completable deleteAsyncById(String uid);

    @Query("DELETE FROM file_backup_status where related_account = :related_account and data_source in ( :feats ) ")
    Completable deleteByFeat(String related_account, List<TransferDataSource> feats);

    @Query("DELETE FROM file_backup_status")
    void deleteAll();

    @Delete
    void deleteOne(FileBackupStatusEntity entity);

    @Delete
    void deleteMultiple(List<FileBackupStatusEntity> entity);

    @Delete
    Single<Integer> deleteOneAsync(FileBackupStatusEntity entity);

    @Update
    void update(FileBackupStatusEntity entity);

    @Update
    Completable updateAsync(FileBackupStatusEntity entity);


    @Update(onConflict = OnConflictStrategy.REPLACE)
    void update(List<FileBackupStatusEntity> entities);

    /////////////////////
    /////// transfer list
    /////////////////////
    @Query("select * from file_backup_status where uid = :uid")
    List<FileBackupStatusEntity> getByUid(String uid);

    @Query("select * from file_backup_status where uid in ( :uids )")
    List<FileBackupStatusEntity> getListByUidsSync(List<String> uids);

    @Query("select * from file_backup_status where repo_id = :repoId and data_source = :dataSource and full_path = :full_path and data_status = 0 order by created_at")
    List<FileBackupStatusEntity> getListByFullPathSync(String repoId, TransferDataSource dataSource, String full_path);

    @Query("select * from file_backup_status where related_account = :related_account and data_source in ('FOLDER_BACKUP','FILE_BACKUP','ALBUM_BACKUP') and data_status = 0 order by created_at desc limit :limit offset :offset")
    Single<List<FileBackupStatusEntity>> getPageUploadListAsync(String related_account, int limit, int offset);

    @Query("select * from file_backup_status where related_account = :related_account and target_path = :target_path and data_status = 0 order by created_at desc limit 1")
    List<FileBackupStatusEntity> getByTargetPathSync(String related_account, String target_path);

    @Query("select * from file_backup_status where repo_id = :repoId and data_source = :dataSource and (parent_path = :parentPath OR parent_path LIKE :parentPath || '%') order by created_at")
    List<FileBackupStatusEntity> getFullListByParentPathSync(String repoId, String parentPath, TransferDataSource dataSource);

    @RawQuery
    int updateEntityStatus(SupportSQLiteQuery query);

    @Query("UPDATE file_backup_status SET repo_name = :newRepoName WHERE repo_id = :repoId")
    void updateRepoNameByRepoId(String repoId, String newRepoName);

}
