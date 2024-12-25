package com.seafile.seadroid2.framework.data.db.dao;

import androidx.room.Dao;
import androidx.room.Delete;
import androidx.room.Insert;
import androidx.room.OnConflictStrategy;
import androidx.room.Query;
import androidx.room.RawQuery;
import androidx.room.Update;
import androidx.sqlite.db.SupportSQLiteQuery;

import com.seafile.seadroid2.framework.data.db.entities.FileTransferEntity;
import com.seafile.seadroid2.enums.TransferAction;
import com.seafile.seadroid2.enums.TransferDataSource;
import com.seafile.seadroid2.enums.TransferResult;
import com.seafile.seadroid2.enums.TransferStatus;

import java.util.List;

import io.reactivex.Completable;
import io.reactivex.Single;

@Dao
public interface FileTransferDAO {

    @Insert(onConflict = OnConflictStrategy.REPLACE)
    void insertAll(List<FileTransferEntity> list);

    /**
     * insert or replace
     */
    @Insert(onConflict = OnConflictStrategy.REPLACE)
    void insert(FileTransferEntity entity);

    @Query("DELETE FROM file_transfer_list  where related_account = :related_account and transfer_action = :transfer_action")
    void deleteAllByAction(String related_account, TransferAction transfer_action);

    @Query("DELETE FROM file_transfer_list where related_account = :related_account and transfer_action = :transfer_action")
    Completable deleteAllByActionAsync(String related_account, TransferAction transfer_action);

    @Query("DELETE FROM file_transfer_list where uid = :uid")
    Completable deleteAsyncById(String uid);

    @Query("DELETE FROM file_transfer_list where related_account = :related_account and data_source in ( :feats ) ")
    Completable deleteByFeat(String related_account, List<TransferDataSource> feats);

    @Query("DELETE FROM file_transfer_list")
    void deleteAll();

    @Delete
    void deleteOne(FileTransferEntity entity);

    @Delete
    Single<Integer> deleteOneAsync(FileTransferEntity entity);

    @Update
    void update(FileTransferEntity entity);

    @Update
    Completable updateAsync(FileTransferEntity entity);


    @Update(onConflict = OnConflictStrategy.REPLACE)
    void update(List<FileTransferEntity> entities);

    /**
     * Modify the task that is being in IN_PROGRESS to CANCELLED
     */
    @Query("update file_transfer_list set transfer_result = 'USER_CANCELLED', transfer_status = 'CANCELLED' where related_account = :related_account and data_source in ( :feats ) and transfer_status in ('IN_PROGRESS','WAITING')")
    Completable cancelByDataSource(String related_account, List<TransferDataSource> feats);

    @Query("update file_transfer_list set transfer_result = 'CANCELLED', transfer_status = 'CANCELLED', transfer_result = :result where related_account = :related_account and data_source = :dataSource and transfer_status in ('IN_PROGRESS','WAITING')")
    void cancel(String related_account, TransferDataSource dataSource, TransferResult result);

    @Query("update file_transfer_list set transfer_status = 'CANCELLED', transfer_result = :result where data_source = 'FILE_BACKUP' and transfer_status in ('IN_PROGRESS','WAITING')")
    void cancelWithFileBackup(TransferResult result);

    @Query("update file_transfer_list set transfer_status = 'CANCELLED', transfer_result = 'CANCELLED', data_status = -1, transferred_size = 0 where data_source in ('FILE_BACKUP','FOLDER_BACKUP')")
    void cancelAllWithFileBackup();

    @Query("select * from file_transfer_list where related_account = :related_account and data_source in ( :feats ) and data_status = 0  order by created_at asc")
    Single<List<FileTransferEntity>> getListByFeatAsync(String related_account, List<TransferDataSource> feats);

    @Query("select * from file_transfer_list where related_account = :related_account and transfer_action = :transfer_action and is_auto_transfer = 1 and transfer_status in ('IN_PROGRESS', 'WAITING') and data_source = :feature and data_status = 0  order by created_at asc limit 1")
    List<FileTransferEntity> getOnePendingTransferSync(String related_account, TransferAction transfer_action, TransferDataSource feature);

    @Query("select COUNT(*) from file_transfer_list where related_account = :related_account and transfer_action = :transfer_action and is_auto_transfer = 1 and transfer_status in ('IN_PROGRESS', 'WAITING') and data_source = :feature and data_status = 0")
    int countPendingTransferSync(String related_account, TransferAction transfer_action, TransferDataSource feature);


    @Query("select * from file_transfer_list where related_account = :related_account and transfer_action = :transfer_action and is_auto_transfer = 1 and transfer_status in ('IN_PROGRESS', 'WAITING') and data_status = 0 order by created_at asc limit :limit offset :offset")
    List<FileTransferEntity> getPagePendingListTransferSync(String related_account, TransferAction transfer_action, int limit, int offset);

    @Query("select * from file_transfer_list where transfer_action = :transfer_action and is_auto_transfer = 1 and transfer_status in ('IN_PROGRESS', 'WAITING') and data_source = :feature and data_status = 0  order by created_at asc limit 1")
    List<FileTransferEntity> getOnePendingTransferAllAccountSync(TransferAction transfer_action, TransferDataSource feature);

    @Query("select * from file_transfer_list where related_account = :related_account and is_auto_transfer = 1 and transfer_action = 'DOWNLOAD' and transfer_status in ('IN_PROGRESS', 'WAITING') and data_status = 0  order by created_at asc limit 1")
    List<FileTransferEntity> getOnePendingDownloadByActionSync(String related_account);

    @Query("select COUNT(*)  from file_transfer_list where related_account = :related_account and is_auto_transfer = 1 and transfer_action = 'DOWNLOAD' and transfer_status in ('IN_PROGRESS', 'WAITING') and data_status = 0")
    int countPendingDownloadListSync(String related_account);


    @Query("select * from file_transfer_list where related_account = :related_account and transfer_action = 'UPLOAD' and data_source in ('FOLDER_BACKUP','FILE_BACKUP','ALBUM_BACKUP') and data_status = 0 order by modified_at desc limit :limit offset :offset")
    List<FileTransferEntity> getPageUploadListSync(String related_account, int limit, int offset);

    @Query("select * from file_transfer_list where related_account = :related_account and transfer_action = 'DOWNLOAD' and data_status = 0  order by modified_at desc limit :limit offset :offset")
    List<FileTransferEntity> getPageDownloadListSync(String related_account, int limit, int offset);

    @Query("select * from file_transfer_list where related_account = :related_account and transfer_action = :transferAction and target_path = :target_path and data_status = 0  order by created_at desc limit 1")
    List<FileTransferEntity> getByTargetPathSync(String related_account, TransferAction transferAction, String target_path);


    @Query("select * from file_transfer_list where uid = :uid")
    List<FileTransferEntity> getByUid(String uid);

    @Query("select * from file_transfer_list where uid in ( :uids )")
    List<FileTransferEntity> getListByUidsSync(List<String> uids);

    @Query("select * from file_transfer_list where repo_id = :repoId and full_path IN(:fullPaths) and transfer_action = :transfer_action order by created_at asc")
    Single<List<FileTransferEntity>> getListByFullPathsAsync(String repoId, List<String> fullPaths, TransferAction transfer_action);

    @Query("select * from file_transfer_list where repo_id = :repoId and transfer_action = 'DOWNLOAD' and transfer_status = 'SUCCEEDED' and parent_path = :parent_path order by created_at asc")
    Single<List<FileTransferEntity>> getDownloadedListByParentAsync(String repoId, String parent_path);


    @Query("select * from file_transfer_list where repo_id = :repoId and full_path IN(:fullPaths) and transfer_action = :transfer_action order by created_at asc")
    List<FileTransferEntity> getListByFullPathsSync(String repoId, List<String> fullPaths, TransferAction transfer_action);

    @Query("select * from file_transfer_list where repo_id = :repoId and transfer_action = :transfer_action order by created_at asc limit :limit offset :offset")
    List<FileTransferEntity> getPageListSync(String repoId, TransferAction transfer_action, int limit, int offset);

    @Query("select * from file_transfer_list where repo_id = :repoId and transfer_action = :transferAction and full_path = :full_path and data_status = 0  order by created_at")
    List<FileTransferEntity> getListByFullPathSync(String repoId, TransferAction transferAction, String full_path);

    @Query("select * from file_transfer_list where repo_id = :repoId and transfer_action = :transferAction and full_path = :full_path and data_status = 0  order by created_at")
    Single<List<FileTransferEntity>> getListByFullPathAsync(String repoId, TransferAction transferAction, String full_path);

    @Query("select COUNT(*) from file_transfer_list where repo_id = :repoId and full_path = :fullPath and transfer_action = :transfer_action and data_source = :feature and data_status = 0 ")
    int checkOneByFullPath(String repoId, String fullPath, TransferAction transfer_action, TransferDataSource feature);


    @RawQuery
    int updateEntityStatus(SupportSQLiteQuery query);

    @Query("select COUNT(*) from file_transfer_list where related_account = :related_account and transfer_action = :transferAction and data_source = :feature and transfer_status in (:transferStatus) and data_status = 0  order by created_at asc")
    Single<Integer> getCount(String related_account, TransferAction transferAction, TransferDataSource feature, List<TransferStatus> transferStatus);
}
