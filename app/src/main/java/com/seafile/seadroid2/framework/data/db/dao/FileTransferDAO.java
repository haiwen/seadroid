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
    void deleteMultiple(List<FileTransferEntity> entity);

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
    @Query("update file_transfer_list set transfer_status = 'CANCELLED', result = :result where related_account = :related_account and data_source in ( :feats ) and transfer_status in ('IN_PROGRESS','WAITING')")
    Completable cancelAllByDataSource(String related_account, List<TransferDataSource> feats, String result);

    /**
     * remove all tasks for ['FILE_BACKUP','FOLDER_BACKUP','ALBUM_BACKUP']
     */
    @Query("update file_transfer_list set transfer_status = 'CANCELLED', result = :result, data_status = -1, transferred_size = 0 where related_account = :related_account and data_source in ('FILE_BACKUP','FOLDER_BACKUP','ALBUM_BACKUP')")
    Completable removeAllUploadByAccount(String related_account, String result);

    @Query("select * from file_transfer_list where related_account = :related_account and data_source in ( :feats ) order by created_at asc")
    Single<List<FileTransferEntity>> getListByDataSourceAsync(String related_account, List<TransferDataSource> feats);

    @Query("select * from file_transfer_list where related_account = :related_account and transfer_action = :transferAction and transfer_status = :transferStatus and data_status = 0 order by created_at")
    Single<List<FileTransferEntity>> getByActionAndStatusAsync(String related_account, TransferAction transferAction, TransferStatus transferStatus);


    /////////////////////
    /////// transfer list
    /////////////////////
    @Query("select * from file_transfer_list where related_account = :related_account and transfer_action = 'UPLOAD' and data_source in ('FOLDER_BACKUP','FILE_BACKUP','ALBUM_BACKUP') and data_status = 0 order by created_at desc limit :limit offset :offset")
    Single<List<FileTransferEntity>> getPageUploadListAsync(String related_account, int limit, int offset);

    @Query("select * from file_transfer_list where related_account = :related_account and transfer_action = 'DOWNLOAD' and data_status = 0 order by created_at desc limit :limit offset :offset")
    Single<List<FileTransferEntity>> getPageDownloadListAsync(String related_account, int limit, int offset);


    @Query("select * from file_transfer_list where related_account = :related_account and transfer_action = :transferAction and target_path = :target_path and data_status = 0 order by created_at desc limit 1")
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

    @Query("select * from file_transfer_list where repo_id = :repoId and transfer_action = :transferAction and full_path = :full_path and data_status = 0 order by created_at")
    Single<List<FileTransferEntity>> getListByFullPathAsync(String repoId, TransferAction transferAction, String full_path);

    @Query("select * from file_transfer_list where repo_id = :repoId and data_source = :dataSource and full_path = :full_path and data_status = 0 order by created_at")
    List<FileTransferEntity> getListByFullPathSync(String repoId, TransferDataSource dataSource, String full_path);

    @Query("select * from file_transfer_list where repo_id = :repoId and data_source = :dataSource and target_path = :targetPath and data_status = 0 order by created_at")
    List<FileTransferEntity> getListByTargetPathSync(String repoId, TransferDataSource dataSource, String targetPath);


    /**
     * Get all the data, including the deleted data and child dir data, under the specified parent directory
     */
    @Query("select * from file_transfer_list where repo_id = :repoId and data_source = :dataSource and (parent_path = :parentPath OR parent_path LIKE :parentPath || '%') order by created_at")
    List<FileTransferEntity> getFullListByParentPathSync(String repoId, String parentPath, TransferDataSource dataSource);


    @Query("select COUNT(*) from file_transfer_list where repo_id = :repoId and full_path = :fullPath and transfer_action = :transfer_action and data_source = :feature and data_status = 0 ")
    int checkOneByFullPath(String repoId, String fullPath, TransferAction transfer_action, TransferDataSource feature);

    @RawQuery
    int updateEntityStatus(SupportSQLiteQuery query);

    @Query("select COUNT(*) from file_transfer_list where related_account = :related_account and transfer_action = :transferAction and data_source = :feature and transfer_status in (:transferStatus) and data_status = 0  order by created_at asc")
    Single<Integer> getCount(String related_account, TransferAction transferAction, TransferDataSource feature, List<TransferStatus> transferStatus);

    /////////////////////
    /////// pending list
    /////////////////////
    // FAILED
    @Query("select * from file_transfer_list where related_account = :related_account and is_auto_transfer = 1 and transfer_status in ('FAILED') and data_source = :feature and data_status = 0 order by created_at asc")
    List<FileTransferEntity> getOneFailedPendingTransferSync(String related_account, TransferDataSource feature);

    // WAITING, IN_PROGRESS
    @Query("select * from file_transfer_list where related_account = :related_account and is_auto_transfer = 1 and transfer_status in ('WAITING', 'IN_PROGRESS') and data_source = :feature and data_status = 0 order by created_at asc limit 1")
    List<FileTransferEntity> getOnePendingTransferSync(String related_account, TransferDataSource feature);

    @Query("select * from file_transfer_list where is_auto_transfer = 1 and transfer_status in ('IN_PROGRESS', 'WAITING') and data_source = :feature and data_status = 0 order by created_at asc limit 1")
    List<FileTransferEntity> getOnePendingTransferAllAccountSync(TransferDataSource feature);

    @Query("select * from file_transfer_list where is_auto_transfer = 1 and transfer_status in ('FAILED') and data_source = :feature and data_status = 0 order by created_at asc")
    List<FileTransferEntity> getOnePendingFailedTransferAllAccountSync(TransferDataSource feature);

    @Query("select * from file_transfer_list where related_account = :related_account and is_auto_transfer = 1 and transfer_action = 'DOWNLOAD' and transfer_status in ('IN_PROGRESS', 'WAITING') and data_status = 0  order by created_at asc limit 1")
    List<FileTransferEntity> getOnePendingDownloadByAccountSync(String related_account);

    @Query("select * from file_transfer_list where related_account = :related_account and is_auto_transfer = 1 and transfer_action = 'DOWNLOAD' and transfer_status in ('FAILED') and data_status = 0  order by created_at asc limit 1")
    List<FileTransferEntity> getOnePendingFailedDownloadByAccountSync(String related_account);

    //////////////
    /////// count
    //////////////
    @Query("select COUNT(*) from file_transfer_list where related_account = :related_account and is_auto_transfer = 1 and transfer_status not in ('SUCCEEDED', 'CANCELLED') and data_source = :feature and data_status = 0")
    int countPendingTransferSync(String related_account, TransferDataSource feature);

    @Query("select COUNT(*) from file_transfer_list where is_auto_transfer = 1 and transfer_status not in ('SUCCEEDED', 'CANCELLED') and data_source = :feature and data_status = 0")
    int countPendingTransferSync(TransferDataSource feature);

    @Query("select COUNT(*)  from file_transfer_list where related_account = :related_account and is_auto_transfer = 1 and transfer_action = 'DOWNLOAD' and transfer_status not in ('SUCCEEDED', 'CANCELLED') and data_status = 0")
    int countPendingDownloadListSync(String related_account);

}
