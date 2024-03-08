package com.seafile.seadroid2.data.db.dao;

import androidx.room.Dao;
import androidx.room.Insert;
import androidx.room.OnConflictStrategy;
import androidx.room.Query;
import androidx.room.Update;

import com.seafile.seadroid2.data.db.entities.FileTransferEntity;
import com.seafile.seadroid2.data.model.enums.TransferAction;
import com.seafile.seadroid2.data.model.enums.TransferFeature;
import com.seafile.seadroid2.data.model.enums.TransferStatus;

import java.util.List;

import io.reactivex.Completable;
import io.reactivex.Single;

@Dao
public interface FileTransferDAO {

    @Insert(onConflict = OnConflictStrategy.REPLACE)
    void insertAll(List<FileTransferEntity> list);

    @Insert(onConflict = OnConflictStrategy.REPLACE)
    void insert(FileTransferEntity entity);

    @Query("DELETE FROM file_transfer_list where transfer_action = :transfer_action")
    void deleteAllByAction(TransferAction transfer_action);

    @Query("DELETE FROM file_transfer_list where transfer_action = :transfer_action")
    Completable deleteAllByActionAsync(TransferAction transfer_action);

    @Query("DELETE FROM file_transfer_list where uid = :uid")
    Completable deleteAsyncById(String uid);

    @Query("DELETE FROM file_transfer_list")
    void deleteAll();

    @Update
    void update(FileTransferEntity entity);

    @Query("select * from file_transfer_list where related_account = :related_account and transfer_action = :transfer_action and transfer_status = :transfer_status order by created_at asc")
    List<FileTransferEntity> getListByAction(String related_account, TransferAction transfer_action, TransferStatus transfer_status);

    @Query("select * from file_transfer_list where related_account = :related_account and transfer_action = :transfer_action and transfer_status = :transfer_status and data_source = :feature order by created_at asc")
    List<FileTransferEntity> getListByAction(String related_account, TransferAction transfer_action, TransferStatus transfer_status,TransferFeature feature);


    @Query("select * from file_transfer_list where related_account = :related_account and transfer_action = :transfer_action order by created_at desc")
    Single<List<FileTransferEntity>> getListByActionAsync(String related_account, TransferAction transfer_action);


    @Query("select * from file_transfer_list where uid = :uid")
    FileTransferEntity getByTransferId(String uid);

    @Query("select * from file_transfer_list where related_account = :related_account and full_path IN(:fullPaths) and transfer_action = :transfer_action order by created_at asc")
    List<FileTransferEntity> getListByFullPaths(String related_account, List<String> fullPaths, TransferAction transfer_action);

    @Query("select * from file_transfer_list where related_account = :related_account and full_path IN(:fullPaths) and transfer_action = :transfer_action order by created_at asc")
    Single<List<FileTransferEntity>> getListByFullPathsAsync(String related_account, List<String> fullPaths, TransferAction transfer_action);


    @Query("select * from file_transfer_list where related_account = :related_account and full_path = :fullPath and transfer_action = :transfer_action order by created_at asc")
    FileTransferEntity getOneByFullPath(String related_account, String fullPath, TransferAction transfer_action);

    @Query("select COUNT(*) from file_transfer_list where related_account = :related_account and full_path = :fullPath and transfer_action = :transfer_action and data_source = :feature")
    int checkOneByFullPath(String related_account, String fullPath, TransferAction transfer_action, TransferFeature feature);
}
