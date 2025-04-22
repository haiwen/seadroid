package com.seafile.seadroid2.ui.transfer_list;

import androidx.lifecycle.MutableLiveData;

import com.blankj.utilcode.util.CollectionUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.enums.TransferAction;
import com.seafile.seadroid2.framework.db.AppDatabase;
import com.seafile.seadroid2.framework.db.entities.FileBackupStatusEntity;
import com.seafile.seadroid2.framework.model.GroupItemModel;
import com.seafile.seadroid2.framework.worker.queue.TransferModel;
import com.seafile.seadroid2.framework.worker.GlobalTransferCacheList;
import com.seafile.seadroid2.ui.base.viewmodel.BaseViewModel;

import java.util.ArrayList;
import java.util.List;

import io.reactivex.Single;

public class TransferListViewModel extends BaseViewModel {

    private Account account;
    private final MutableLiveData<List<Object>> mTransferListLiveData = new MutableLiveData<>();

    public MutableLiveData<List<Object>> getTransferListLiveData() {
        return mTransferListLiveData;
    }

    public void loadData(TransferAction transferAction, int pageIndex, int pageSize) {
        getRefreshLiveData().setValue(true);
        List<Object> objects = new ArrayList<>();

        //
        if (transferAction == TransferAction.UPLOAD) {
            objects.add(new GroupItemModel(R.string.uploading));

            List<TransferModel> mediaList = GlobalTransferCacheList.ALBUM_BACKUP_QUEUE.getSortedTransferMapList();
            List<TransferModel> folderList = GlobalTransferCacheList.FOLDER_BACKUP_QUEUE.getSortedTransferMapList();
            List<TransferModel> fileList = GlobalTransferCacheList.FILE_UPLOAD_QUEUE.getSortedTransferMapList();
            objects.addAll(mediaList);
            objects.addAll(folderList);
            objects.addAll(fileList);
        } else {
            objects.add(new GroupItemModel(R.string.downloading));
            List<TransferModel> downloadList = GlobalTransferCacheList.DOWNLOAD_QUEUE.getSortedTransferMapList();
            objects.addAll(downloadList);
        }


        //remove upload category
        if (objects.size() == 1) {
            objects.clear();
        }

        if (account == null) {
            account = SupportAccountManager.getInstance().getCurrentAccount();
        }

        if (account == null) {
            getRefreshLiveData().setValue(false);
            return;
        }

        if (TransferAction.DOWNLOAD == transferAction) {
            getTransferListLiveData().setValue(objects);
            getRefreshLiveData().setValue(false);
            return;
        }

        int offset = (pageIndex - 1) * pageSize;

        Single<List<FileBackupStatusEntity>> single = AppDatabase
                .getInstance()
                .fileTransferDAO()
                .getPageUploadListAsync(account.getSignature(), pageSize, offset);
        addSingleDisposable(single, list -> {
            if (!CollectionUtils.isEmpty(list)) {
//                if (transferAction == TransferAction.UPLOAD) {
                objects.add(new GroupItemModel(R.string.upload_finished));
//                } else {
//                    objects.add(new GroupItemModel(R.string.download_finished));
//                }
            }
            objects.addAll(list);
            getTransferListLiveData().setValue(objects);
            getRefreshLiveData().setValue(false);
        });
    }


//
//    public void cancelAllUploadTask(Consumer<Boolean> consumer) {
//        Account account = SupportAccountManager.getInstance().getCurrentAccount();
//        List<TransferDataSource> dataSources = CollectionUtils.newArrayList(TransferDataSource.ALBUM_BACKUP, TransferDataSource.FILE_BACKUP, TransferDataSource.FOLDER_BACKUP);
//        Completable completable = AppDatabase.getInstance().fileTransferDAO().cancelAllByDataSource(account.getSignature(), dataSources, SeafException.USER_CANCELLED_EXCEPTION.getMessage());
//        addCompletableDisposable(completable, new Action() {
//            @Override
//            public void run() throws Exception {
//                consumer.accept(true);
//            }
//        });
//    }
//
//    public void cancelAllDownloadTask(Consumer<Boolean> consumer) {
//        Account account = SupportAccountManager.getInstance().getCurrentAccount();
//        if (account == null) {
//            return;
//        }
//
//        List<TransferDataSource> dataSources = CollectionUtils.newArrayList(TransferDataSource.DOWNLOAD);
//        Completable completable = AppDatabase.getInstance().fileTransferDAO().cancelAllByDataSource(account.getSignature(), dataSources, SeafException.USER_CANCELLED_EXCEPTION.getMessage());
//        addCompletableDisposable(completable, new Action() {
//            @Override
//            public void run() throws Exception {
//                consumer.accept(true);
//            }
//        });
//    }


//    public void removeAllDownloadTask(Consumer<Boolean> consumer, boolean isDeleteLocalFile) {
//        getRefreshLiveData().setValue(true);
//
//        Account account = SupportAccountManager.getInstance().getCurrentAccount();
//        if (account == null) {
//            getRefreshLiveData().setValue(false);
//            return;
//        }
//
//        List<TransferDataSource> features = CollectionUtils.newArrayList(TransferDataSource.DOWNLOAD);
//        //query all data, including deleted data, based on different users
//        Single<List<FileTransferEntity>> single = AppDatabase.getInstance().fileTransferDAO().getListByDataSourceAsync(account.getSignature(), features);
//
//        Single<Boolean> single1 = single.flatMap(new Function<List<FileTransferEntity>, SingleSource<Boolean>>() {
//            @Override
//            public SingleSource<Boolean> apply(List<FileTransferEntity> transferList) throws Exception {
//                return Single.create(new SingleOnSubscribe<Boolean>() {
//                    @Override
//                    public void subscribe(SingleEmitter<Boolean> emitter) throws Exception {
//                        if (isDeleteLocalFile) {
//                            for (FileTransferEntity entity : transferList) {
//                                FileUtils.delete(entity.target_path);
//                                SLogs.d("deleted : " + entity.target_path);
//                            }
//                        }
//
//                        AppDatabase.getInstance().fileTransferDAO().deleteAllByAction(account.getSignature(), TransferAction.DOWNLOAD);
//
//                        emitter.onSuccess(true);
//                    }
//                });
//            }
//        });
//
//        addSingleDisposable(single1, new Consumer<Boolean>() {
//            @Override
//            public void accept(Boolean b) throws Exception {
//                if (consumer != null) {
//                    consumer.accept(true);
//                }
//            }
//        });
//    }

//    public void removeSpecialUploadListTask(List<FileTransferEntity> list, Consumer<Boolean> consumer) {
//        Single<Boolean> single = Single.create(new SingleOnSubscribe<Boolean>() {
//            @Override
//            public void subscribe(SingleEmitter<Boolean> emitter) throws Exception {
//
//                for (FileTransferEntity entity : list) {
//                    entity.data_status = -1;
//                    entity.transfer_status = TransferStatus.CANCELLED;
//                    entity.result = SeafException.USER_CANCELLED_EXCEPTION.getMessage();
//                    entity.transferred_size = 0;
//
//                    AppDatabase.getInstance().fileTransferDAO().update(entity);
//                }
//
//                emitter.onSuccess(true);
//            }
//        });
//
//        addSingleDisposable(single, new Consumer<Boolean>() {
//            @Override
//            public void accept(Boolean aBoolean) throws Exception {
//                if (consumer != null) {
//                    consumer.accept(true);
//                }
//            }
//        });
//    }

//    public void removeAllUploadTask(Consumer<Boolean> consumer) {
//        getShowLoadingDialogLiveData().setValue(true);
//
//        Account account = SupportAccountManager.getInstance().getCurrentAccount();
//
//        Completable completable = AppDatabase.getInstance().fileTransferDAO().removeAllUploadByAccount(account.getSignature(), SeafException.USER_CANCELLED_EXCEPTION.getMessage());
//        addCompletableDisposable(completable, new Action() {
//            @Override
//            public void run() throws Exception {
//                if (consumer != null) {
//                    consumer.accept(true);
//                }
//
//                getShowLoadingDialogLiveData().setValue(false);
//            }
//        });
//
//    }
//
//    public void restartSpecialStatusTask(TransferAction transferAction, TransferStatus transferStatus, Consumer<Boolean> consumer) {
//        getShowLoadingDialogLiveData().setValue(true);
//
//        Account account = SupportAccountManager.getInstance().getCurrentAccount();
//        if (account == null) {
//            getShowLoadingDialogLiveData().setValue(false);
//            return;
//        }
//
//        Single<List<FileTransferEntity>> single = AppDatabase.getInstance().fileTransferDAO().getByActionAndStatusAsync(account.getSignature(), transferAction, transferStatus);
//        Single<Boolean> single1 = single.flatMap(new Function<List<FileTransferEntity>, SingleSource<Boolean>>() {
//            @Override
//            public SingleSource<Boolean> apply(List<FileTransferEntity> list) throws Exception {
//
//                if (CollectionUtils.isEmpty(list)) {
//                    return Single.just(false);
//                }
//
//                return Single.create(new SingleOnSubscribe<Boolean>() {
//                    @Override
//                    public void subscribe(SingleEmitter<Boolean> emitter) throws Exception {
//
//                        for (FileTransferEntity entity : list) {
//                            if (transferAction == TransferAction.DOWNLOAD) {
//                                FileUtils.delete(entity.target_path);
//                                SLogs.d("deleted : " + entity.target_path);
//                            }
//
//                            entity.transfer_status = TransferStatus.WAITING;
//                            entity.result = null;
//                            entity.transferred_size = 0;
//                            entity.action_end_at = 0;
//
//                            AppDatabase.getInstance().fileTransferDAO().update(entity);
//                        }
//                        emitter.onSuccess(true);
//                    }
//                });
//            }
//        });
//
//        addSingleDisposable(single1, new Consumer<Boolean>() {
//            @Override
//            public void accept(Boolean aBoolean) throws Exception {
//                if (consumer != null) {
//                    consumer.accept(aBoolean);
//                }
//                getShowLoadingDialogLiveData().setValue(false);
//            }
//        });
//
//    }
//
//    public void restartUpload(List<FileTransferEntity> list, Consumer<Boolean> consumer) {
//        Single<Boolean> single = Single.create(new SingleOnSubscribe<Boolean>() {
//            @Override
//            public void subscribe(SingleEmitter<Boolean> emitter) throws Exception {
//
//                for (FileTransferEntity entity : list) {
//                    if (entity.transfer_status == TransferStatus.WAITING) {
//                        continue;
//                    }
//
//                    entity.transfer_status = TransferStatus.WAITING;
//                    entity.result = null;
//                    entity.transferred_size = 0;
//                    entity.action_end_at = 0;
//
//                    AppDatabase.getInstance().fileTransferDAO().update(entity);
//                }
//
//                emitter.onSuccess(true);
//            }
//        });
//        addSingleDisposable(single, new Consumer<Boolean>() {
//            @Override
//            public void accept(Boolean aBoolean) throws Exception {
//                if (consumer != null) {
//                    consumer.accept(true);
//                }
//            }
//        });
//
//    }
//
//    public void restartDownload(List<FileTransferEntity> list, Consumer<Boolean> consumer) {
//        Single<Boolean> single = Single.create(new SingleOnSubscribe<Boolean>() {
//            @Override
//            public void subscribe(SingleEmitter<Boolean> emitter) throws Exception {
//
//                for (FileTransferEntity entity : list) {
//                    if (entity.transfer_status == TransferStatus.WAITING) {
//                        continue;
//                    }
//
//                    if (entity.transfer_action == TransferAction.DOWNLOAD) {
//                        FileUtils.delete(entity.target_path);
//                        SLogs.d("deleted : " + entity.target_path);
//                    }
//
//                    entity.transfer_status = TransferStatus.WAITING;
//                    entity.result = null;
//                    entity.transferred_size = 0;
//                    entity.action_end_at = 0;
//
//                    AppDatabase.getInstance().fileTransferDAO().update(entity);
//                }
//
//                emitter.onSuccess(true);
//            }
//        });
//
//        addSingleDisposable(single, new Consumer<Boolean>() {
//            @Override
//            public void accept(Boolean aBoolean) throws Exception {
//                if (consumer != null) {
//                    consumer.accept(true);
//                }
//            }
//        });
//    }
}
