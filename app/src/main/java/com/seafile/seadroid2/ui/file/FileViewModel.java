package com.seafile.seadroid2.ui.file;

import android.text.TextUtils;
import android.util.Pair;

import androidx.lifecycle.MutableLiveData;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.FileUtils;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.framework.data.db.entities.DirentModel;
import com.seafile.seadroid2.framework.data.model.dirents.DirentFileModel;
import com.seafile.seadroid2.framework.data.db.AppDatabase;
import com.seafile.seadroid2.framework.data.db.entities.FileTransferEntity;
import com.seafile.seadroid2.framework.data.db.entities.RepoModel;
import com.seafile.seadroid2.framework.data.model.enums.TransferAction;
import com.seafile.seadroid2.framework.data.model.enums.TransferResult;
import com.seafile.seadroid2.framework.data.model.enums.TransferStatus;
import com.seafile.seadroid2.framework.datastore.DataManager;
import com.seafile.seadroid2.framework.http.IO;
import com.seafile.seadroid2.framework.worker.ExistingFileStrategy;
import com.seafile.seadroid2.ui.base.viewmodel.BaseViewModel;
import com.seafile.seadroid2.framework.util.TransferUtils;
import com.seafile.seadroid2.framework.worker.BackgroundJobManagerImpl;

import java.io.File;
import java.util.List;

import io.reactivex.Flowable;
import io.reactivex.Single;
import io.reactivex.SingleEmitter;
import io.reactivex.SingleOnSubscribe;
import io.reactivex.functions.Action;
import io.reactivex.functions.BiFunction;
import io.reactivex.functions.Consumer;
import io.reactivex.functions.Function3;
import kotlin.Triple;

public class FileViewModel extends BaseViewModel {
    private final MutableLiveData<Long[]> progressLiveData = new MutableLiveData<>();
    private final MutableLiveData<File> outFileLiveData = new MutableLiveData<>();
    private final MutableLiveData<Boolean> CancelLiveData = new MutableLiveData<>();

    public MutableLiveData<Long[]> getProgressLiveData() {
        return progressLiveData;
    }

    public MutableLiveData<File> getOutFileLiveData() {
        return outFileLiveData;
    }

    public MutableLiveData<Boolean> getCancelLiveData() {
        return CancelLiveData;
    }

    public void loadFileDetail(String repoId, String path, Consumer<Triple<RepoModel, DirentFileModel, FileTransferEntity>> consumer) {

        // get file detail
        Single<DirentFileModel> single = IO.getInstanceWithLoggedIn().execute(FileService.class).getFileDetail(repoId, path);

        //
        Single<List<RepoModel>> repoListSingle = AppDatabase.getInstance().repoDao().getByIdAsync(repoId);

        Single<FileTransferEntity> transferEntity = AppDatabase
                .getInstance()
                .fileTransferDAO()
                .getOneByFullPathAsync(repoId, path, TransferAction.DOWNLOAD);

        Single<Triple<RepoModel, DirentFileModel, FileTransferEntity>> s = Single.zip(single, repoListSingle, transferEntity, new Function3<DirentFileModel, List<RepoModel>, FileTransferEntity, Triple<RepoModel, DirentFileModel, FileTransferEntity>>() {
            @Override
            public Triple<RepoModel, DirentFileModel, FileTransferEntity> apply(DirentFileModel direntFileModel, List<RepoModel> repoModels, FileTransferEntity fileTransferEntity) throws Exception {
                if (CollectionUtils.isEmpty(repoModels)) {
                    throw SeafException.notFoundException;
                }

                return new Triple<>(repoModels.get(0), direntFileModel, fileTransferEntity);
            }
        });

        addSingleDisposable(s, consumer, new Consumer<Throwable>() {
            @Override
            public void accept(Throwable throwable) throws Exception {
                SeafException seafException = getExceptionByThrowable(throwable);
                getSeafExceptionLiveData().setValue(seafException);
            }
        });
    }

    /**
     * Overlay downloads
     */
    public void preDownload(RepoModel repoModel, DirentModel direntModel, File destinationFile) {
        Account account = SupportAccountManager.getInstance().getCurrentAccount();

        Single<Boolean> s = Single.create(new SingleOnSubscribe<Boolean>() {
            @Override
            public void subscribe(SingleEmitter<Boolean> emitter) throws Exception {
                //this is in child thread.

                FileTransferEntity dbEntity = AppDatabase
                        .getInstance()
                        .fileTransferDAO()
                        .getOneByFullPathSync(repoModel.repo_id, direntModel.full_path, TransferAction.DOWNLOAD);

                if (null == dbEntity) {

                    FileTransferEntity transferEntity = FileTransferEntity.convertDirentModel2This(repoModel.encrypted, direntModel);
                    AppDatabase.getInstance().fileTransferDAO().insert(transferEntity);

                    emitter.onSuccess(true);
                    return;
                }


                //re-download
                if (dbEntity.transfer_result == TransferResult.TRANSMITTED) {
                    dbEntity.action_end_at = 0;
                    dbEntity.transfer_result = TransferResult.NO_RESULT;
                    dbEntity.transfer_status = TransferStatus.WAITING;

                    AppDatabase.getInstance().fileTransferDAO().insert(dbEntity);

                    emitter.onSuccess(true);
                    return;
                }

                //in DownloadWorker downloading queue
                if (dbEntity.is_auto_transfer) {
                    dbEntity.is_auto_transfer = false;
                    dbEntity.transfer_result = TransferResult.NO_RESULT;
                    dbEntity.transfer_status = TransferStatus.WAITING;

                    //stop download worker
                    BackgroundJobManagerImpl.getInstance().cancelFilesDownloadJob();
                }

                AppDatabase.getInstance().fileTransferDAO().insert(dbEntity);

                emitter.onSuccess(true);
            }
        });

        addSingleDisposable(s, new Consumer<Boolean>() {
            @Override
            public void accept(Boolean abool) throws Exception {
                download(account, direntModel, destinationFile);
            }
        });
    }

    public void download(Account account, DirentModel direntModel, File destinationFile) {

        Single<String> urlSingle = IO.getInstanceWithLoggedIn().execute(FileService.class).getFileDownloadLinkAsync(direntModel.repo_id, direntModel.full_path);

        addSingleDisposable(urlSingle, new Consumer<String>() {
            @Override
            public void accept(String url) throws Exception {

                File tempFile = DataManager.createTempFile();

                //start download
                Flowable<Long[]> flowable = IO.getInstanceWithLoggedIn().downloadBinary(url, tempFile);
                addFlowableDisposable(flowable, new Consumer<Long[]>() {
                    @Override
                    public void accept(Long[] longs) throws Exception {
                        getProgressLiveData().setValue(longs);
                    }
                }, new Consumer<Throwable>() {
                    @Override
                    public void accept(Throwable throwable) throws Exception {
                        SeafException seafException = getExceptionByThrowable(throwable);
                        getSeafExceptionLiveData().setValue(seafException);

                        updateFileTransferEntity(account, direntModel.repo_id, direntModel.full_path, destinationFile, seafException);
                    }
                }, new Action() {
                    @Override
                    public void run() throws Exception {
                        //delete it if exists.
                        if (destinationFile.exists()) {
                            destinationFile.delete();
                        }

                        //rename
                        tempFile.renameTo(destinationFile);

                        updateFileTransferEntity(account, direntModel.repo_id, direntModel.full_path, destinationFile, null);
                    }
                });
            }
        });
    }

    private void updateFileTransferEntity(Account account, String repo_id, String fullPathInRepo, File destinationFile, SeafException seafException) {
        Single<File> s = Single.create(new SingleOnSubscribe<File>() {
            @Override
            public void subscribe(SingleEmitter<File> emitter) throws Exception {
                //this is in child thread.

                FileTransferEntity dbEntity = AppDatabase
                        .getInstance()
                        .fileTransferDAO()
                        .getOneByFullPathSync(repo_id, fullPathInRepo, TransferAction.DOWNLOAD);

                if (dbEntity != null) {

                    if (seafException != null) {
                        dbEntity.transfer_result = TransferUtils.convertException2TransferResult(seafException);
                        dbEntity.transfer_status = TransferStatus.FAILED;
                        dbEntity.file_md5 = null;
                        dbEntity.transferred_size = 0;
                        dbEntity.target_path = null;
                    } else {
                        dbEntity.transfer_result = TransferResult.TRANSMITTED;
                        dbEntity.transfer_status = TransferStatus.SUCCEEDED;
                        dbEntity.file_md5 = FileUtils.getFileMD5ToString(destinationFile).toLowerCase();
                        dbEntity.transferred_size = destinationFile.length();
                        dbEntity.target_path = destinationFile.getAbsolutePath();
                    }

                    dbEntity.action_end_at = System.currentTimeMillis();
                    dbEntity.file_original_modified_at = dbEntity.action_end_at;

                    AppDatabase.getInstance().fileTransferDAO().update(dbEntity);
                }

                emitter.onSuccess(destinationFile);
            }
        });

        addSingleDisposable(s, new Consumer<File>() {
            @Override
            public void accept(File destinationFile) throws Exception {
                outFileLiveData.setValue(destinationFile);

                //probably already stopped, restart it.
//                BackgroundJobManagerImpl.getInstance().startFileDownloadWorker();
            }
        });
    }

    public void cancelDownload(String repo_id, String fullPathInRepo) {
        disposeAll();

        Single<Boolean> s = Single.create(new SingleOnSubscribe<Boolean>() {
            @Override
            public void subscribe(SingleEmitter<Boolean> emitter) throws Exception {
                //this is in child thread.
                List<FileTransferEntity> transferList = AppDatabase
                        .getInstance()
                        .fileTransferDAO()
                        .getListByFullPathsSync(repo_id, CollectionUtils.newArrayList(fullPathInRepo), TransferAction.DOWNLOAD);

                if (!CollectionUtils.isEmpty(transferList)) {
                    FileTransferEntity entity = transferList.get(0);

                    entity.transfer_result = TransferResult.CANCELLED;
                    entity.transfer_status = TransferStatus.CANCELLED;
                    entity.transferred_size = 0;

                    AppDatabase.getInstance().fileTransferDAO().update(entity);
                }

                emitter.onSuccess(true);
            }
        });

        addSingleDisposable(s, new Consumer<Boolean>() {
            @Override
            public void accept(Boolean b) throws Exception {
                CancelLiveData.setValue(b);
            }
        });

    }

}
