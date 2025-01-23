package com.seafile.seadroid2.ui.main;

import android.content.Context;
import android.net.Uri;
import android.text.TextUtils;
import android.webkit.MimeTypeMap;

import androidx.fragment.app.Fragment;
import androidx.lifecycle.MutableLiveData;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.FileUtils;
import com.blankj.utilcode.util.ToastUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.framework.data.db.entities.EncKeyCacheEntity;
import com.seafile.seadroid2.framework.data.db.entities.FileTransferEntity;
import com.seafile.seadroid2.framework.data.db.entities.PermissionEntity;
import com.seafile.seadroid2.framework.data.model.dirents.DirentFileModel;
import com.seafile.seadroid2.enums.TransferAction;
import com.seafile.seadroid2.enums.TransferDataSource;
import com.seafile.seadroid2.enums.TransferStatus;
import com.seafile.seadroid2.framework.datastore.DataManager;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.framework.worker.ExistingFileStrategy;
import com.seafile.seadroid2.ui.base.viewmodel.BaseViewModel;
import com.seafile.seadroid2.context.NavContext;
import com.seafile.seadroid2.framework.data.ServerInfo;
import com.seafile.seadroid2.framework.data.db.AppDatabase;
import com.seafile.seadroid2.framework.data.db.entities.DirentModel;
import com.seafile.seadroid2.framework.data.db.entities.RepoModel;
import com.seafile.seadroid2.framework.data.model.repo.RepoWrapperModel;
import com.seafile.seadroid2.framework.data.model.server.ServerInfoModel;
import com.seafile.seadroid2.ui.file.FileService;
import com.seafile.seadroid2.ui.repo.RepoService;
import com.seafile.seadroid2.framework.http.HttpIO;
import com.seafile.seadroid2.ui.activities.AllActivitiesFragment;
import com.seafile.seadroid2.ui.repo.RepoQuickFragment;
import com.seafile.seadroid2.ui.settings.TabSettings2Fragment;
import com.seafile.seadroid2.ui.star.StarredQuickFragment;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.worker.BackgroundJobManagerImpl;

import org.apache.commons.io.IOUtils;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import io.reactivex.Single;
import io.reactivex.SingleEmitter;
import io.reactivex.SingleOnSubscribe;
import io.reactivex.functions.Consumer;

public class MainViewModel extends BaseViewModel {
    //    private final MutableLiveData<Pair<String, String>> OnNewFileDownloadLiveData = new MutableLiveData<>();
    private final MutableLiveData<Integer> OnResortListLiveData = new MutableLiveData<>();
    private final MutableLiveData<String> _onSearchLiveData = new MutableLiveData<>();

    //force refresh repo/dirents
    private final MutableLiveData<Boolean> OnForceRefreshRepoListLiveData = new MutableLiveData<>();

    //show swipeRefresh in Repo Fragment
    private final MutableLiveData<Boolean> OnShowRefreshLoadingInRepoLiveData = new MutableLiveData<>();


    private final MutableLiveData<Boolean> OnNavChangeListenerLiveData = new MutableLiveData<>();

    private final MutableLiveData<Boolean> _searchViewExpandedLiveData = new MutableLiveData<>(false);

    public MutableLiveData<Boolean> getSearchViewExpandedLiveData() {
        return _searchViewExpandedLiveData;
    }

    public MutableLiveData<String> getSearchViewQueryLiveData() {
        return _searchViewQueryLiveData;
    }


    private final MutableLiveData<String> _searchViewQueryLiveData = new MutableLiveData<>();
    private final MutableLiveData<ServerInfo> _serverInfoLiveData = new MutableLiveData<>();

//    public MutableLiveData<Pair<String, String>> getOnNewFileDownloadLiveData() {
//        return OnNewFileDownloadLiveData;
//    }


    public MutableLiveData<String> getOnSearchLiveData() {
        return _onSearchLiveData;
    }

    public MutableLiveData<Boolean> getOnForceRefreshRepoListLiveData() {
        return OnForceRefreshRepoListLiveData;
    }

    public MutableLiveData<Boolean> getOnShowRefreshLoadingInRepoLiveData() {
        return OnShowRefreshLoadingInRepoLiveData;
    }

    public MutableLiveData<Boolean> getOnNavContextChangeListenerLiveData() {
        return OnNavChangeListenerLiveData;
    }

    public MutableLiveData<ServerInfo> getServerInfoLiveData() {
        return _serverInfoLiveData;
    }

    public MutableLiveData<Integer> getOnResortListLiveData() {
        return OnResortListLiveData;
    }

    private NavContext navContext = null;

    public NavContext getNavContext() {
        if (navContext == null) {
            navContext = new NavContext();
        }
        return navContext;
    }

    private final List<Fragment> fragments = CollectionUtils.newUnmodifiableListNotNull(
            RepoQuickFragment.newInstance(),
            StarredQuickFragment.newInstance(),
            AllActivitiesFragment.newInstance(),
            TabSettings2Fragment.newInstance()
    );

    public List<Fragment> getFragments() {
        return fragments;
    }

    public MainViewModel() {
        getNavContext();
    }

    public void getServerInfo() {
        Single<ServerInfoModel> single = HttpIO.getCurrentInstance().execute(MainService.class).getServerInfo();
        addSingleDisposable(single, new Consumer<ServerInfoModel>() {
            @Override
            public void accept(ServerInfoModel serverInfo) throws Exception {
                Account account = SupportAccountManager.getInstance().getCurrentAccount();
                if (account == null) {
                    return;
                }

                ServerInfo serverInfo1 = new ServerInfo(account.server, serverInfo.version, serverInfo.getFeaturesString(), serverInfo.encrypted_library_version);
                SupportAccountManager.getInstance().setServerInfo(account, serverInfo1);

                getServerInfoLiveData().setValue(serverInfo1);
            }
        });
    }

    public void requestRepoModel(String repoId, Consumer<RepoModel> consumer) {
        getOnShowRefreshLoadingInRepoLiveData().setValue(true);

        //from db
        Single<List<RepoModel>> singleDb = AppDatabase.getInstance().repoDao().getRepoById(repoId);
        addSingleDisposable(singleDb, new Consumer<List<RepoModel>>() {
            @Override
            public void accept(List<RepoModel> repoModels) throws Exception {
                if (consumer != null) {
                    if (CollectionUtils.isEmpty(repoModels)) {
                        //no data in sqlite, request RepoApi again
                        getRepoModelFromRemote(repoId, consumer);
                    } else {
                        consumer.accept(repoModels.get(0));
                        getOnShowRefreshLoadingInRepoLiveData().setValue(false);
                    }
                } else {
                    getOnShowRefreshLoadingInRepoLiveData().setValue(false);
                }
            }
        }, new Consumer<Throwable>() {
            @Override
            public void accept(Throwable throwable) throws Exception {
                getOnShowRefreshLoadingInRepoLiveData().setValue(false);
                SLogs.e(throwable);
            }
        });
    }

    private void getRepoModelFromRemote(String repoId, Consumer<RepoModel> consumer) {
        //from net
        Single<RepoWrapperModel> singleNet = HttpIO.getCurrentInstance().execute(RepoService.class).getReposAsync();
        addSingleDisposable(singleNet, new Consumer<RepoWrapperModel>() {
            @Override
            public void accept(RepoWrapperModel repoWrapperModel) throws Exception {
                getOnShowRefreshLoadingInRepoLiveData().setValue(false);

                if (repoWrapperModel == null || CollectionUtils.isEmpty(repoWrapperModel.repos)) {
                    ToastUtils.showLong(R.string.search_library_not_found);
                    return;
                }

                Optional<RepoModel> optionalRepoModel = repoWrapperModel.repos
                        .stream()
                        .filter(f -> TextUtils.equals(f.repo_id, repoId))
                        .findFirst();
                if (optionalRepoModel.isPresent()) {
                    if (consumer != null) {
                        consumer.accept(optionalRepoModel.get());
                    }
                } else {
                    ToastUtils.showLong(R.string.search_library_not_found);
                }
            }
        }, new Consumer<Throwable>() {
            @Override
            public void accept(Throwable throwable) throws Exception {
                getOnShowRefreshLoadingInRepoLiveData().setValue(false);
                String msg = getErrorMsgByThrowable(throwable);
                ToastUtils.showLong(msg);
            }
        });
    }

    public void getPermissionFromLocal(String repoId, int pNum, Consumer<PermissionEntity> consumer) {
        Single<List<PermissionEntity>> pSingle = AppDatabase.getInstance().permissionDAO().getByRepoAndIdAsync(repoId, pNum);
        addSingleDisposable(pSingle, new Consumer<List<PermissionEntity>>() {
            @Override
            public void accept(List<PermissionEntity> permissionEntities) throws Exception {

                if (consumer != null) {
                    if (CollectionUtils.isEmpty(permissionEntities)) {
                        consumer.accept(null);
                    } else {
                        consumer.accept(permissionEntities.get(0));
                    }

                }
            }
        });
    }

    public void getEncCacheDB(String repoId, Consumer<EncKeyCacheEntity> consumer) {
        Single<List<EncKeyCacheEntity>> single = AppDatabase.getInstance().encKeyCacheDAO().getListByRepoIdAsync(repoId);
        addSingleDisposable(single, new Consumer<List<EncKeyCacheEntity>>() {
            @Override
            public void accept(List<EncKeyCacheEntity> list) throws Exception {
                if (CollectionUtils.isEmpty(list)) {
                    consumer.accept(null);
                } else {
                    consumer.accept(list.get(0));
                }
            }
        });
    }


    public void checkLocalDirent(Account account, Context context, RepoModel repoModel, String parentDir, List<Uri> uriList, Consumer<List<Uri>> consumer) {
        if (CollectionUtils.isEmpty(uriList)) {
            return;
        }

        Single<List<Uri>> single = Single.create(new SingleOnSubscribe<List<Uri>>() {
            @Override
            public void subscribe(SingleEmitter<List<Uri>> emitter) throws Exception {

                List<Uri> newUriList = new ArrayList<>();

                for (Uri uri : uriList) {

                    String fileName = Utils.getFilenameFromUri(context, uri);
                    String fullPath = Utils.pathJoin(parentDir, fileName);

                    List<DirentModel> dirents = AppDatabase.getInstance().direntDao().getListByFullPathSync(repoModel.repo_id, fullPath);
                    if (!CollectionUtils.isEmpty(dirents)) {
                        continue;
                    }

                    File destinationFile = copyFile(account, context, uri, repoModel.repo_id, repoModel.repo_name, false);
                    FileTransferEntity transferEntity = getUploadTransferEntity(account, repoModel, parentDir, destinationFile.getAbsolutePath(), false);
                    AppDatabase.getInstance().fileTransferDAO().insert(transferEntity);

                    newUriList.add(uri);
                }

                emitter.onSuccess(newUriList);
            }
        });

        addSingleDisposable(single, new Consumer<List<Uri>>() {
            @Override
            public void accept(List<Uri> uris) throws Exception {
                if (consumer != null) {
                    consumer.accept(uris);
                }
            }
        });
    }

    public void checkRemoteDirent(String repoId, String fullPath, Consumer<DirentFileModel> consumer) throws IOException {
        Single<DirentFileModel> detailSingle = HttpIO.getCurrentInstance()
                .execute(FileService.class)
                .getFileDetail(repoId, fullPath);
        addSingleDisposable(detailSingle, new Consumer<DirentFileModel>() {
            @Override
            public void accept(DirentFileModel direntFileModel) throws Exception {
                if (consumer != null) {
                    consumer.accept(direntFileModel);
                }
            }
        }, new Consumer<Throwable>() {
            @Override
            public void accept(Throwable throwable) throws Exception {
                if (consumer != null) {
                    consumer.accept(null);
                }
            }
        });
    }

    private File copyFile(Account account, Context context, Uri o, String repo_id, String repo_name, boolean isUpdate) throws IOException {
        String fileName = Utils.getFilenameFromUri(context, o);
        String p = getNavContext().getNavPath() + fileName;

        File destinationFile = DataManager.getLocalRepoFile(account, repo_id, repo_name, p);
        if (destinationFile.exists() && isUpdate) {
            destinationFile.delete();
        }

        try (InputStream in = context.getContentResolver().openInputStream(o);
             OutputStream out = Files.newOutputStream(destinationFile.toPath())) {
            IOUtils.copy(in, out);
        }

        return destinationFile;
    }

    private Single<File> getCopyFileSingle(Account account, Context context, Uri o, String repo_id, String repo_name, boolean isUpdate) {
        return Single.create(new SingleOnSubscribe<File>() {
            @Override
            public void subscribe(SingleEmitter<File> emitter) throws Exception {
                File destinationFile = copyFile(account, context, o, repo_id, repo_name, isUpdate);

                emitter.onSuccess(destinationFile);
            }
        });
    }

    /**
     * <ol>
     *     <li>copy the file to the app's internal cache(/0/Android/media/package_name/Seafile/...).</li>
     *     <li>generate the FileTransferEntity, and insert into DB.</li>
     *     <li>start UploadFileManuallyWorker</li>
     *     <li>it will be deleted when UploadFileManuallyWorker end</li>
     * <ol/>
     */
    public void addUploadTask(Account account, Context context, RepoModel repoModel, String parentDir, Uri sourceUri, String fileName, boolean isReplace, Consumer<FileTransferEntity> consumer) {
        ToastUtils.showLong(R.string.upload_waiting);

        //content://com.android.providers.media.documents/document/image:1000182224
        FileTransferEntity entity = getUploadTransferEntityByUri(context, account, repoModel, parentDir, sourceUri, fileName, isReplace);
        insertUploadTask(entity, new Consumer<FileTransferEntity>() {
            @Override
            public void accept(FileTransferEntity fileTransferEntity) throws Exception {
                SLogs.e("addUploadTask uri: complete");
            }
        });

//        Single<File> single = getCopyFileSingle(account, context, sourceUri, repoModel.repo_id, repoModel.repo_name, isReplace);
//        addSingleDisposable(single, new Consumer<File>() {
//            @Override
//            public void accept(File appLocalCacheFile) throws Exception {
//                addUploadTask(account, repoModel, parentDir, appLocalCacheFile.getAbsolutePath(), isReplace, consumer);
//            }
//        });
    }

    public void addUploadTask(Account account, RepoModel repoModel, String parentDir, String appLocalCachedFile, boolean isReplace, Consumer<FileTransferEntity> consumer) {
        FileTransferEntity entity = getUploadTransferEntity(account, repoModel, parentDir, appLocalCachedFile, isReplace);
        insertUploadTask(entity, new Consumer<FileTransferEntity>() {
            @Override
            public void accept(FileTransferEntity fileTransferEntity) throws Exception {
                SLogs.e("addUploadTask file: complete");
            }
        });
    }

    public void insertUploadTask(FileTransferEntity fileTransferEntity, Consumer<FileTransferEntity> consumer) {
        if (fileTransferEntity == null) {
            return;
        }

        Single<FileTransferEntity> single = Single.create(new SingleOnSubscribe<FileTransferEntity>() {
            @Override
            public void subscribe(SingleEmitter<FileTransferEntity> emitter) throws Exception {

                AppDatabase.getInstance().fileTransferDAO().insert(fileTransferEntity);
                emitter.onSuccess(fileTransferEntity);
            }
        });


        addSingleDisposable(single, new Consumer<FileTransferEntity>() {
            @Override
            public void accept(FileTransferEntity transferEntity) throws Exception {

                if (TextUtils.isEmpty(transferEntity.uid)) {
                    return;
                }

                //start worker
                BackgroundJobManagerImpl.getInstance().startFileManualUploadWorker();

                if (consumer != null) {
                    consumer.accept(transferEntity);
                }
            }
        }, new Consumer<Throwable>() {
            @Override
            public void accept(Throwable throwable) throws Exception {
                SLogs.e("addUploadTask", throwable);
            }
        });
    }

    private FileTransferEntity getUploadTransferEntityByUri(Context context, Account account, RepoModel repoModel, String parentDir, Uri uri, String fileName, boolean isReplace) {
        FileTransferEntity entity = new FileTransferEntity();

        if (uri == null || TextUtils.isEmpty(uri.toString())) {
            return null;
        }

        String mime = Utils.getMimeType(context, uri);
        long size = Utils.getFileSize(context, uri);
        long lastModifiedTime = Utils.getFileCreateTime(context, uri);

        entity.full_path = uri.toString();
        entity.target_path = Utils.pathJoin(parentDir, fileName);
        entity.setParent_path(parentDir);

        entity.file_name = fileName;
        entity.file_size = size;
        entity.file_format = FileUtils.getFileExtension(fileName);
        entity.file_md5 = null;
        entity.mime_type = mime;

        //notice
        entity.repo_id = repoModel.repo_id;
        entity.repo_name = repoModel.repo_name;
        entity.related_account = account.getSignature();

        entity.data_source = TransferDataSource.FILE_BACKUP;
        entity.created_at = System.currentTimeMillis();
        entity.modified_at = entity.created_at;
        entity.file_original_modified_at = lastModifiedTime;
        entity.action_end_at = 0;
        entity.file_strategy = isReplace ? ExistingFileStrategy.REPLACE : ExistingFileStrategy.KEEP;
        entity.is_copy_to_local = false;
        entity.transfer_action = TransferAction.UPLOAD;
        entity.result = null;
        entity.transfer_status = TransferStatus.WAITING;

        entity.uid = entity.getUID();
        return entity;
    }

    private FileTransferEntity getUploadTransferEntity(Account account, RepoModel repoModel, String parentDir, String appLocalCacheFilePath, boolean isReplace) {
        FileTransferEntity entity = new FileTransferEntity();

        File file = new File(appLocalCacheFilePath);
        if (!file.exists()) {
            return null;
        }

        entity.full_path = file.getAbsolutePath();
        entity.target_path = Utils.pathJoin(parentDir, file.getName());
        entity.setParent_path(parentDir);

        entity.file_name = file.getName();
        entity.file_size = file.length();
        entity.file_format = FileUtils.getFileExtension(entity.full_path);
        entity.file_md5 = FileUtils.getFileMD5ToString(entity.full_path).toLowerCase();
        entity.mime_type = MimeTypeMap.getSingleton().getMimeTypeFromExtension(entity.file_format);

        //notice
        entity.repo_id = repoModel.repo_id;
        entity.repo_name = repoModel.repo_name;
        entity.related_account = account.getSignature();

        entity.data_source = TransferDataSource.FILE_BACKUP;
        entity.created_at = System.currentTimeMillis();
        entity.modified_at = entity.created_at;
        entity.file_original_modified_at = file.lastModified();
        entity.action_end_at = 0;
        entity.file_strategy = isReplace ? ExistingFileStrategy.REPLACE : ExistingFileStrategy.KEEP;
        entity.is_copy_to_local = false;
        entity.transfer_action = TransferAction.UPLOAD;
        entity.result = null;
        entity.transfer_status = TransferStatus.WAITING;

        entity.uid = entity.getUID();
        return entity;
    }

}
