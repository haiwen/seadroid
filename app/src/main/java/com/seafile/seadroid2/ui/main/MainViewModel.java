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
import com.seafile.seadroid2.enums.ActionModeCallbackType;
import com.seafile.seadroid2.framework.data.db.entities.EncKeyCacheEntity;
import com.seafile.seadroid2.framework.data.db.entities.FileTransferEntity;
import com.seafile.seadroid2.framework.data.db.entities.PermissionEntity;
import com.seafile.seadroid2.framework.data.model.dirents.DirentFileModel;
import com.seafile.seadroid2.enums.TransferAction;
import com.seafile.seadroid2.enums.TransferDataSource;
import com.seafile.seadroid2.enums.TransferResult;
import com.seafile.seadroid2.enums.TransferStatus;
import com.seafile.seadroid2.framework.data.model.permission.PermissionListWrapperModel;
import com.seafile.seadroid2.framework.data.model.permission.PermissionWrapperModel;
import com.seafile.seadroid2.framework.datastore.DataManager;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.framework.worker.ExistingFileStrategy;
import com.seafile.seadroid2.preferences.Settings;
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
import com.seafile.seadroid2.ui.settings.TabSettingsFragment;
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
import java.util.Collections;
import java.util.List;
import java.util.Optional;

import io.reactivex.Completable;
import io.reactivex.Single;
import io.reactivex.SingleEmitter;
import io.reactivex.SingleOnSubscribe;
import io.reactivex.SingleSource;
import io.reactivex.functions.Consumer;
import io.reactivex.functions.Function;
import kotlin.Pair;

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
    private final MutableLiveData<ActionModeCallbackType> _onActionModeLiveData = new MutableLiveData<>();

    public MutableLiveData<ActionModeCallbackType> getOnActionModeLiveData() {
        return _onActionModeLiveData;
    }

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
            TabSettingsFragment.newInstance()
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
        Single<RepoWrapperModel> singleNet = HttpIO.getCurrentInstance().execute(RepoService.class).getRepos();
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
        Single<List<PermissionEntity>> pSingle = AppDatabase.getInstance().permissionDAO().getWithAsync(repoId, pNum);
        Single<PermissionEntity> s = pSingle.flatMap(new Function<List<PermissionEntity>, SingleSource<PermissionEntity>>() {
            @Override
            public SingleSource<PermissionEntity> apply(List<PermissionEntity> pList) throws Exception {

                if (CollectionUtils.isEmpty(pList)) {
                    return null;
                }

                return Single.just(pList.get(0));
            }
        }).flatMap(new Function<PermissionEntity, SingleSource<PermissionEntity>>() {
            @Override
            public SingleSource<PermissionEntity> apply(PermissionEntity entity) throws Exception {
                Single<List<PermissionEntity>> r = getLoadRepoPermissionFromRemoteSingle(repoId);

                return r.flatMap(new Function<List<PermissionEntity>, SingleSource<? extends PermissionEntity>>() {
                    @Override
                    public SingleSource<? extends PermissionEntity> apply(List<PermissionEntity> permissionEntities) throws Exception {
                        if (CollectionUtils.isEmpty(permissionEntities)) {
                            return null;

                        }
                        Optional<PermissionEntity> p = permissionEntities.stream().filter(f -> f.id == pNum).findFirst();
                        if (p.isPresent()) {
                            return Single.just(p.get());
                        }
                        return null;
                    }
                });
            }
        });

        addSingleDisposable(s, new Consumer<PermissionEntity>() {
            @Override
            public void accept(PermissionEntity entity) throws Exception {
                if (consumer != null) {
                    consumer.accept(entity);
                }
            }
        });
    }


    private Single<List<PermissionEntity>> getLoadRepoPermissionFromRemoteSingle(String repoId) {
        Single<PermissionListWrapperModel> single = HttpIO.getCurrentInstance().execute(RepoService.class).getCustomSharePermissions(repoId);
        return single.flatMap(new Function<PermissionListWrapperModel, SingleSource<List<PermissionEntity>>>() {
            @Override
            public SingleSource<List<PermissionEntity>> apply(PermissionListWrapperModel wrapperModel) throws Exception {

                List<PermissionEntity> list = CollectionUtils.newArrayList();

                for (PermissionWrapperModel model : wrapperModel.permission_list) {
                    list.add(new PermissionEntity(repoId, model));
                }

                Completable insertCompletable = AppDatabase.getInstance().permissionDAO().insertAllAsync(list);
                Single<Long> insertAllSingle = insertCompletable.toSingleDefault(0L);
                return insertAllSingle.flatMap(new Function<Long, SingleSource<List<PermissionEntity>>>() {
                    @Override
                    public SingleSource<List<PermissionEntity>> apply(Long aLong) throws Exception {
                        SLogs.d("The list has been inserted into the local database");
                        return Single.just(list);
                    }
                });
            }
        });
    }


    public void getRepoModelFromLocal(String repoId, Consumer<Pair<RepoModel, PermissionEntity>> consumer) {
        //from db
        Single<List<RepoModel>> dbSingle = AppDatabase.getInstance().repoDao().getRepoById(repoId);
        Single<Pair<RepoModel, PermissionEntity>> r = dbSingle.flatMap(new Function<List<RepoModel>, SingleSource<Pair<RepoModel, PermissionEntity>>>() {
            @Override
            public SingleSource<Pair<RepoModel, PermissionEntity>> apply(List<RepoModel> repoModels) throws Exception {
                if (CollectionUtils.isEmpty(repoModels)) {
                    return null;
                }

                RepoModel repoModel = repoModels.get(0);
                if (TextUtils.isEmpty(repoModel.permission)) {
                    return Single.just(new Pair<>(repoModel, null));
                }

                if (!repoModel.isCustomPermission()) {
                    return Single.just(new Pair<>(repoModel, null));
                }

                int pNum = repoModel.getCustomPermissionNum();

                Single<List<PermissionEntity>> pSingle = AppDatabase.getInstance().permissionDAO().getWithAsync(repoId, pNum);

                return pSingle.flatMap(new Function<List<PermissionEntity>, SingleSource<Pair<RepoModel, PermissionEntity>>>() {
                    @Override
                    public SingleSource<Pair<RepoModel, PermissionEntity>> apply(List<PermissionEntity> permissionEntities) throws Exception {
                        if (CollectionUtils.isEmpty(permissionEntities)) {
                            return Single.just(new Pair<>(repoModel, null));
                        }

                        return Single.just(new Pair<>(repoModel, permissionEntities.get(0)));
                    }
                });
            }
        });


        addSingleDisposable(r, new Consumer<Pair<RepoModel, PermissionEntity>>() {
            @Override
            public void accept(Pair<RepoModel, PermissionEntity> pair) throws Exception {
                if (consumer != null) {
                    consumer.accept(pair);
                }
            }
        }, new Consumer<Throwable>() {
            @Override
            public void accept(Throwable throwable) throws Exception {
                SLogs.e(throwable);
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

    public void searchLocal(String key) {

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
    public void addUploadTask(Account account, Context context, RepoModel repoModel, String parentDir, Uri sourceUri, boolean isReplace, Consumer<FileTransferEntity> consumer) {
        ToastUtils.showLong(R.string.upload_waiting);

        Single<File> single = getCopyFileSingle(account, context, sourceUri, repoModel.repo_id, repoModel.repo_name, isReplace);
        addSingleDisposable(single, new Consumer<File>() {
            @Override
            public void accept(File appLocalCacheFile) throws Exception {
                addUploadTask(account, repoModel, parentDir, appLocalCacheFile.getAbsolutePath(), isReplace, consumer);
            }
        });
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
        entity.transfer_result = TransferResult.NO_RESULT;
        entity.transfer_status = TransferStatus.WAITING;

        entity.uid = entity.getUID();
        return entity;
    }

    public void addUploadTask(Account account, RepoModel repoModel, String parentDir, String appLocalCacheFile, boolean isReplace, Consumer<FileTransferEntity> consumer) {
        Single<FileTransferEntity> single = Single.create(new SingleOnSubscribe<FileTransferEntity>() {
            @Override
            public void subscribe(SingleEmitter<FileTransferEntity> emitter) throws Exception {

                FileTransferEntity entity = getUploadTransferEntity(account, repoModel, parentDir, appLocalCacheFile, isReplace);

                AppDatabase.getInstance().fileTransferDAO().insert(entity);

                emitter.onSuccess(entity);

            }
        });

        addSingleDisposable(single, new Consumer<FileTransferEntity>() {
            @Override
            public void accept(FileTransferEntity transferEntity) throws Exception {

                if (TextUtils.isEmpty(transferEntity.uid)) {
                    return;
                }

                //start worker
                BackgroundJobManagerImpl.getInstance().startFileUploadWorker();

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
}
