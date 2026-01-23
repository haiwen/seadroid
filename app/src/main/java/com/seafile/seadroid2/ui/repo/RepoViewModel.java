package com.seafile.seadroid2.ui.repo;

import android.content.Context;
import android.text.TextUtils;
import android.util.Pair;

import androidx.lifecycle.MutableLiveData;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.NetworkUtils;
import com.blankj.utilcode.util.TimeUtils;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.context.NavContext;
import com.seafile.seadroid2.enums.FileViewType;
import com.seafile.seadroid2.enums.ItemPositionEnum;
import com.seafile.seadroid2.enums.RefreshStatusEnum;
import com.seafile.seadroid2.framework.crypto.SecurePasswordManager;
import com.seafile.seadroid2.framework.datastore.sp.SettingsManager;
import com.seafile.seadroid2.framework.db.AppDatabase;
import com.seafile.seadroid2.framework.db.entities.DirentModel;
import com.seafile.seadroid2.framework.db.entities.EncKeyCacheEntity;
import com.seafile.seadroid2.framework.db.entities.PermissionEntity;
import com.seafile.seadroid2.framework.db.entities.RepoModel;
import com.seafile.seadroid2.framework.http.HttpIO;
import com.seafile.seadroid2.framework.model.BaseModel;
import com.seafile.seadroid2.framework.model.ResultModel;
import com.seafile.seadroid2.framework.model.dirents.CachedDirentModel;
import com.seafile.seadroid2.framework.model.dirents.DirentRecursiveFileModel;
import com.seafile.seadroid2.framework.model.repo.Dirent2Model;
import com.seafile.seadroid2.framework.model.search.SearchFileModel;
import com.seafile.seadroid2.framework.model.search.SearchFileWrapperModel;
import com.seafile.seadroid2.framework.model.search.SearchModel;
import com.seafile.seadroid2.framework.model.search.SearchWrapperModel;
import com.seafile.seadroid2.framework.service.BackupThreadExecutor;
import com.seafile.seadroid2.framework.service.PreDownloadHelper;
import com.seafile.seadroid2.framework.util.Objs;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.util.Toasts;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.preferences.Settings;
import com.seafile.seadroid2.baseviewmodel.BaseViewModel;
import com.seafile.seadroid2.ui.dialog_fragment.DialogService;
import com.seafile.seadroid2.ui.search.SearchService;
import com.seafile.seadroid2.ui.star.StarredService;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import io.reactivex.Flowable;
import io.reactivex.Single;
import io.reactivex.SingleEmitter;
import io.reactivex.SingleOnSubscribe;
import io.reactivex.SingleSource;
import io.reactivex.functions.Action;
import io.reactivex.functions.Consumer;
import io.reactivex.functions.Function;
import okhttp3.RequestBody;

public class RepoViewModel extends BaseViewModel {

    private final MutableLiveData<Boolean> _showEmptyViewLiveData = new MutableLiveData<>();

    public MutableLiveData<Boolean> getShowEmptyViewLiveData() {
        return _showEmptyViewLiveData;
    }

    private final MutableLiveData<List<BaseModel>> _objListLiveData = new MutableLiveData<>();
    private final MutableLiveData<Boolean> _starredLiveData = new MutableLiveData<>();

    public MutableLiveData<Boolean> getStarredLiveData() {
        return _starredLiveData;
    }

    public MutableLiveData<List<BaseModel>> getObjListLiveData() {
        return _objListLiveData;
    }


    ///
    private final MutableLiveData<RepoViewModel.DecryptResult> _decryptRepoLiveData = new MutableLiveData<>();

    public MutableLiveData<RepoViewModel.DecryptResult> getDecryptRepoLiveData() {
        return _decryptRepoLiveData;
    }

    private final MutableLiveData<ResultModel> _remoteVerifyRepoPwdLiveData = new MutableLiveData<>();

    public MutableLiveData<ResultModel> getRemoteVerifyRepoPwdLiveData() {
        return _remoteVerifyRepoPwdLiveData;
    }

    /**
     * decrypt result enum
     */
    public enum DecryptResult {
        NEED_PASSWORD,
        PASSWORD_EXPIRED,
        SUCCESS,
        FAILED
    }


    public void decryptRepo(RepoModel repoModel) {
        if (repoModel == null || !repoModel.encrypted) {
            // The non-encrypted database will be returned to success
            getDecryptRepoLiveData().setValue(DecryptResult.SUCCESS);
            return;
        }

        // query the local database to get the password cache
        Single<List<EncKeyCacheEntity>> encSingle = AppDatabase.getInstance().encKeyCacheDAO().getListByRepoIdAsync(repoModel.repo_id);

        Single<DecryptResult> decryptSingle = encSingle.flatMap(new Function<List<EncKeyCacheEntity>, SingleSource<DecryptResult>>() {
            @Override
            public SingleSource<DecryptResult> apply(List<EncKeyCacheEntity> encKeyCacheEntities) throws Exception {
                if (CollectionUtils.isEmpty(encKeyCacheEntities)) {
                    return Single.just(DecryptResult.NEED_PASSWORD);
                }

                EncKeyCacheEntity encKeyCacheEntity = encKeyCacheEntities.get(0);
                long now = TimeUtils.getNowMills();
                boolean isExpired = encKeyCacheEntity.expire_time_long == 0 || now > encKeyCacheEntity.expire_time_long;

                if (isExpired) {
                    // the password has expired
                    if (TextUtils.isEmpty(encKeyCacheEntity.enc_key) || TextUtils.isEmpty(encKeyCacheEntity.enc_iv)) {
                        // There is no valid encryption key and you need to re-enter your password
                        return Single.just(DecryptResult.NEED_PASSWORD);
                    } else {
                        // There is an encryption key, which is attempted to decrypt and verify remotely
                        return Single.just(DecryptResult.PASSWORD_EXPIRED);
                    }
                } else {
                    // The password has not expired, and it will be returned to success
                    return Single.just(DecryptResult.SUCCESS);
                }
            }
        });

        Single<DecryptResult> resultSingle = decryptSingle.flatMap(new Function<DecryptResult, SingleSource<DecryptResult>>() {
            @Override
            public SingleSource<DecryptResult> apply(DecryptResult result) throws Exception {
                if (result == DecryptResult.PASSWORD_EXPIRED) {
                    // password expired try remote verification
                    return tryRemoteVerifyWithCachedPassword(repoModel.repo_id);
                }
                return Single.just(result);
            }
        });

        addSingleDisposable(resultSingle, new Consumer<DecryptResult>() {
            @Override
            public void accept(DecryptResult result) throws Exception {
                getDecryptRepoLiveData().setValue(result);
            }
        }, new Consumer<Throwable>() {
            @Override
            public void accept(Throwable throwable) throws Exception {
                // an exception occurs and the return fails
                getDecryptRepoLiveData().setValue(DecryptResult.FAILED);
            }
        });
    }

    /**
     * remote authentication with cached passwords
     */
    private Single<DecryptResult> tryRemoteVerifyWithCachedPassword(String repoId) {
        return AppDatabase.getInstance().encKeyCacheDAO().getListByRepoIdAsync(repoId)
                .flatMap(new Function<List<EncKeyCacheEntity>, SingleSource<DecryptResult>>() {
                    @Override
                    public SingleSource<DecryptResult> apply(List<EncKeyCacheEntity> entities) throws Exception {
                        if (CollectionUtils.isEmpty(entities)) {
                            return Single.just(DecryptResult.NEED_PASSWORD);
                        }

                        EncKeyCacheEntity entity = entities.get(0);
                        if (TextUtils.isEmpty(entity.enc_key) || TextUtils.isEmpty(entity.enc_iv)) {
                            return Single.just(DecryptResult.NEED_PASSWORD);
                        }

                        String cachedPassword = SecurePasswordManager.decryptPassword(entity.enc_key, entity.enc_iv);
                        if (TextUtils.isEmpty(cachedPassword)) {
                            return Single.just(DecryptResult.NEED_PASSWORD);
                        }

                        return getRemoteVerifySingle(repoId, cachedPassword)
                                .map(new Function<ResultModel, DecryptResult>() {
                                    @Override
                                    public DecryptResult apply(ResultModel resultModel) throws Exception {
                                        return resultModel.success ? DecryptResult.SUCCESS : DecryptResult.NEED_PASSWORD;
                                    }
                                });
                    }
                });
    }

    /**
     * do remote authentication and update the local password cache
     */
    public Single<ResultModel> getRemoteVerifySingle(String repoId, String password) {
        Map<String, String> requestDataMap = new HashMap<>();
        requestDataMap.put("password", password);
        Map<String, RequestBody> bodyMap = genRequestBody(requestDataMap);

        Single<ResultModel> netSingle = HttpIO.getCurrentInstance().execute(DialogService.class).setPassword(repoId, bodyMap);

        return netSingle.flatMap(new Function<ResultModel, SingleSource<ResultModel>>() {
            @Override
            public SingleSource<ResultModel> apply(ResultModel resultModel) throws Exception {
                if (resultModel.success) {
                    return updateLocalPasswordCache(repoId, password)
                            .map(new Function<SeafException, ResultModel>() {
                                @Override
                                public ResultModel apply(SeafException aVoid) throws Exception {
                                    return resultModel;
                                }
                            });
                }
                return Single.just(resultModel);
            }
        });
    }

    private Single<SeafException> updateLocalPasswordCache(String repoId, String password) {
        return Single.create(emitter -> {
            try {
                EncKeyCacheEntity encEntity = new EncKeyCacheEntity();
                encEntity.v = 2;
                encEntity.repo_id = repoId;

                Pair<String, String> p = SecurePasswordManager.encryptPassword(password);
                if (p != null) {
                    encEntity.enc_key = p.first;
                    encEntity.enc_iv = p.second;

                    long expire = TimeUtils.getNowMills();
                    expire += SettingsManager.DECRYPTION_EXPIRATION_TIME;
                    encEntity.expire_time_long = expire;

                    AppDatabase.getInstance().encKeyCacheDAO().insert(encEntity);
                }

                emitter.onSuccess(SeafException.SUCCESS);
            } catch (Exception e) {
                emitter.onError(SeafException.UNSUPPORTED_ENC_VERSION);
            }
        });
    }

    public void loadData(NavContext context, RefreshStatusEnum refreshStatus, boolean isBlank) {
        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        if (account == null) {
            return;
        }

        if (RefreshStatusEnum.ONLY_REMOTE == refreshStatus) {
            //delete/new/starred/rename/refresh/... -> ONLY_REMOTE
            if (context.inRepo()) {
                loadDirentsFromRemote(account, context);
            } else {
                loadReposFromRemote(account);
            }
        } else if (RefreshStatusEnum.LOCAL_THEN_REMOTE == refreshStatus) {
            //first load data
            if (context.inRepo()) {
                loadDirentsFromLocal(account, context, true, isBlank);
            } else {
                loadReposFromLocal(account, true, isBlank);
            }
        } else if (RefreshStatusEnum.ONLY_LOCAL == refreshStatus) {
            //back/sort/page_change -> ONLY_LOCAL
            if (context.inRepo()) {
                loadDirentsFromLocal(account, context, false, isBlank);
            } else {
                loadReposFromLocal(account, false, isBlank);
            }
        }
    }

    private void loadReposFromLocal(Account account, boolean isLoadRemoteData, boolean isBlank) {
        //clear list
        if (isBlank) {
            getObjListLiveData().setValue(null);
        }

        Single<List<BaseModel>> singleDB = AppDatabase.getInstance()
                .repoDao()
                .getListByAccount(account.getSignature())
                .flatMap(new Function<List<RepoModel>, SingleSource<List<BaseModel>>>() {
                    @Override
                    public SingleSource<List<BaseModel>> apply(List<RepoModel> repoModels) throws Exception {
                        List<BaseModel> bs = Objs.convertToAdapterList(repoModels, false);
                        return Single.just(bs);
                    }
                });

        addSingleDisposable(singleDB, new Consumer<List<BaseModel>>() {
            @Override
            public void accept(List<BaseModel> list) {
                getObjListLiveData().setValue(list);

                if (isLoadRemoteData && NetworkUtils.isConnected()) {
                    loadReposFromRemote(account);
                } else {
                    getShowEmptyViewLiveData().setValue(CollectionUtils.isEmpty(list));
                }
            }
        }, new Consumer<Throwable>() {
            @Override
            public void accept(Throwable throwable) throws Exception {
                SLogs.e(throwable);
            }
        });
    }

    private void loadReposFromRemote(Account account) {
        if (!NetworkUtils.isConnected()) {
            getRefreshLiveData().setValue(false);
            return;
        }

        getRefreshLiveData().setValue(true);

        //load net data and load local data
        Single<List<BaseModel>> resultSingle = Objs.getReposSingleFromServer(account);

        addSingleDisposable(resultSingle, new Consumer<List<BaseModel>>() {
            @Override
            public void accept(List<BaseModel> models) throws Exception {
                getObjListLiveData().setValue(models);
                getShowEmptyViewLiveData().setValue(CollectionUtils.isEmpty(models));
                getRefreshLiveData().setValue(false);
            }
        }, new Consumer<Throwable>() {
            @Override
            public void accept(Throwable throwable) throws Exception {
                getRefreshLiveData().setValue(false);

                SeafException seafException = getSeafExceptionByThrowable(throwable);
                if (seafException == SeafException.REMOTE_WIPED_EXCEPTION) {
                    //post a request
                    completeRemoteWipe();
                }

                getObjListLiveData().setValue(null);
                getSeafExceptionLiveData().setValue(seafException);

            }
        });
    }

    private void loadDirentsFromLocal(Account account, NavContext navContext, boolean isLoadRemoteData, boolean isBlank) {
        //clear list
        if (isBlank) {
            getObjListLiveData().setValue(null);
        }

        FileViewType fileViewType = Settings.FILE_LIST_VIEW_TYPE.queryValue();

        Single<List<BaseModel>> r = getLoadLocalDirentsSingle(account, navContext)
                .flatMap(new Function<List<DirentModel>, SingleSource<List<BaseModel>>>() {
                    @Override
                    public SingleSource<List<BaseModel>> apply(List<DirentModel> direntModels) throws Exception {
                        if (fileViewType == FileViewType.GALLERY) {
                            List<DirentModel> ds = direntModels.stream().filter(new java.util.function.Predicate<DirentModel>() {
                                @Override
                                public boolean test(DirentModel direntModel) {
                                    return Utils.isViewableImage(direntModel.name) || Utils.isVideoFile(direntModel.name);
                                }
                            }).collect(Collectors.toList());
                            return Single.just(new ArrayList<>(ds));
                        } else {
                            List<BaseModel> results = Objs.parseLocalDirents(direntModels);

                            if (!CollectionUtils.isEmpty(results)) {
                                if (results.size() == 1) {
                                    results.get(0).item_position = ItemPositionEnum.ALL;
                                } else if (results.size() == 2) {
                                    results.get(0).item_position = ItemPositionEnum.START;
                                    results.get(1).item_position = ItemPositionEnum.END;
                                } else {
                                    results.get(0).item_position = ItemPositionEnum.START;
                                    results.get(results.size() - 1).item_position = ItemPositionEnum.END;
                                }
                            }
                            return Single.just(results);
                        }
                    }
                });

        addSingleDisposable(r, new Consumer<List<BaseModel>>() {
            @Override
            public void accept(List<BaseModel> results) throws Exception {

                getObjListLiveData().setValue(results);

                if (isLoadRemoteData && NetworkUtils.isConnected()) {
                    loadDirentsFromRemote(account, navContext);
                } else {
                    getShowEmptyViewLiveData().setValue(CollectionUtils.isEmpty(results));
                }
            }
        });
    }

    private Single<List<DirentModel>> getLoadLocalDirentsSingle(Account account, NavContext navContext) {
        RepoModel repoModel = navContext.getRepoModel();

        String repoId = repoModel.repo_id;
        String parentDir = navContext.getNavPath();
        boolean isRepoCustomPermission = repoModel.isCustomPermission();
        String repoPermName = repoModel.permission;

        if (!parentDir.endsWith("/")) {
            parentDir = parentDir + "/";
        }

        Single<List<CachedDirentModel>> direntDBSingle = AppDatabase.getInstance().direntDao().getDirentsWithLocalFileId(repoId, parentDir);
        return direntDBSingle.flatMap(new Function<List<CachedDirentModel>, SingleSource<List<DirentModel>>>() {
            @Override
            public SingleSource<List<DirentModel>> apply(List<CachedDirentModel> cachedDirentModels) throws Exception {
                if (CollectionUtils.isEmpty(cachedDirentModels)) {
                    return Single.just(new ArrayList<DirentModel>());
                }

                List<DirentModel> direntModels = cachedDirentModels.stream().map(new java.util.function.Function<CachedDirentModel, DirentModel>() {
                    @Override
                    public DirentModel apply(CachedDirentModel cachedDirentModel) {
                        cachedDirentModel.dirent.local_file_id = cachedDirentModel.local_file_id;
                        return cachedDirentModel.dirent;
                    }
                }).collect(Collectors.toList());

                return Single.just(direntModels);
            }
        });
    }

    private void loadDirentsFromRemote(Account account, NavContext navContext) {
        if (!NetworkUtils.isConnected()) {
            getRefreshLiveData().setValue(false);
            return;
        }

        getRefreshLiveData().setValue(true);

        RepoModel repoModel = navContext.getRepoModel();
        String repoId = repoModel.repo_id;
        String repoName = navContext.getRepoModel().repo_name;

        String parentDir = navContext.getNavPath();
        if (!parentDir.endsWith("/")) {
            parentDir = parentDir + "/";
        }

        Single<List<DirentModel>> direntSingle = Objs.getDirentsSingleFromServer(account, repoId, repoName, parentDir);
        addSingleDisposable(direntSingle, new Consumer<List<DirentModel>>() {
            @Override
            public void accept(List<DirentModel> direntModels) throws Exception {

                FileViewType fileViewType = Settings.FILE_LIST_VIEW_TYPE.queryValue();

                List<BaseModel> results = new ArrayList<>();

                if (FileViewType.GALLERY == fileViewType) {
                    List<DirentModel> ds = direntModels.stream().filter(new java.util.function.Predicate<DirentModel>() {
                        @Override
                        public boolean test(DirentModel direntModel) {
                            return Utils.isViewableImage(direntModel.name) || Utils.isVideoFile(direntModel.name);
                        }
                    }).collect(Collectors.toList());

                    results.addAll(ds);
                } else {
                    results.addAll(direntModels);
                }

                getObjListLiveData().setValue(results);
                getShowEmptyViewLiveData().setValue(CollectionUtils.isEmpty(results));
                getRefreshLiveData().setValue(false);
            }
        }, new Consumer<Throwable>() {
            @Override
            public void accept(Throwable throwable) throws Exception {
                getRefreshLiveData().setValue(false);

                SeafException seafException = getSeafExceptionByThrowable(throwable);
                if (seafException == SeafException.REMOTE_WIPED_EXCEPTION) {
                    //post a request
                    completeRemoteWipe();
                }

                getObjListLiveData().setValue(null);
                getSeafExceptionLiveData().setValue(seafException);
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

    public void getRepoModelEntity(String repoId, Consumer<RepoModel> consumer) {
        Single<List<RepoModel>> r = AppDatabase.getInstance().repoDao().getRepoById(repoId);
        addSingleDisposable(r, new Consumer<List<RepoModel>>() {
            @Override
            public void accept(List<RepoModel> rs) throws Exception {
                if (consumer != null) {
                    if (CollectionUtils.isEmpty(rs)) {
                        consumer.accept(null);
                    } else {
                        consumer.accept(rs.get(0));
                    }
                }
            }
        });
    }

    public void getRepoModelAndPermissionEntity(String repoId, Consumer<Pair<RepoModel, PermissionEntity>> consumer) {
        Single<Pair<RepoModel, PermissionEntity>> r = getSingleForLoadRepoModelAndAllPermission(repoId);
        addSingleDisposable(r, new Consumer<Pair<RepoModel, PermissionEntity>>() {
            @Override
            public void accept(Pair<RepoModel, PermissionEntity> pair) throws Exception {
                if (consumer != null) {
                    consumer.accept(pair);
                }
            }
        });
    }

    /**
     * get the repoModel and repoMode‘s PermissionEntity from local, if not exist, get from remote.
     */
    private Single<Pair<RepoModel, PermissionEntity>> getSingleForLoadRepoModelAndAllPermission(String repoId) {
        Single<List<RepoModel>> repoSingle = AppDatabase.getInstance().repoDao().getRepoById(repoId);
        return repoSingle.flatMap(new io.reactivex.functions.Function<List<RepoModel>, SingleSource<Pair<RepoModel, PermissionEntity>>>() {
            @Override
            public SingleSource<Pair<RepoModel, PermissionEntity>> apply(List<RepoModel> repoModels) throws Exception {
                if (CollectionUtils.isEmpty(repoModels)) {
                    return Single.just(new Pair<>(null, null));
                }

                RepoModel repoModel = repoModels.get(0);
                if (!repoModel.isCustomPermission()) {
                    return Single.just(new Pair<>(repoModel, new PermissionEntity(repoId, repoModel.permission)));
                }

                Single<List<PermissionEntity>> pSingle = AppDatabase.getInstance().permissionDAO().getByRepoAndIdAsync(repoId, repoModel.getCustomPermissionNum());
                return pSingle.flatMap((io.reactivex.functions.Function<List<PermissionEntity>, SingleSource<Pair<RepoModel, PermissionEntity>>>) pList -> {
                    //no data in local db
                    if (CollectionUtils.isEmpty(pList)) {
                        return Single.just(new Pair<>(repoModel, new PermissionEntity(repoModel.repo_id, "r")));
                    }

                    //get first permission
                    return Single.just(new Pair<>(repoModel, pList.get(0)));
                });
            }
        });
    }


    public void multiStarOrNot(List<BaseModel> selectedList, boolean isStar) {
        if (CollectionUtils.isEmpty(selectedList)) {
            return;
        }

        getShowLoadingDialogLiveData().setValue(true);

        List<Flowable<?>> singles = new ArrayList<>();
        for (BaseModel baseModel : selectedList) {
            if (baseModel instanceof RepoModel m) {

                if (isStar == m.starred) {
                    continue;
                }

                singles.add(getSingleForStar(m.repo_id, "/", isStar));
            } else if (baseModel instanceof DirentModel m) {
                if (isStar == m.starred) {
                    continue;
                }

                singles.add(getSingleForStar(m.repo_id, m.full_path, isStar));
            }
        }

        if (CollectionUtils.isEmpty(singles)) {
            getShowLoadingDialogLiveData().setValue(false);
            return;
        }

        Flowable<Object> flowable = Flowable.mergeDelayError(singles, 5, Flowable.bufferSize());
        addFlowableDisposable(flowable, new Consumer<Object>() {
            @Override
            public void accept(Object o) throws Exception {
                if (o instanceof Dirent2Model dirent2Model) {
                    SLogs.d("dirent star success:" + dirent2Model);
                } else if (o instanceof ResultModel resultModel) {
                    SLogs.d("repo star success:" + resultModel);
                }
            }
        }, new Consumer<Throwable>() {
            @Override
            public void accept(Throwable throwable) throws Exception {
                getShowLoadingDialogLiveData().setValue(false);
                getStarredLiveData().setValue(false);
                String errMsg = getErrorMsgByThrowable(throwable);
                Toasts.show(errMsg);
            }
        }, new Action() {
            @Override
            public void run() throws Exception {
                getShowLoadingDialogLiveData().setValue(false);
                getStarredLiveData().setValue(true);
            }
        });


    }

    private Flowable<?> getSingleForStar(String repoId, String path, boolean isStar) {
        if (isStar) {
            Map<String, String> requestDataMap = new HashMap<>();
            requestDataMap.put("repo_id", repoId);
            requestDataMap.put("path", path);

            Map<String, RequestBody> bodyMap = genRequestBody(requestDataMap);
            Single<Dirent2Model> single = HttpIO.getCurrentInstance().execute(StarredService.class).star(bodyMap);

            return single.toFlowable();
        } else {
            Single<ResultModel> single1 = HttpIO.getCurrentInstance().execute(StarredService.class).unStar(repoId, path);
            return single1.toFlowable();
        }
    }


    /// /////////////////////// search ////////////////////////////

    private final MutableLiveData<List<SearchModel>> mListLiveData = new MutableLiveData<>();

    public MutableLiveData<List<SearchModel>> getSearchListLiveData() {
        return mListLiveData;
    }

    public void searchNext(String repoId, String repoName, String q, boolean isPro, int pageNo, int pageSize) {
        if (isPro) {
            searchPro(repoId, q, pageNo, pageSize);
        } else {
            searchCE(repoId, repoName, q);
        }
    }

    public void searchPro(String repoId, String q, int pageNo, int pageSize) {
        if (TextUtils.isEmpty(q)) {
            return;
        }

        getRefreshLiveData().setValue(true);

        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        String typeOrRepoId = TextUtils.isEmpty(repoId) ? "all" : repoId;
        Single<SearchWrapperModel> single = HttpIO.getCurrentInstance().execute(SearchService.class).search(typeOrRepoId, q, "all", pageNo, pageSize);

        addSingleDisposable(single, new Consumer<SearchWrapperModel>() {
            @Override
            public void accept(SearchWrapperModel searchWrapperModel) throws Exception {

                if (searchWrapperModel == null || searchWrapperModel.results == null) {
                    getSearchListLiveData().setValue(CollectionUtils.newArrayList());
                    getRefreshLiveData().setValue(false);
                    return;
                }

                List<SearchModel> results = searchWrapperModel.results;

                for (SearchModel result : results) {
                    result.related_account = account.getSignature();
                }

                //calculate item_position
                if (CollectionUtils.isEmpty(results)) {

                } else if (results.size() == 1) {
                    results.get(0).item_position = ItemPositionEnum.ALL;
                } else {
                    results.get(0).item_position = ItemPositionEnum.START;
                    results.get(results.size() - 1).item_position = ItemPositionEnum.END;
                }

                getSearchListLiveData().setValue(results);
                getRefreshLiveData().setValue(false);
            }
        }, new Consumer<Throwable>() {
            @Override
            public void accept(Throwable throwable) throws Exception {
                getSeafExceptionLiveData().setValue(getSeafExceptionByThrowable(throwable));

                getRefreshLiveData().setValue(false);
            }
        });
    }

    public void searchCE(String repoId, String repoName, String q) {
        if (TextUtils.isEmpty(q)) {
            return;
        }

        getRefreshLiveData().setValue(true);

        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        Single<SearchFileWrapperModel> single = HttpIO.getCurrentInstance().execute(SearchService.class).searchFile(repoId, q);

        addSingleDisposable(single, new Consumer<SearchFileWrapperModel>() {
            @Override
            public void accept(SearchFileWrapperModel wrapperModel) throws Exception {

                if (wrapperModel == null || wrapperModel.data == null) {
                    getSearchListLiveData().setValue(CollectionUtils.newArrayList());
                    getRefreshLiveData().setValue(false);
                    return;
                }

                List<SearchFileModel> results = wrapperModel.data;

                List<SearchModel> ret = new ArrayList<>();
                for (SearchFileModel result : results) {
                    SearchModel searchModel = new SearchModel();
                    searchModel.related_account = account.getSignature();
                    searchModel.fullpath = result.path;
                    searchModel.size = result.size;
                    searchModel.is_dir = TextUtils.equals(result.type, "folder");
                    searchModel.name = Utils.getFileNameFromPath(result.path);
                    searchModel.repo_id = repoId;
                    searchModel.repo_name = repoName;
                    ret.add(searchModel);
                }

                //calculate item_position
                if (CollectionUtils.isEmpty(results)) {

                } else if (results.size() == 1) {
                    results.get(0).item_position = ItemPositionEnum.ALL;
                } else {
                    results.get(0).item_position = ItemPositionEnum.START;
                    results.get(results.size() - 1).item_position = ItemPositionEnum.END;
                }

                getSearchListLiveData().setValue(ret);
                getRefreshLiveData().setValue(false);
            }
        }, new Consumer<Throwable>() {
            @Override
            public void accept(Throwable throwable) throws Exception {
                getSeafExceptionLiveData().setValue(getSeafExceptionByThrowable(throwable));

                getRefreshLiveData().setValue(false);
            }
        });
    }

    /// download
    public void preDownload(Context context, Account account, List<DirentModel> direntModels) {
        getSecondRefreshLiveData().setValue(true);
        Single<SeafException> single = Single.create(new SingleOnSubscribe<SeafException>() {
            @Override
            public void subscribe(SingleEmitter<SeafException> emitter) throws Exception {
                if (CollectionUtils.isEmpty(direntModels)) {
                    emitter.onSuccess(SeafException.SUCCESS);
                    return;
                }

                for (DirentModel direntModel : direntModels) {
                    try {
                        if (direntModel.isDir()) {
                            List<DirentRecursiveFileModel> list = PreDownloadHelper.fetchRecursiveFiles(direntModel);
                            PreDownloadHelper.insertIntoDbWhenDirentIsDir(account, direntModel, list);
                        } else {
                            PreDownloadHelper.insertIntoDbWhenDirentIsFile(account, direntModel);
                        }
                    } catch (IOException e) {
                        SLogs.e(e.getMessage());
                    }
                }

                emitter.onSuccess(SeafException.SUCCESS);
            }
        });

        addSingleDisposable(single, new Consumer<SeafException>() {
            @Override
            public void accept(SeafException e) throws Exception {
                getSecondRefreshLiveData().setValue(false);

                //
                BackupThreadExecutor.getInstance().runDownloadTask();
            }
        });

    }

}
