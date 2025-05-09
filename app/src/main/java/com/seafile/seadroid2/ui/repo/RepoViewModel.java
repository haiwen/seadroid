package com.seafile.seadroid2.ui.repo;

import android.content.Context;
import android.text.TextUtils;
import android.util.Pair;
import android.view.MenuInflater;
import android.view.MenuItem;

import androidx.annotation.NonNull;
import androidx.lifecycle.MutableLiveData;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.NetworkUtils;
import com.blankj.utilcode.util.TimeUtils;
import com.blankj.utilcode.util.ToastUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.context.NavContext;
import com.seafile.seadroid2.enums.FileViewType;
import com.seafile.seadroid2.enums.ItemPositionEnum;
import com.seafile.seadroid2.enums.RefreshStatusEnum;
import com.seafile.seadroid2.framework.crypto.SecurePasswordManager;
import com.seafile.seadroid2.framework.db.AppDatabase;
import com.seafile.seadroid2.framework.db.entities.DirentModel;
import com.seafile.seadroid2.framework.db.entities.EncKeyCacheEntity;
import com.seafile.seadroid2.framework.db.entities.PermissionEntity;
import com.seafile.seadroid2.framework.db.entities.RepoModel;
import com.seafile.seadroid2.framework.model.BaseModel;
import com.seafile.seadroid2.framework.model.ResultModel;
import com.seafile.seadroid2.framework.model.TResultModel;
import com.seafile.seadroid2.framework.model.dirents.CachedDirentModel;
import com.seafile.seadroid2.framework.model.permission.PermissionWrapperModel;
import com.seafile.seadroid2.framework.model.repo.Dirent2Model;
import com.seafile.seadroid2.framework.datastore.sp.SettingsManager;
import com.seafile.seadroid2.framework.http.HttpIO;
import com.seafile.seadroid2.framework.util.Objs;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.preferences.Settings;
import com.seafile.seadroid2.ui.base.viewmodel.BaseViewModel;
import com.seafile.seadroid2.ui.bottomsheetmenu.ActionMenu;
import com.seafile.seadroid2.ui.dialog_fragment.DialogService;
import com.seafile.seadroid2.ui.star.StarredService;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

import io.reactivex.Flowable;
import io.reactivex.Single;
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
    private final MutableLiveData<List<MenuItem>> _menuItemListLiveData = new MutableLiveData<>();
    /**
     * {@link #getCurrentNavPermissionCacheMap()}
     */
    private final ConcurrentHashMap<String, PermissionEntity> _permissionMap = new ConcurrentHashMap<>();

    /**
     * There are only 6 cases at most.
     * <pre>
     *     repo's permission:
     *     <"repo-?", PermissionEntity()>
     *
     *     dirent's permission:
     *     <"rw", PermissionEntity()>
     *     <"r", PermissionEntity()>
     *     <"manage", PermissionEntity()>
     *     <"cloud-edit", PermissionEntity()>
     *     <"cloud-preview", PermissionEntity()>
     * </pre>
     */
    public ConcurrentHashMap<String, PermissionEntity> getCurrentNavPermissionCacheMap() {
        return _permissionMap;
    }

    private void addRepoPermissionIntoMap(PermissionEntity permission) {
        getCurrentNavPermissionCacheMap().put("repo", permission);
    }

    private PermissionEntity getRepoPermissionFromMap() {
        return getCurrentNavPermissionCacheMap().get("repo");
    }

    private boolean isNotExistsRepoPermission() {
        return !getCurrentNavPermissionCacheMap().containsKey("repo");
    }

    private void removeNonRepoPermission() {
        getCurrentNavPermissionCacheMap().keySet().removeIf(s -> !"repo".equals(s));
    }

    public void removeAllPermission() {
        getCurrentNavPermissionCacheMap().clear();
    }


    public MutableLiveData<List<MenuItem>> getMenuItemListLiveData() {
        return _menuItemListLiveData;
    }

    public MutableLiveData<Boolean> getStarredLiveData() {
        return _starredLiveData;
    }

    public MutableLiveData<List<BaseModel>> getObjListLiveData() {
        return _objListLiveData;
    }


    public void decryptRepo(String repoId, Consumer<String> consumer) {
        if (consumer == null) {
            throw new IllegalArgumentException("consumer is null");
        }

        Single<List<EncKeyCacheEntity>> encSingle = AppDatabase.getInstance().encKeyCacheDAO().getListByRepoIdAsync(repoId);
        Single<String> s = encSingle.flatMap(new Function<List<EncKeyCacheEntity>, SingleSource<String>>() {
            @Override
            public SingleSource<String> apply(List<EncKeyCacheEntity> encKeyCacheEntities) throws Exception {
                if (CollectionUtils.isEmpty(encKeyCacheEntities)) {
                    return Single.just("need-to-re-enter-password");//need password and save into database
                }

                long now = TimeUtils.getNowMills();
                EncKeyCacheEntity encKeyCacheEntity = encKeyCacheEntities.get(0);
                boolean isExpired = encKeyCacheEntity.expire_time_long == 0 || now > encKeyCacheEntity.expire_time_long;
                if (isExpired) {
                    if (TextUtils.isEmpty(encKeyCacheEntity.enc_key) || TextUtils.isEmpty(encKeyCacheEntity.enc_iv)) {
                        return Single.just("need-to-re-enter-password");//expired, need password
                    } else {
                        String decryptPassword = SecurePasswordManager.decryptPassword(encKeyCacheEntity.enc_key, encKeyCacheEntity.enc_iv);
                        return Single.just(decryptPassword);//expired, but no password
                    }
                }

                return Single.just("done");
            }
        });


        addSingleDisposable(s, new Consumer<String>() {
            @Override
            public void accept(String i) throws Exception {
                consumer.accept(i);
            }
        });
    }

    public void remoteVerify(String repoId, String password, Consumer<ResultModel> consumer) {
        if (consumer == null) {
            throw new IllegalArgumentException("consumer is null");
        }
        getRefreshLiveData().setValue(true);

        Map<String, String> requestDataMap = new HashMap<>();
        requestDataMap.put("password", password);
        Map<String, RequestBody> bodyMap = genRequestBody(requestDataMap);

        Single<ResultModel> netSingle = HttpIO.getCurrentInstance().execute(DialogService.class).setPassword(repoId, bodyMap);
        Single<ResultModel> single = netSingle.flatMap(new Function<ResultModel, SingleSource<ResultModel>>() {
            @Override
            public SingleSource<ResultModel> apply(ResultModel resultModel) throws Exception {

                //update local password and expire
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


                return Single.just(resultModel);
            }
        });

        addSingleDisposable(single, tResultModel -> {
            getRefreshLiveData().setValue(false);
            consumer.accept(tResultModel);
        }, throwable -> {
            getRefreshLiveData().setValue(false);

            TResultModel<RepoModel> tResultModel = new TResultModel<>();
            tResultModel.error_msg = getErrorMsgByThrowable(throwable);
            consumer.accept(tResultModel);
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

        removeAllPermission();

        Single<List<BaseModel>> singleDB = AppDatabase.getInstance().repoDao().getListByAccount(account.getSignature())
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
        });
    }

    private void loadReposFromRemote(Account account) {
        if (!NetworkUtils.isConnected()) {
            getRefreshLiveData().setValue(false);
            return;
        }

        removeAllPermission();

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

                SeafException seafException = getExceptionByThrowable(throwable);
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

        List<Single<?>> singles = new ArrayList<>();
        singles.add(direntDBSingle);

        if (isRepoCustomPermission && isNotExistsRepoPermission()) {
            //get special number permission from db
            int pNum = repoModel.getCustomPermissionNum();
            Single<List<PermissionEntity>> pSingle = AppDatabase.getInstance().permissionDAO().getByRepoAndIdAsync(repoId, pNum);
            singles.add(pSingle);
        }

        return Single.zip(singles, new Function<Object[], List<DirentModel>>() {
            @Override
            public List<DirentModel> apply(Object[] results) throws Exception {

                List<CachedDirentModel> cachedDirentList = (List<CachedDirentModel>) results[0];
                if (CollectionUtils.isEmpty(cachedDirentList)) {
                    return CollectionUtils.newArrayList();
                }

                List<DirentModel> direntModels = cachedDirentList.stream().map(new java.util.function.Function<CachedDirentModel, DirentModel>() {
                    @Override
                    public DirentModel apply(CachedDirentModel cachedDirentModel) {
                        cachedDirentModel.dirent.local_file_id = cachedDirentModel.local_file_id;
                        return cachedDirentModel.dirent;
                    }
                }).collect(Collectors.toList());

                //cache repo permission
                if (isRepoCustomPermission && isNotExistsRepoPermission()) {
                    List<PermissionEntity> permissionList = (List<PermissionEntity>) results[1];
                    if (!CollectionUtils.isEmpty(permissionList)) {
                        addRepoPermissionIntoMap(permissionList.get(0));
                    }
                } else {
                    addRepoPermissionIntoMap(new PermissionEntity(repoId, repoModel.permission));
                }

                return direntModels;
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
        List<Single<?>> singles = new ArrayList<>();
        singles.add(direntSingle);

        boolean isRepoCustomPermission = navContext.getRepoModel().isCustomPermission();
        if (isRepoCustomPermission) {
            Single<PermissionWrapperModel> permissionWrapperModelSingle = HttpIO.getCurrentInstance().execute(RepoService.class).getCustomSharePermissionById(repoId, repoModel.getCustomPermissionNum());
            singles.add(permissionWrapperModelSingle);
        }

        Single<List<DirentModel>> r = Single.zip(singles, new Function<Object[], List<DirentModel>>() {
            @Override
            public List<DirentModel> apply(Object[] results) throws Exception {

                List<DirentModel> direntList = (List<DirentModel>) results[0];
                if (CollectionUtils.isEmpty(direntList)) {
                    return direntList;
                }

                //cache repo permission
                if (isRepoCustomPermission) {
                    PermissionWrapperModel permissionWrapperModel = (PermissionWrapperModel) results[1];
                    if (permissionWrapperModel != null) {
                        addRepoPermissionIntoMap(new PermissionEntity(repoId, permissionWrapperModel.permission));
                    }
                } else {
                    addRepoPermissionIntoMap(new PermissionEntity(repoId, repoModel.permission));
                }

                return direntList;
            }
        });

        addSingleDisposable(r, new Consumer<List<DirentModel>>() {
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

                SeafException seafException = getExceptionByThrowable(throwable);
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
        return repoSingle.flatMap(new Function<List<RepoModel>, SingleSource<Pair<RepoModel, PermissionEntity>>>() {
            @Override
            public SingleSource<Pair<RepoModel, PermissionEntity>> apply(List<RepoModel> repoModels) throws Exception {
                if (CollectionUtils.isEmpty(repoModels)) {
                    return Single.just(new Pair<>(null, null));
                }

                RepoModel repoModel = repoModels.get(0);
                if (repoModel.isCustomPermission()) {
                    return Single.just(new Pair<>(repoModel, new PermissionEntity()));
                }

                return Single.just(new Pair<>(repoModel, new PermissionEntity(repoId, repoModel.permission)));

            }
        }).flatMap(new Function<Pair<RepoModel, PermissionEntity>, SingleSource<Pair<RepoModel, PermissionEntity>>>() {
            @Override
            public SingleSource<Pair<RepoModel, PermissionEntity>> apply(Pair<RepoModel, PermissionEntity> pair) throws Exception {

                if (pair.first == null) {
                    return Single.error(SeafException.NOT_FOUND_EXCEPTION);
                }

                if (pair.second.isValid()) {
                    return Single.just(pair);
                }

                RepoModel repoModel = pair.first;
                Single<List<PermissionEntity>> pSingle = AppDatabase.getInstance().permissionDAO().getByRepoAndIdAsync(repoId, repoModel.getCustomPermissionNum());
                return pSingle.flatMap((Function<List<PermissionEntity>, SingleSource<Pair<RepoModel, PermissionEntity>>>) pList -> {
                    //no data in local db
                    if (CollectionUtils.isEmpty(pList)) {
                        return Single.just(new Pair<>(pair.first, new PermissionEntity()));
                    }

                    //get first permission
                    return Single.just(new Pair<>(pair.first, pList.get(0)));
                });
            }
        }).flatMap(new Function<Pair<RepoModel, PermissionEntity>, SingleSource<Pair<RepoModel, PermissionEntity>>>() {
            @Override
            public SingleSource<Pair<RepoModel, PermissionEntity>> apply(Pair<RepoModel, PermissionEntity> pair) throws Exception {
                if (pair.second.isValid()) {
                    return Single.just(pair);
                }

                Single<PermissionEntity> permissionSingle = getSingleForLoadRepoPermissionFromRemote(repoId, pair.first.getCustomPermissionNum());
                return permissionSingle.flatMap(new Function<PermissionEntity, SingleSource<Pair<RepoModel, PermissionEntity>>>() {
                    @Override
                    public SingleSource<Pair<RepoModel, PermissionEntity>> apply(PermissionEntity p1) throws Exception {
                        if (p1.isValid()) {
                            return Single.just(new Pair<>(pair.first, p1));
                        }

                        return Single.just(new Pair<>(pair.first, new PermissionEntity()));
                    }
                });
            }
        });

    }

    /**
     * get the repoModel and repoMode‘s PermissionEntity from local, if not exist, get from remote.
     */
    private Single<PermissionEntity> getSingleForLoadRepoModelAndPermission(String repoId, int pNum) {
        //get permission from db by special number

        Single<List<PermissionEntity>> pSingle = AppDatabase.getInstance().permissionDAO().getByRepoAndIdAsync(repoId, pNum);
        return pSingle.flatMap((Function<List<PermissionEntity>, SingleSource<PermissionEntity>>) pList -> {
            //no data in local db
            if (CollectionUtils.isEmpty(pList)) {
                return Single.just(new PermissionEntity());
            }

            //get first permission
            return Single.just(pList.get(0));
        }).flatMap(new Function<PermissionEntity, SingleSource<PermissionEntity>>() {
            @Override
            public SingleSource<PermissionEntity> apply(PermissionEntity p) throws Exception {
                if (p.isValid()) {
                    return Single.just(p);
                }

                Single<PermissionEntity> permissionSingle = getSingleForLoadRepoPermissionFromRemote(repoId, pNum);
                return permissionSingle.flatMap(new Function<PermissionEntity, SingleSource<PermissionEntity>>() {
                    @Override
                    public SingleSource<PermissionEntity> apply(PermissionEntity p1) throws Exception {
                        return Single.just(p1);
                    }
                });
            }
        });

    }

    private Single<PermissionEntity> getSingleForLoadRepoPermissionFromRemote(String repoId, int pNum) {
        Single<PermissionWrapperModel> single = HttpIO.getCurrentInstance().execute(RepoService.class).getCustomSharePermissionById(repoId, pNum);
        return single.flatMap(new Function<PermissionWrapperModel, SingleSource<PermissionEntity>>() {
            @Override
            public SingleSource<PermissionEntity> apply(PermissionWrapperModel wrapperModel) throws Exception {
                if (wrapperModel == null || wrapperModel.permission == null) {
                    return Single.just(new PermissionEntity());
                }

                PermissionEntity permission = new PermissionEntity(repoId, wrapperModel.permission);

                AppDatabase.getInstance().permissionDAO().insert(permission);
                SLogs.d("The list has been inserted into the local database");

                return Single.just(permission);
            }
        });
    }

    public void inflateRepoMenu(Context context) {

        removeAllPermission();

        toParseMenu(context, R.menu.bottom_sheet_op_repo, null, CollectionUtils.newArrayList(R.id.unstar));
    }

    /**
     * @param selectedRepoModels
     */
    public void inflateRepoMenuWithSelected(Context context, List<RepoModel> selectedRepoModels, List<Integer> disableMenuIds, List<Integer> removedMenuIds) {
        if (CollectionUtils.isEmpty(selectedRepoModels)) {
            inflateRepoMenu(context);
            return;
        }

        int menuId = R.menu.bottom_sheet_op_repo;
        if (selectedRepoModels.size() == 1) {
            RepoModel repoModel = selectedRepoModels.get(0);
            if (repoModel.isCustomPermission()) {
                getRefreshLiveData().setValue(true);
                Single<PermissionEntity> r = getSingleForLoadRepoModelAndPermission(repoModel.repo_id, repoModel.getCustomPermissionNum());
                addSingleDisposable(r, new Consumer<PermissionEntity>() {
                    @Override
                    public void accept(PermissionEntity permission) throws Exception {
                        getRefreshLiveData().setValue(false);
                        List<PermissionEntity> pList = !permission.isValid() ? null : CollectionUtils.newArrayList(permission);
                        toParseMenu(context, menuId, pList, disableMenuIds, removedMenuIds);
                    }
                }, new Consumer<Throwable>() {
                    @Override
                    public void accept(Throwable throwable) throws Exception {
                        getRefreshLiveData().setValue(false);
                        toParseMenu(context, menuId, disableMenuIds, removedMenuIds);
                    }
                });
            } else {
                List<PermissionEntity> permissionEntities = CollectionUtils.newArrayList(new PermissionEntity(repoModel.repo_id, repoModel.permission));
                toParseMenu(context, menuId, permissionEntities, disableMenuIds, removedMenuIds);
            }

        } else {

            //
            List<PermissionEntity> permissionEntities = CollectionUtils.newArrayList();
            for (RepoModel repoModel : selectedRepoModels) {
                //NOTICE this is a special permission("r"), not a real permission
                //because: currently, multiple repo lists cannot be deleted at the same time
                //it will be fixed later
                permissionEntities.add(new PermissionEntity(repoModel.repo_id, "r"));
            }

            toParseMenu(context, menuId, permissionEntities, disableMenuIds, removedMenuIds);
        }
    }

    public void inflateDirentMenu(Context context) {
        removeNonRepoPermission();

        toParseMenu(context, R.menu.bottom_sheet_op_dirent, null, null, CollectionUtils.newArrayList(R.id.unstar));
    }

    public void inflateDirentMenuWithSelected(Context context, List<DirentModel> selectedDirentList, List<Integer> disableMenuIds, List<Integer> removedMenuIds) {
        if (CollectionUtils.isEmpty(selectedDirentList)) {
            inflateDirentMenu(context);
            return;
        }

        removeNonRepoPermission();

        for (DirentModel model : selectedDirentList) {
            if (model.isCustomPermission()) {
                //custom permission is same as repo permission
                continue;
            }

            cachePermissionMapData(model.permission, new PermissionEntity(model.repo_id, model.permission));
        }

        int menuId = R.menu.bottom_sheet_op_dirent;

        if (selectedDirentList.size() == 1) {
            PermissionEntity repoPerm = getRepoPermissionFromMap();
            DirentModel direntModel = selectedDirentList.get(0);

            List<PermissionEntity> permissionList = null;
            if (direntModel.isCustomPermission()) {
                if (direntModel.getCustomPermissionNum() == repoPerm.id) {
                    permissionList = new ArrayList<>(CollectionUtils.newArrayList(repoPerm));
                } else {

                }
            } else if (direntModel.permission.equals(repoPerm.name)) {
                permissionList = new ArrayList<>(CollectionUtils.newArrayList(repoPerm));
            } else {
                //dirent's permissions can only be one of these 5 permission: "rw"/"r"/"cloud-edit"/"cloud-preview"/"manage"
                permissionList = new ArrayList<>(CollectionUtils.newArrayList(new PermissionEntity(direntModel.repo_id, direntModel.permission)));
            }

            toParseMenu(context, menuId, permissionList, disableMenuIds, removedMenuIds);
        } else {
            List<PermissionEntity> permissionList = new ArrayList<>(getCurrentNavPermissionCacheMap().values());
            toParseMenu(context, menuId, permissionList, disableMenuIds, removedMenuIds);
        }


    }


    private void cachePermissionMapData(String permissionName, @NonNull PermissionEntity entity) {

        if (getCurrentNavPermissionCacheMap().containsKey(permissionName)) {
            return;
        }

        getCurrentNavPermissionCacheMap().put(permissionName, entity);
    }

    private void toParseMenu(Context context, int menuId, List<PermissionEntity> permissionList, List<Integer> disableMenuIds, List<Integer> removedMenuIds) {
        List<MenuItem> items = parseMenu(context, menuId, permissionList, disableMenuIds, removedMenuIds);
        getMenuItemListLiveData().setValue(items);
    }

    private void toParseMenu(Context context, int menuId, List<Integer> disableMenuIds, List<Integer> removedMenuIds) {
        List<PermissionEntity> permissionList = new ArrayList<>(getCurrentNavPermissionCacheMap().values());
        List<MenuItem> items = parseMenu(context, menuId, permissionList, disableMenuIds, removedMenuIds);
        getMenuItemListLiveData().setValue(items);
    }

    private List<MenuItem> parseMenu(Context context, int menuId, List<PermissionEntity> permissionList, List<Integer> disableMenuIds, List<Integer> removedMenuIds) {
        List<MenuItem> items = inflateMenu(context, menuId);

        //if no permission list, disable all menu
        if (CollectionUtils.isEmpty(permissionList)) {
            for (MenuItem item : items) {
                item.setEnabled(false);
            }

            //
            if (!CollectionUtils.isEmpty(removedMenuIds)) {
                items = items.stream().filter(item -> item.getItemId() != R.id.unstar).collect(Collectors.toList());
            }
            return items;
        }

        //enable firstly
        for (MenuItem item : items) {
            item.setEnabled(true);
        }

        //to disable
        for (MenuItem item : items) {
            if (!item.isEnabled()) {
                continue;
            }

            if (item.getItemId() == R.id.rename) {
                long l = permissionList.stream().filter(f -> !f.modify).count();
                item.setEnabled(!(l > 0));
            } else if (item.getItemId() == R.id.move) {
                long l = permissionList.stream().filter(f -> !f.modify).count();
                item.setEnabled(!(l > 0));
            } else if (item.getItemId() == R.id.copy) {
                long l = permissionList.stream().filter(f -> !f.copy).count();
                item.setEnabled(!(l > 0));
            } else if (item.getItemId() == R.id.delete) {
                long l = permissionList.stream().filter(f -> !f.delete).count();
                item.setEnabled(!(l > 0));
            } else if (item.getItemId() == R.id.upload) {
                long l = permissionList.stream().filter(f -> !f.upload).count();
                item.setEnabled(!(l > 0));
            } else if (item.getItemId() == R.id.download) {
                long l = permissionList.stream().filter(f -> !f.download).count();
                item.setEnabled(!(l > 0));
            } else if (item.getItemId() == R.id.share) {
                long l = permissionList.stream().filter(f -> !f.download_external_link).count();
                item.setEnabled(!(l > 0));
            } else if (item.getItemId() == R.id.export) {
                long l = permissionList.stream().filter(f -> !f.download).count();
                item.setEnabled(!(l > 0));
            } else if (item.getItemId() == R.id.open) {
                long l = permissionList.stream().filter(f -> !f.download).count();
                item.setEnabled(!(l > 0));
            }

            if (!CollectionUtils.isEmpty(disableMenuIds)) {
                if (disableMenuIds.contains(item.getItemId())) {
                    item.setEnabled(false);
                }
            }
        }

        if (!CollectionUtils.isEmpty(removedMenuIds)) {
            items = items.stream().filter(item -> !removedMenuIds.contains(item.getItemId())).collect(Collectors.toList());
        }

        return items;
    }

    private List<MenuItem> inflateMenu(Context context, int rid) {
        ActionMenu menu = new ActionMenu(context);

        MenuInflater inflater = new MenuInflater(context);
        inflater.inflate(rid, menu);

        List<MenuItem> items = new ArrayList<>(menu.size());
        for (int i = 0; i < menu.size(); i++) {
            items.add(menu.getItem(i));
        }

        return items;
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
                ToastUtils.showLong(errMsg);
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
}
