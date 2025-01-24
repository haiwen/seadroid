package com.seafile.seadroid2.ui.repo;

import android.content.Context;
import android.text.TextUtils;
import android.view.MenuInflater;
import android.view.MenuItem;

import androidx.annotation.NonNull;
import androidx.lifecycle.MutableLiveData;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.NetworkUtils;
import com.blankj.utilcode.util.ToastUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.enums.RefreshStatusEnum;
import com.seafile.seadroid2.framework.data.model.permission.PermissionWrapperModel;
import com.seafile.seadroid2.ui.bottomsheetmenu.ActionMenu;
import com.seafile.seadroid2.context.NavContext;
import com.seafile.seadroid2.enums.FileViewType;
import com.seafile.seadroid2.framework.data.db.AppDatabase;
import com.seafile.seadroid2.framework.data.db.entities.DirentModel;
import com.seafile.seadroid2.framework.data.db.entities.EncKeyCacheEntity;
import com.seafile.seadroid2.framework.data.db.entities.FileTransferEntity;
import com.seafile.seadroid2.framework.data.db.entities.PermissionEntity;
import com.seafile.seadroid2.framework.data.db.entities.RepoModel;
import com.seafile.seadroid2.framework.data.model.BaseModel;
import com.seafile.seadroid2.framework.data.model.ResultModel;
import com.seafile.seadroid2.enums.TransferStatus;
import com.seafile.seadroid2.framework.data.model.repo.Dirent2Model;
import com.seafile.seadroid2.framework.http.HttpIO;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.preferences.Settings;
import com.seafile.seadroid2.ui.base.viewmodel.BaseViewModel;
import com.seafile.seadroid2.framework.util.Objs;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.ui.star.StarredService;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

import io.reactivex.Flowable;
import io.reactivex.Single;
import io.reactivex.SingleSource;
import io.reactivex.functions.Action;
import io.reactivex.functions.Consumer;
import io.reactivex.functions.Function;
import kotlin.Pair;
import okhttp3.RequestBody;

public class RepoViewModel extends BaseViewModel {

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


    public void loadData(NavContext context, RefreshStatusEnum refreshStatus) {
        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        if (account == null) {
            return;
        }

        //force refresh
        if (RefreshStatusEnum.REMOTE == refreshStatus) {
            if (context.inRepo()) {
                loadDirentsFromRemote(account, context);
            } else {
                loadReposFromRemote(account);
            }
        } else if (RefreshStatusEnum.LOCAL_BEFORE_REMOTE == refreshStatus) {
            if (context.inRepo()) {
                FileViewType fileViewType = Settings.FILE_LIST_VIEW_TYPE.queryValue();
                if (FileViewType.GALLERY == fileViewType) {
                    loadDirentsFromLocalWithGalleryViewType(account, context, true);
                } else {
                    loadDirentsFromLocal(account, context, true);
                }
            } else {
                loadReposFromLocal(account, true);
            }
        } else if (RefreshStatusEnum.ONLY_LOCAL == refreshStatus) {
            if (context.inRepo()) {
                FileViewType fileViewType = Settings.FILE_LIST_VIEW_TYPE.queryValue();
                if (FileViewType.GALLERY == fileViewType) {
                    loadDirentsFromLocalWithGalleryViewType(account, context, false);
                } else {
                    loadDirentsFromLocal(account, context, false);
                }
            } else {
                loadReposFromLocal(account, false);
            }
        } else {
            //RefreshStatusEnum.NO: do nothing
        }
    }

    private void loadReposFromLocal(Account account, boolean isLoadRemoteData) {
        removeAllPermission();

        if (isLoadRemoteData) {
            getRefreshLiveData().setValue(true);
        }

        Single<List<RepoModel>> singleDB = AppDatabase.getInstance().repoDao().getListByAccount(account.getSignature());
        addSingleDisposable(singleDB, new Consumer<List<RepoModel>>() {
            @Override
            public void accept(List<RepoModel> repoModels) {

                if (CollectionUtils.isEmpty(repoModels)) {
                    loadReposFromRemote(account);
                    return;
                }

                List<BaseModel> list = Objs.convertToAdapterList(repoModels, false);
                if (isLoadRemoteData) {
                    if (!CollectionUtils.isEmpty(list)) {
                        getObjListLiveData().setValue(list);
                    }
                    loadReposFromRemote(account);
                } else {
                    getObjListLiveData().setValue(list);
                    getRefreshLiveData().setValue(false);
                }
            }
        });
    }

    private void loadReposFromRemote(Account account) {
        removeAllPermission();

        if (!NetworkUtils.isConnected()) {
            getRefreshLiveData().setValue(false);
            return;
        }

        //load net data and load local data
        Single<List<BaseModel>> resultSingle = Objs.getReposSingleFromServer(account);

        addSingleDisposable(resultSingle, new Consumer<List<BaseModel>>() {
            @Override
            public void accept(List<BaseModel> models) throws Exception {
                getObjListLiveData().setValue(models);
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
                getSeafExceptionLiveData().setValue(seafException);
            }
        });
    }

    private Single<List<DirentModel>> getLoadDirentsFromLocalSingle(Account account, NavContext navContext) {
        RepoModel repoModel = navContext.getRepoModel();

        String repoId = repoModel.repo_id;
        String parentDir = navContext.getNavPath();
        boolean isRepoCustomPermission = repoModel.isCustomPermission();
        String repoPermName = repoModel.permission;

        if (!parentDir.endsWith("/")) {
            parentDir = parentDir + "/";
        }

        Single<List<DirentModel>> direntDBSingle = AppDatabase.getInstance().direntDao().getListByParentPathAsync(repoId, parentDir);
        Single<List<FileTransferEntity>> curParentDownloadedList = AppDatabase.getInstance().fileTransferDAO().getDownloadedListByParentAsync(repoId, parentDir);

        List<Single<?>> singles = new ArrayList<>();
        singles.add(direntDBSingle);
        singles.add(curParentDownloadedList);

        if (isRepoCustomPermission && isNotExistsRepoPermission()) {
            //get special number permission from db
            int pNum = repoModel.getCustomPermissionNum();
            Single<List<PermissionEntity>> pSingle = AppDatabase.getInstance().permissionDAO().getByRepoAndIdAsync(repoId, pNum);
            singles.add(pSingle);
        }

        return Single.zip(singles, new Function<Object[], List<DirentModel>>() {
            @Override
            public List<DirentModel> apply(Object[] results) throws Exception {

                List<DirentModel> direntList = (List<DirentModel>) results[0];
                if (CollectionUtils.isEmpty(direntList)) {
                    return direntList;
                }

                List<FileTransferEntity> downloadedList = (List<FileTransferEntity>) results[1];
                if (!CollectionUtils.isEmpty(downloadedList)) {
                    for (DirentModel direntModel : direntList) {
                        String fullPath = direntModel.parent_dir + direntModel.name;
                        Optional<FileTransferEntity> firstOp = downloadedList.stream().filter(f -> TextUtils.equals(fullPath, f.full_path)).findFirst();
                        if (firstOp.isPresent()) {
                            FileTransferEntity entity = firstOp.get();
                            if (entity.transfer_status == TransferStatus.SUCCEEDED) {
                                direntModel.transfer_status = entity.transfer_status;
                                direntModel.local_file_path = entity.target_path;
                            }
                        }
                    }
                }

                //cache repo permission
                if (isRepoCustomPermission && isNotExistsRepoPermission()) {
                    List<PermissionEntity> permissionList = (List<PermissionEntity>) results[2];
                    if (!CollectionUtils.isEmpty(permissionList)) {
                        addRepoPermissionIntoMap(permissionList.get(0));
                    }
                } else {
                    addRepoPermissionIntoMap(new PermissionEntity(repoId, repoModel.permission));
                }

                return direntList;
            }
        });
    }

    private void loadDirentsFromLocalWithGalleryViewType(Account account, NavContext navContext, boolean isLoadRemoteData) {
        getRefreshLiveData().setValue(true);

        Single<List<DirentModel>> r = getLoadDirentsFromLocalSingle(account, navContext);
        addSingleDisposable(r, new Consumer<List<DirentModel>>() {
            @Override
            public void accept(List<DirentModel> direntModels) throws Exception {

                List<DirentModel> rets = CollectionUtils.newArrayList();
                for (DirentModel direntModel : direntModels) {
                    if (Utils.isViewableImage(direntModel.name) || Utils.isVideoFile(direntModel.name)) {
                        rets.add(direntModel);
                    }
                }

                if (isLoadRemoteData) {
                    loadDirentsFromRemote(account, navContext);
                } else {
                    getObjListLiveData().setValue(Objs.parseLocalDirents(rets));
                    getRefreshLiveData().setValue(false);
                }
            }
        });
    }

    private void loadDirentsFromLocal(Account account, NavContext navContext, boolean isLoadRemoteData) {
        getRefreshLiveData().setValue(true);

        Single<List<DirentModel>> r = getLoadDirentsFromLocalSingle(account, navContext);

        addSingleDisposable(r, new Consumer<List<DirentModel>>() {
            @Override
            public void accept(List<DirentModel> direntModels) throws Exception {

                List<BaseModel> bs = Objs.parseLocalDirents(direntModels);

                if (isLoadRemoteData) {
                    if (!CollectionUtils.isEmpty(bs)) {
                        getObjListLiveData().setValue(new ArrayList<>(bs));
                    }
                    loadDirentsFromRemote(account, navContext);
                } else {
                    getObjListLiveData().setValue(new ArrayList<>(bs));
                    getRefreshLiveData().setValue(false);
                }
            }
        });
    }

    private void loadDirentsFromRemote(Account account, NavContext navContext) {
        if (!NetworkUtils.isConnected()) {
            getRefreshLiveData().setValue(false);
            getSeafExceptionLiveData().setValue(SeafException.NETWORK_EXCEPTION);
            return;
        }

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
                if (FileViewType.GALLERY == fileViewType) {
                    List<DirentModel> rets = CollectionUtils.newArrayList();
                    for (DirentModel direntModel : direntModels) {
                        if (Utils.isViewableImage(direntModel.name) || Utils.isVideoFile(direntModel.name)) {
                            rets.add(direntModel);
                        }
                    }

                    getObjListLiveData().setValue(new ArrayList<>(rets));
                } else {
                    getObjListLiveData().setValue(new ArrayList<>(direntModels));
                }

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
                getSeafExceptionLiveData().setValue(seafException);
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
        Single<Pair<RepoModel, PermissionEntity>> r = getRepoModelAndAllPermissionSingle(repoId);
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
    private Single<Pair<RepoModel, PermissionEntity>> getRepoModelAndAllPermissionSingle(String repoId) {
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

                if (pair.getFirst() == null) {
                    return Single.error(SeafException.NOT_FOUND_EXCEPTION);
                }

                if (pair.getSecond().isValid()) {
                    return Single.just(pair);
                }

                RepoModel repoModel = pair.getFirst();
                Single<List<PermissionEntity>> pSingle = AppDatabase.getInstance().permissionDAO().getByRepoAndIdAsync(repoId, repoModel.getCustomPermissionNum());
                return pSingle.flatMap((Function<List<PermissionEntity>, SingleSource<Pair<RepoModel, PermissionEntity>>>) pList -> {
                    //no data in local db
                    if (CollectionUtils.isEmpty(pList)) {
                        return Single.just(new Pair<>(pair.getFirst(), new PermissionEntity()));
                    }

                    //get first permission
                    return Single.just(new Pair<>(pair.getFirst(), pList.get(0)));
                });
            }
        }).flatMap(new Function<Pair<RepoModel, PermissionEntity>, SingleSource<Pair<RepoModel, PermissionEntity>>>() {
            @Override
            public SingleSource<Pair<RepoModel, PermissionEntity>> apply(Pair<RepoModel, PermissionEntity> pair) throws Exception {
                if (pair.getSecond().isValid()) {
                    return Single.just(pair);
                }

                Single<PermissionEntity> permissionSingle = getLoadRepoPermissionFromRemoteSingle(repoId, pair.getFirst().getCustomPermissionNum());
                return permissionSingle.flatMap(new Function<PermissionEntity, SingleSource<Pair<RepoModel, PermissionEntity>>>() {
                    @Override
                    public SingleSource<Pair<RepoModel, PermissionEntity>> apply(PermissionEntity p1) throws Exception {
                        if (p1.isValid()) {
                            return Single.just(new Pair<>(pair.getFirst(), p1));
                        }

                        return Single.just(new Pair<>(pair.getFirst(), new PermissionEntity()));
                    }
                });
            }
        });

    }

    /**
     * get the repoModel and repoMode‘s PermissionEntity from local, if not exist, get from remote.
     */
    private Single<PermissionEntity> getRepoModelAndPermissionSingle(String repoId, int pNum) {
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

                Single<PermissionEntity> permissionSingle = getLoadRepoPermissionFromRemoteSingle(repoId, pNum);
                return permissionSingle.flatMap(new Function<PermissionEntity, SingleSource<PermissionEntity>>() {
                    @Override
                    public SingleSource<PermissionEntity> apply(PermissionEntity p1) throws Exception {
                        return Single.just(p1);
                    }
                });
            }
        });

    }

    private Single<PermissionEntity> getLoadRepoPermissionFromRemoteSingle(String repoId, int pNum) {
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
                Single<PermissionEntity> r = getRepoModelAndPermissionSingle(repoModel.repo_id, repoModel.getCustomPermissionNum());
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
                    //没有这个情况
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

                singles.add(getStarSingle(m.repo_id, "/", isStar));
            } else if (baseModel instanceof DirentModel m) {
                if (isStar == m.starred) {
                    continue;
                }

                singles.add(getStarSingle(m.repo_id, m.full_path, isStar));
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

    private Flowable<?> getStarSingle(String repoId, String path, boolean isStar) {
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
