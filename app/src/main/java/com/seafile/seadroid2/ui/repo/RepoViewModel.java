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
import com.seafile.seadroid2.annotation.Todo;
import com.seafile.seadroid2.annotation.Unstable;
import com.seafile.seadroid2.framework.data.model.repo.RepoPermissionWrapper;
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
import com.seafile.seadroid2.framework.data.model.permission.PermissionListWrapperModel;
import com.seafile.seadroid2.framework.data.model.permission.PermissionWrapperModel;
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
import java.util.stream.Collectors;

import io.reactivex.Completable;
import io.reactivex.Flowable;
import io.reactivex.Single;
import io.reactivex.SingleSource;
import io.reactivex.functions.Action;
import io.reactivex.functions.BiFunction;
import io.reactivex.functions.Consumer;
import io.reactivex.functions.Function;
import kotlin.Pair;
import okhttp3.RequestBody;

public class RepoViewModel extends BaseViewModel {

    private final MutableLiveData<List<BaseModel>> _objListLiveData = new MutableLiveData<>();
    private final MutableLiveData<Boolean> _starredLiveData = new MutableLiveData<>();

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
                loadReposFromLocal(account, refreshStatus);//same to ONLY_LOCAL
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
                loadReposFromLocal(account, refreshStatus);//here
            }
        } else {
            //RefreshStatusEnum.NO: do nothing
        }

    }

    private void loadReposFromLocal(Account account, RefreshStatusEnum refreshStatus) {
        if (RefreshStatusEnum.REMOTE == refreshStatus) {
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

                List<BaseModel> list = Objs.parseRepoListForAdapter(repoModels, account.getSignature(), false);
                getObjListLiveData().setValue(list);

                if (RefreshStatusEnum.REMOTE == refreshStatus) {
                    loadReposFromRemote(account);
                } else {
                    getRefreshLiveData().setValue(false);
                }
            }
        });
    }

    private void loadReposFromRemote(Account account) {
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
                if (seafException == SeafException.remoteWipedException) {
                    //post a request
                    completeRemoteWipe();
                }
                getSeafExceptionLiveData().setValue(seafException);
            }
        });
    }

    private void loadDirentsFromLocalWithGalleryViewType(Account account, NavContext navContext, boolean isLoadRemoteData) {
        getRefreshLiveData().setValue(true);

        String repoId = navContext.getRepoModel().repo_id;
        String parentDir = navContext.getNavPath();

        Single<List<DirentModel>> direntDBSingle = AppDatabase.getInstance().direntDao().getListByParentPathAsync(repoId, parentDir);
        addSingleDisposable(direntDBSingle, new Consumer<List<DirentModel>>() {
            @Override
            public void accept(List<DirentModel> direntModels) throws Exception {

                List<DirentModel> rets = CollectionUtils.newArrayList();
                for (DirentModel direntModel : direntModels) {
                    if (Utils.isViewableImage(direntModel.name) || Utils.isVideoFile(direntModel.name)) {
                        rets.add(direntModel);
                    }
                }

                if (isLoadRemoteData || CollectionUtils.isEmpty(rets)) {
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

        String repoId = navContext.getRepoModel().repo_id;
        String parentDir = navContext.getNavPath();
        if (!parentDir.endsWith("/")) {
            parentDir = parentDir + "/";
        }

        Single<List<DirentModel>> direntDBSingle = AppDatabase.getInstance().direntDao().getListByParentPathAsync(repoId, parentDir);
        Single<List<FileTransferEntity>> curParentDownloadedList = AppDatabase.getInstance().fileTransferDAO().getDownloadedListByParentAsync(repoId, parentDir);

        Single<List<DirentModel>> resultSingle = Single.zip(direntDBSingle, curParentDownloadedList, new BiFunction<List<DirentModel>, List<FileTransferEntity>, List<DirentModel>>() {
            @Override
            public List<DirentModel> apply(List<DirentModel> direntModels, List<FileTransferEntity> cur_parent_downloaded_list) throws Exception {
                if (CollectionUtils.isEmpty(direntModels)) {
                    return direntModels;
                }

                for (DirentModel direntModel : direntModels) {
                    String fullPath = direntModel.parent_dir + direntModel.name;
                    Optional<FileTransferEntity> firstOp = cur_parent_downloaded_list.stream().filter(f -> TextUtils.equals(fullPath, f.full_path)).findFirst();
                    if (firstOp.isPresent()) {
                        FileTransferEntity entity = firstOp.get();
                        if (entity.transfer_status == TransferStatus.SUCCEEDED) {
                            direntModel.transfer_status = entity.transfer_status;
                            direntModel.local_file_path = entity.target_path;
                        }
                    }
                }

                return direntModels;
            }
        });

        addSingleDisposable(resultSingle, new Consumer<List<DirentModel>>() {
            @Override
            public void accept(List<DirentModel> direntModels) throws Exception {

                if (CollectionUtils.isEmpty(direntModels)) {
                    loadDirentsFromRemote(account, navContext);
                } else {
                    getObjListLiveData().setValue(Objs.parseLocalDirents(direntModels));
                    if (isLoadRemoteData) {
                        loadDirentsFromRemote(account, navContext);
                    } else {
                        getRefreshLiveData().setValue(false);
                    }
                }
            }
        });
    }

    private void loadDirentsFromRemote(Account account, NavContext context) {
        if (!NetworkUtils.isConnected()) {
            getRefreshLiveData().setValue(false);
            getSeafExceptionLiveData().setValue(SeafException.networkException);
            return;
        }

        String repoId = context.getRepoModel().repo_id;
        String repoName = context.getRepoModel().repo_name;
        String parentDir = context.getNavPath();

        if ("/".equals(parentDir)) {
            loadPermissionFromRemote(repoId);
        }

        if (!parentDir.endsWith("/")) {
            parentDir = parentDir + "/";
        }

        Single<List<DirentModel>> resultSingle = Objs.getDirentsSingleFromServer(account, repoId, repoName, parentDir);

        addSingleDisposable(resultSingle, new Consumer<List<DirentModel>>() {
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
                if (seafException == SeafException.remoteWipedException) {
                    //post a request
                    completeRemoteWipe();
                }
                getSeafExceptionLiveData().setValue(seafException);
            }
        });
    }

    public List<MenuItem> inflateMenu(Context context, int rid) {
        ActionMenu menu = new ActionMenu(context);

        MenuInflater inflater = new MenuInflater(context);
        inflater.inflate(rid, menu);

        List<MenuItem> items = new ArrayList<>(menu.size());
        for (int i = 0; i < menu.size(); i++) {
            items.add(menu.getItem(i));
        }

        return items;
    }

    private final MutableLiveData<List<MenuItem>> _menuItemListLiveData = new MutableLiveData<>();

    public MutableLiveData<List<MenuItem>> getMenuItemListLiveData() {
        return _menuItemListLiveData;
    }

    public void getRepoModelAndPermissionEntity(String repoId, boolean isForce, Consumer<RepoPermissionWrapper> consumer) {
        Single<Pair<RepoModel, List<PermissionEntity>>> r = getRepoModelAndAllPermissionSingle(repoId, isForce);
        addSingleDisposable(r, new Consumer<Pair<RepoModel, List<PermissionEntity>>>() {
            @Override
            public void accept(Pair<RepoModel, List<PermissionEntity>> pair) throws Exception {
                if (consumer != null) {
                    if (CollectionUtils.isEmpty(pair.getSecond())) {
                        consumer.accept(new RepoPermissionWrapper(pair.getFirst(), null));
                        return;
                    }

                    List<PermissionEntity> list = pair.getSecond();

                    RepoModel repoModel = pair.getFirst();
                    Optional<PermissionEntity> permission = list.stream().filter(f -> f.id == repoModel.getCustomPermissionNum()).findFirst();

                    if (!permission.isPresent()) {
                        consumer.accept(new RepoPermissionWrapper(pair.getFirst(), null));
                        return;
                    }

                    consumer.accept(new RepoPermissionWrapper(pair.getFirst(), permission.get()));
                }
            }
        });
    }

    /**
     * do not use
     * <li>get the list of libraries first.</li>
     * <li>obtain the corresponding local permission data from the library list</li>
     * <li>if the length of the local permission data list and the library list are the same, the data will be returned directly</li>
     * <li>if not, send multiple requests to the server concurrently to obtain the permission list data</li>
     * <li>merge the permission list data and the library list data to return</li>
     */
    @Unstable
    @Todo
    private Single<List<RepoPermissionWrapper>> getMultipleRepoModelAndRelatePermissionDataSingle(List<String> repoIds, boolean isForce) {
        Single<List<RepoModel>> dbSingle = AppDatabase.getInstance().repoDao().getRepoListByIds(repoIds);
        return dbSingle.flatMap(new Function<List<RepoModel>, SingleSource<List<RepoPermissionWrapper>>>() {
            @Override
            public SingleSource<List<RepoPermissionWrapper>> apply(List<RepoModel> repoModels) throws Exception {
                if (CollectionUtils.isEmpty(repoModels)) {
                    return Single.error(new IllegalArgumentException("No RepoModels found for the given repoIds"));
                }

                if (isForce) {
                    // return empty list
                    List<RepoPermissionWrapper> wrappers = repoModels.stream()
                            .map(model -> new RepoPermissionWrapper(model, null))
                            .collect(Collectors.toList());
                    return Single.just(wrappers);
                }

                return handleRepoPermissions(repoModels);
            }
        }).onErrorResumeNext(throwable -> {
            // 错误处理，记录日志或返回默认值
            SLogs.e("Error in getRepoModelAndAllPermissionSingle2", throwable);
            return Single.error(throwable);
        });
    }

    @Unstable
    @Todo
    private Single<List<RepoPermissionWrapper>> handleRepoPermissions(List<RepoModel> repoModels) {
        List<Integer> customPermissionIds = repoModels.stream()
                .filter(RepoModel::isCustomPermission)
                .map(RepoModel::getCustomPermissionNum)
                .collect(Collectors.toList());

        if (customPermissionIds.isEmpty()) {
            // 如果没有自定义权限，直接返回默认权限
            List<RepoPermissionWrapper> wrappers = repoModels.stream()
                    .map(repo -> new RepoPermissionWrapper(repo,
                            new PermissionEntity(repo.repo_id, repo.permission)))
                    .collect(Collectors.toList());
            return Single.just(wrappers);
        }

        Single<List<PermissionEntity>> pSingle = AppDatabase.getInstance().permissionDAO().getByIdsAsync(customPermissionIds);
        return pSingle.map(permissionEntities -> mapPermissionsToWrappers(repoModels, permissionEntities, customPermissionIds));
    }

    @Unstable
    @Todo
    private List<RepoPermissionWrapper> mapPermissionsToWrappers(List<RepoModel> repoModels,
                                                                 List<PermissionEntity> permissionEntities,
                                                                 List<Integer> customPermissionIds) {
        boolean isValid = customPermissionIds.size() == permissionEntities.size();
        List<RepoPermissionWrapper> wrappers = new ArrayList<>();

        for (RepoModel model : repoModels) {
            if (model.isCustomPermission()) {
                Optional<PermissionEntity> matchedPermission = permissionEntities.stream()
                        .filter(entity -> TextUtils.equals(entity.repo_id, model.repo_id))
                        .findFirst();
                wrappers.add(new RepoPermissionWrapper(model, matchedPermission.orElse(null)));
            } else {
                wrappers.add(new RepoPermissionWrapper(model,
                        new PermissionEntity(model.repo_id, model.permission)));
            }
        }

        // 如果权限不完整，需从远程加载
        if (!isValid) {
            wrappers.forEach(wrapper -> {
                if (wrapper.getPermission() == null) {
                    wrapper.setPermission(null);
                }
            });
        }

        return wrappers;
    }

    /**
     * get the repoModel and repoMode‘s PermissionEntity from local, if not exist, get from remote.
     * if isForce is true, get from remote directly and save to db
     */
    private Single<Pair<RepoModel, List<PermissionEntity>>> getRepoModelAndAllPermissionSingle(String repoId, boolean isForce) {
        Single<List<RepoModel>> dbSingle = AppDatabase.getInstance().repoDao().getRepoById(repoId);
        return dbSingle.flatMap(new Function<List<RepoModel>, SingleSource<Pair<RepoModel, List<PermissionEntity>>>>() {
                    @Override
                    public SingleSource<Pair<RepoModel, List<PermissionEntity>>> apply(List<RepoModel> repoModels) throws Exception {
                        if (CollectionUtils.isEmpty(repoModels)) {
                            return null;
                        }

                        RepoModel repoModel = repoModels.get(0);
                        if (TextUtils.isEmpty(repoModel.permission)) {
                            //This issue doesn't actually happen, but it's still checked again from the remote check
                            return Single.just(new Pair<>(repoModel, null));
                        }

                        if (isForce) {
                            //get permission from remote
                            return Single.just(new Pair<>(repoModel, null));
                        }

                        //get special number permission from db
                        Single<List<PermissionEntity>> pSingle = AppDatabase.getInstance().permissionDAO().getByRepoIdAsync(repoId);

                        return pSingle.flatMap((Function<List<PermissionEntity>, SingleSource<Pair<RepoModel, List<PermissionEntity>>>>) pList -> {

                            //no data in local db
                            if (CollectionUtils.isEmpty(pList)) {
                                return Single.just(new Pair<>(repoModel, null));
                            }

                            //get first permission
                            return Single.just(new Pair<>(repoModel, pList));
                        });
                    }
                })
                //from remote
                .flatMap((Function<Pair<RepoModel, List<PermissionEntity>>, SingleSource<Pair<RepoModel, List<PermissionEntity>>>>) pair -> {
                    if (pair.getSecond() != null) {
                        return Single.just(pair);
                    }

                    Single<List<PermissionEntity>> permissionSingle = getLoadRepoPermissionFromRemoteSingle(repoId);
                    return permissionSingle.flatMap((Function<List<PermissionEntity>, SingleSource<Pair<RepoModel, List<PermissionEntity>>>>) remoteList -> {
                        if (CollectionUtils.isEmpty(remoteList)) {
                            return Single.just(pair);
                        }

                        return Single.just(new Pair<>(pair.getFirst(), remoteList));
                    });
                });
    }

    /**
     * <pre>
     *     <"rw", PermissionEntity(permission,ids)>
     *     <"r", PermissionEntity(permission,ids)>
     *     <"custom-48", PermissionEntity(permission,ids)>
     *     <"custom-49", PermissionEntity(permission,ids)>
     * </pre>
     */
    private final HashMap<String, PermissionEntity> _permissionMap = new HashMap<>();

    public HashMap<String, PermissionEntity> getPermissionStackMap() {
        return _permissionMap;
    }

    public void inflateRepoMenu(Context context) {
        getPermissionStackMap().clear();
        toParseMenu(context, R.menu.bottom_sheet_op_repo, null, CollectionUtils.newArrayList(R.id.unstar));
    }

    /**
     * @param repoModels
     */
    public void inflateRepoMenuWithParams(Context context, List<RepoModel> repoModels, boolean is_checked, List<Integer> disableMenuIds, List<Integer> removedMenuIds, boolean isForce) {
        if (CollectionUtils.isEmpty(repoModels)) {
            return;
        }

        int menuId = R.menu.bottom_sheet_op_repo;
        if (!is_checked) {
            multipleRemoveCachedRepoPermissionMapData(repoModels);

            toParseMenu(context, menuId, disableMenuIds, removedMenuIds);
            return;
        }

        if (repoModels.size() == 1) {
            inflateRepoMenuWithParams(context, repoModels.get(0), disableMenuIds, removedMenuIds, isForce);
        } else {
            List<PermissionEntity> permissionEntities = CollectionUtils.newArrayList();
            for (RepoModel repoModel : repoModels) {
                //NOTICE this is a special permission("r"), not a real permission
                //because: currently, multiple repo lists cannot be deleted at the same time
                //it will be fixed later
                permissionEntities.add(new PermissionEntity(repoModel.repo_id, "r"));
            }

            multipleCacheRepoPermissionMapData(repoModels, permissionEntities);

            toParseMenu(context, menuId, disableMenuIds, removedMenuIds);
        }
    }

    public void inflateRepoMenuWithParams(Context context, RepoModel repoModel, List<Integer> disableMenuIds, List<Integer> removedMenuIds, boolean isForce) {
        int menuId = R.menu.bottom_sheet_op_repo;

        if (!repoModel.is_checked) {
            //remove permission
            removeCachedPermissionMapData(repoModel.permission, repoModel.repo_id);

            toParseMenu(context, menuId, disableMenuIds, removedMenuIds);
            return;
        }

        Single<Pair<RepoModel, List<PermissionEntity>>> r = getRepoModelAndAllPermissionSingle(repoModel.repo_id, isForce);
        addSingleDisposable(r, new Consumer<Pair<RepoModel, List<PermissionEntity>>>() {
            @Override
            public void accept(Pair<RepoModel, List<PermissionEntity>> pair) throws Exception {

                multipleCacheRepoPermissionMapData(CollectionUtils.newArrayList(repoModel), pair.getSecond());

                toParseMenu(context, menuId, disableMenuIds, removedMenuIds);
            }
        });
    }

    public void inflateDirentMenu(Context context) {
        getPermissionStackMap().clear();

        toParseMenu(context, R.menu.bottom_sheet_op_dirent, null, CollectionUtils.newArrayList(R.id.unstar));
    }

    public void inflateDirentMenuWithParams(Context context, List<DirentModel> direntModels, boolean isChecked, List<Integer> disableMenuIds, List<Integer> removedMenuIds, boolean isForce) {
        if (CollectionUtils.isEmpty(direntModels)) {
            return;
        }

        int menuId = R.menu.bottom_sheet_op_dirent;

        if (!isChecked) {
            //remove permission
            multipleRemoveCachedDirentPermissionMapData(direntModels);

            toParseMenu(context, menuId, disableMenuIds, removedMenuIds);
            return;
        }

        String repo_id = direntModels.get(0).repo_id;

        Single<Pair<RepoModel, List<PermissionEntity>>> r = getRepoModelAndAllPermissionSingle(repo_id, isForce);
        addSingleDisposable(r, new Consumer<Pair<RepoModel, List<PermissionEntity>>>() {
            @Override
            public void accept(Pair<RepoModel, List<PermissionEntity>> pair) throws Exception {
                RepoModel repoModel = pair.getFirst();
                List<PermissionEntity> permissionList = pair.getSecond();

                multipleCacheDirentPermissionMapData(direntModels, permissionList);

                toParseMenu(context, menuId, disableMenuIds, removedMenuIds);
            }
        });
    }

    public void clearCachePermissionMap() {
        getPermissionStackMap().clear();
    }

    private void multipleRemoveCachedDirentPermissionMapData(List<DirentModel> direntModels) {
        if (CollectionUtils.isEmpty(direntModels)) {
            return;
        }

        for (DirentModel direntModel : direntModels) {
            removeCachedPermissionMapData(direntModel.permission, direntModel.uid);
        }
    }

    private void multipleRemoveCachedRepoPermissionMapData(List<RepoModel> repoModels) {
        if (CollectionUtils.isEmpty(repoModels)) {
            return;
        }
        for (RepoModel repoModel : repoModels) {
            removeCachedPermissionMapData(repoModel.permission, repoModel.repo_id);
        }
    }

    private void removeCachedPermissionMapData(String permission, String id) {
        if (!getPermissionStackMap().containsKey(permission)) {
            return;
        }

        PermissionEntity entity = getPermissionStackMap().get(permission);
        if (entity == null) {
            return;
        }

        if (!entity.hasId(id)) {
            return;
        }

        entity.removeById(id);

        //
        if (entity.isEmptyIds()) {
            getPermissionStackMap().remove(permission);
        }
    }

    private void multipleCacheDirentPermissionMapData(List<DirentModel> models, List<PermissionEntity> entities) {
        if (CollectionUtils.isEmpty(models)) {
            return;
        }

        for (DirentModel model : models) {
            if (!model.isCustomPermission()) {
                cachePermissionMapData(model.permission, model, new PermissionEntity(model.repo_id, model.permission));
            } else {
                entities.stream().filter(f -> f.id == model.getCustomPermissionNum()).findFirst().ifPresent(entity -> cachePermissionMapData(model.permission, model, entity));
            }
        }
    }

    private void multipleCacheRepoPermissionMapData(List<RepoModel> models, List<PermissionEntity> entities) {
        if (CollectionUtils.isEmpty(models)) {
            return;
        }

        for (RepoModel model : models) {
            if (!model.isCustomPermission()) {
                cachePermissionMapData(model.permission, model, new PermissionEntity(model.repo_id, model.permission));
            } else {
                entities.stream().filter(f -> f.id == model.getCustomPermissionNum()).findFirst().ifPresent(entity -> cachePermissionMapData(model.permission, model, entity));
            }
        }
    }

    private void cachePermissionMapData(String permission, BaseModel baseModel, @NonNull PermissionEntity entity) {
        if (!getPermissionStackMap().containsKey(permission)) {
            entity.cacheBaseModel(baseModel);
        } else {
            PermissionEntity entity1 = getPermissionStackMap().get(permission);
            assert entity1 != null;
            entity1.cacheBaseModel(baseModel);
        }

        getPermissionStackMap().put(permission, entity);
    }

    private void toParseMenu(Context context, int menuId, List<Integer> disableMenuIds, List<Integer> removedMenuIds) {
        List<PermissionEntity> permissionList = new ArrayList<>(getPermissionStackMap().values());
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

    private void loadPermissionFromRemote(String repoId) {
        Single<List<PermissionEntity>> r = getLoadRepoPermissionFromRemoteSingle(repoId);

        addSingleDisposable(r, new Consumer<List<PermissionEntity>>() {
            @Override
            public void accept(List<PermissionEntity> list) throws Exception {
                SLogs.e("permission has been loaded");
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
