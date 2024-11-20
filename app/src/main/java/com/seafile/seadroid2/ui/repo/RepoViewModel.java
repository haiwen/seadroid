package com.seafile.seadroid2.ui.repo;

import android.content.Context;
import android.text.TextUtils;
import android.view.MenuInflater;
import android.view.MenuItem;

import androidx.lifecycle.MutableLiveData;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.NetworkUtils;
import com.blankj.utilcode.util.ToastUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
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
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
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


    public void loadData(NavContext context, boolean forceRefresh) {
        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        if (account == null) {
            return;
        }

        if (!context.inRepo()) {
            loadReposFromLocal(account, forceRefresh);
        } else if (forceRefresh) {
            loadDirentsFromRemote(account, context);
        } else {
            FileViewType fileViewType = Settings.FILE_LIST_VIEW_TYPE.queryValue();
            if (FileViewType.GALLERY == fileViewType) {
                loadDirentsFromLocalWithGalleryViewType(account, context);
            } else {
                loadDirentsFromLocal(account, context);
            }
        }
    }

    private void loadReposFromLocal(Account account, boolean isForce) {
        if (isForce) {
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

                if (isForce) {
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

    private void loadDirentsFromLocalWithGalleryViewType(Account account, NavContext context) {
        getRefreshLiveData().setValue(true);

        String repoId = context.getRepoModel().repo_id;
        String parentDir = context.getNavPath();

        Single<List<DirentModel>> direntDBSingle = AppDatabase.getInstance().direntDao().getListByParentPath(repoId, parentDir);
        addSingleDisposable(direntDBSingle, new Consumer<List<DirentModel>>() {
            @Override
            public void accept(List<DirentModel> direntModels) throws Exception {

                List<DirentModel> rets = CollectionUtils.newArrayList();
                for (DirentModel direntModel : direntModels) {
                    if (Utils.isViewableImage(direntModel.name) || Utils.isVideoFile(direntModel.name)) {
                        rets.add(direntModel);
                    }
                }

                if (CollectionUtils.isEmpty(rets)) {
                    loadDirentsFromRemote(account, context);
                } else {
                    getObjListLiveData().setValue(Objs.parseLocalDirents(rets));
                    getRefreshLiveData().setValue(false);
                }
            }
        });
    }

    private void loadDirentsFromLocal(Account account, NavContext context) {
        getRefreshLiveData().setValue(true);

        String repoId = context.getRepoModel().repo_id;
        String parentDir = context.getNavPath();

        Single<List<DirentModel>> direntDBSingle = AppDatabase.getInstance().direntDao().getListByParentPath(repoId, parentDir);
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
                    loadDirentsFromRemote(account, context);
                } else {
                    getObjListLiveData().setValue(Objs.parseLocalDirents(direntModels));
                    getRefreshLiveData().setValue(false);
                }
            }
        });
    }

    private void loadDirentsFromRemote(Account account, NavContext context) {
        if (!NetworkUtils.isConnected()) {
            getRefreshLiveData().setValue(false);
            return;
        }

        String repoId = context.getRepoModel().repo_id;
        String repoName = context.getRepoModel().repo_name;
        String parentDir = context.getNavPath();

        if ("/".equals(parentDir)) {
            loadPermissionFromRemote(repoId);
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


    public void getRepoModelAndPermissionEntity(String repoId, boolean isForce, Consumer<Pair<RepoModel, PermissionEntity>> consumer) {
        Single<Pair<RepoModel, PermissionEntity>> r = getRepoModelAndPermissionEntitySingle(repoId, isForce);
        addSingleDisposable(r, new Consumer<Pair<RepoModel, PermissionEntity>>() {
            @Override
            public void accept(Pair<RepoModel, PermissionEntity> repoModelPermissionEntityPair) throws Exception {
                if (consumer != null) {
                    consumer.accept(repoModelPermissionEntityPair);
                }
            }
        });
    }

    /**
     * get repoModel and permissionEntity from local, if not exist, get from remote.
     * if isForce is true, get from remote directly and save to db
     */
    private Single<Pair<RepoModel, PermissionEntity>> getRepoModelAndPermissionEntitySingle(String repoId, boolean isForce) {
        Single<List<RepoModel>> dbSingle = AppDatabase.getInstance().repoDao().getRepoById(repoId);
        return dbSingle.flatMap(new Function<List<RepoModel>, SingleSource<Pair<RepoModel, PermissionEntity>>>() {
            @Override
            public SingleSource<Pair<RepoModel, PermissionEntity>> apply(List<RepoModel> repoModels) throws Exception {
                if (CollectionUtils.isEmpty(repoModels)) {
                    return null;
                }

                RepoModel repoModel = repoModels.get(0);
                if (TextUtils.isEmpty(repoModel.permission)) {
                    //This issue doesn't actually happen, but it's still checked again from the remote check
                    return Single.just(new Pair<>(repoModel, null));
                }

                if (!repoModel.isCustomPermission()) {

                    //Optionally
                    PermissionEntity permission = convertCustomPermission(repoModel.repo_id, repoModel.permission);

                    return Single.just(new Pair<>(repoModel, permission));
                }

                if (isForce) {

                    //get permission from remote
                    return Single.just(new Pair<>(repoModel, null));
                }

                int pNum = repoModel.getCustomPermissionNum();

                //get special number permission from db
                Single<List<PermissionEntity>> pSingle = AppDatabase.getInstance().permissionDAO().getByIdAsync(repoId, pNum);

                return pSingle.flatMap(new Function<List<PermissionEntity>, SingleSource<Pair<RepoModel, PermissionEntity>>>() {
                    @Override
                    public SingleSource<Pair<RepoModel, PermissionEntity>> apply(List<PermissionEntity> permissionEntities) throws Exception {

                        //no data in local db
                        if (CollectionUtils.isEmpty(permissionEntities)) {
                            return Single.just(new Pair<>(repoModel, null));
                        }

                        //get first permission
                        return Single.just(new Pair<>(repoModel, permissionEntities.get(0)));
                    }
                });
            }
        }).flatMap(new Function<Pair<RepoModel, PermissionEntity>, SingleSource<Pair<RepoModel, PermissionEntity>>>() {
            @Override
            public SingleSource<Pair<RepoModel, PermissionEntity>> apply(Pair<RepoModel, PermissionEntity> pair) throws Exception {
                if (pair.getSecond() != null) {
                    return Single.just(pair);
                }

                Single<List<PermissionEntity>> permissionSingle = getLoadRepoPermissionFromRemoteSingle(repoId);
                return permissionSingle.flatMap(new Function<List<PermissionEntity>, SingleSource<Pair<RepoModel, PermissionEntity>>>() {
                    @Override
                    public SingleSource<Pair<RepoModel, PermissionEntity>> apply(List<PermissionEntity> remotePermissionEntities) throws Exception {
                        if (CollectionUtils.isEmpty(remotePermissionEntities)) {
                            return Single.just(pair);
                        }

                        Optional<PermissionEntity> f = remotePermissionEntities.stream().filter(p -> p.id == pair.getFirst().getCustomPermissionNum()).findFirst();
                        return Single.just(new Pair<>(pair.getFirst(), f.get()));
                    }
                });
            }
        });
    }


    public HashMap<String, Set<String>> getPermissionObjMap() {
        return _permissionObjMap;
    }

    public HashMap<String, PermissionEntity> getPermissionStackMap() {
        return _permissionMap;
    }

    //
    private final HashMap<String, Set<String>> _permissionObjMap = new HashMap<>();
    private final HashMap<String, PermissionEntity> _permissionMap = new HashMap<>();

    public void inflateRepoMenuWithPermission(Context context, RepoModel repoModel, List<Integer> disableMenuIds, List<Integer> removedMenuIds, boolean isForce) {
        if (!repoModel.is_checked) {
            //remove permission
            removeCachedPermissionMapData(repoModel.permission, repoModel.repo_id);

            List<PermissionEntity> list = new ArrayList<>(getPermissionStackMap().values());

            int menuId = R.menu.bottom_sheet_op_repo;
            List<MenuItem> items = parseMenu(context, menuId, list, disableMenuIds, removedMenuIds);

            getMenuItemListLiveData().setValue(items);

            return;
        }


        Single<Pair<RepoModel, PermissionEntity>> r = getRepoModelAndPermissionEntitySingle(repoModel.repo_id, isForce);

        addSingleDisposable(r, new Consumer<Pair<RepoModel, PermissionEntity>>() {
            @Override
            public void accept(Pair<RepoModel, PermissionEntity> pair) throws Exception {
                if (null == pair) {
                    getSeafExceptionLiveData().setValue(SeafException.unknownException);
                    return;
                }

                RepoModel repoModel = pair.getFirst();
                PermissionEntity entity;
                if (!repoModel.isCustomPermission()) {
                    entity = convertCustomPermission(repoModel.repo_id, repoModel.permission);
                } else {
                    entity = pair.getSecond();
                }

                cachePermissionMapData(repoModel.permission, repoModel.repo_id, entity);


                List<PermissionEntity> list = new ArrayList<>(getPermissionStackMap().values());
                int menuId = R.menu.bottom_sheet_op_repo;
                List<MenuItem> items = parseMenu(context, menuId, list, disableMenuIds, removedMenuIds);

                getMenuItemListLiveData().setValue(items);
            }
        });
    }


    public void inflateDirentMenuWithPermission(Context context, DirentModel direntModel, List<Integer> disableMenuIds, List<Integer> removedMenuIds, boolean isForce) {
        int menuId = R.menu.bottom_sheet_op_dirent;


        if (!direntModel.is_checked) {
            //remove permission
            removeCachedPermissionMapData(direntModel.permission, direntModel.uid);

            List<PermissionEntity> tempList1 = new ArrayList<>(getPermissionStackMap().values());
            List<MenuItem> items = parseMenu(context, menuId, tempList1, disableMenuIds, removedMenuIds);
            getMenuItemListLiveData().setValue(items);
            return;
        }

        if (!direntModel.isCustomPermission()) {
            PermissionEntity entity = convertCustomPermission(direntModel.repo_id, direntModel.permission);
            cachePermissionMapData(direntModel.permission, direntModel.uid, entity);

            List<PermissionEntity> tempList = new ArrayList<>(getPermissionStackMap().values());
            List<MenuItem> items = parseMenu(context, menuId, tempList, disableMenuIds, removedMenuIds);
            getMenuItemListLiveData().setValue(items);
            return;
        }


        //
        Single<Pair<RepoModel, PermissionEntity>> r = getRepoModelAndPermissionEntitySingle(direntModel.repo_id, isForce);
        addSingleDisposable(r, new Consumer<Pair<RepoModel, PermissionEntity>>() {
            @Override
            public void accept(Pair<RepoModel, PermissionEntity> pair) throws Exception {
                RepoModel repoModel = pair.getFirst();
                PermissionEntity entity;
                if (!repoModel.isCustomPermission()) {
                    entity = convertCustomPermission(repoModel.repo_id, repoModel.permission);
                } else {
                    entity = pair.getSecond();
                }

                cachePermissionMapData(direntModel.permission, direntModel.uid, entity);

                List<PermissionEntity> list = new ArrayList<>(getPermissionStackMap().values());
                List<MenuItem> items = parseMenu(context, menuId, list, disableMenuIds, removedMenuIds);
                getMenuItemListLiveData().setValue(items);
            }
        });
    }

    public void clearCachePermissionMap() {
        getPermissionObjMap().clear();
        getPermissionStackMap().clear();
    }

    private void removeCachedPermissionMapData(String permission, String id) {
        if (!getPermissionObjMap().containsKey(permission)) {
            return;
        }

        Set<String> set = getPermissionObjMap().get(permission);
        if (set == null) {
            return;
        }

        if (!set.contains(id)) {
            return;
        }

        set.remove(id);

        //
        if (set.isEmpty()) {
            getPermissionStackMap().remove(permission);
        }
    }

    private void cachePermissionMapData(String permission, String id, PermissionEntity entity) {
        if (!getPermissionStackMap().containsKey(permission)) {
            getPermissionStackMap().put(permission, entity);
        }

        if (getPermissionObjMap().containsKey(permission)) {
            Objects.requireNonNull(getPermissionObjMap().get(permission)).add(id);
        } else {
            getPermissionObjMap().put(permission, CollectionUtils.newHashSet(id));
        }
    }

    private PermissionEntity convertCustomPermission(String repoId, String p) {

        PermissionEntity permission = new PermissionEntity();
        permission.name = p;
        permission.id = -1;
        permission.repo_id = repoId;

        if ("cloud-edit".equals(p)) {
            //用户可以通过浏览器在线查看和编辑，文件不能被下载。
            permission.create = true;
            permission.upload = false;
            permission.download = false;
            permission.preview = true;
            permission.copy = true;
            permission.delete = true;
            permission.modify = true;
            permission.download_external_link = false;

        } else if ("preview".equals(p)) {
            //用户只能通过浏览器在线查看，文件不能被下载。
            permission.create = false;
            permission.upload = false;
            permission.download = false;
            permission.preview = true;
            permission.copy = false;
            permission.delete = false;
            permission.modify = false;
            permission.download_external_link = false;
        } else if ("r".equals(p)) {
            //用户可以查看、下载和同步文件
            permission.create = false;
            permission.upload = false;
            permission.download = true;
            permission.preview = true;
            permission.copy = true;
            permission.delete = false;
            permission.modify = false;
            permission.download_external_link = false;
        } else if ("rw".equals(p)) {
            permission.create = true;
            permission.upload = true;
            permission.download = true;
            permission.preview = true;
            permission.copy = true;
            permission.delete = true;
            permission.modify = true;
            permission.download_external_link = true;
        }

        return permission;
    }

    private List<MenuItem> parseMenu(Context context, int menuId, List<PermissionEntity> list, List<Integer> disableMenuIds, List<Integer> removedMenuIds) {
        List<MenuItem> items = inflateMenu(context, menuId);

        //if no permission list, disable all menu
        if (CollectionUtils.isEmpty(list)) {
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
                long l = list.stream().filter(f -> !f.modify).count();
                item.setEnabled(!(l > 0));
            } else if (item.getItemId() == R.id.move) {
                long l = list.stream().filter(f -> !f.modify).count();
                item.setEnabled(!(l > 0));
            } else if (item.getItemId() == R.id.copy) {
                long l = list.stream().filter(f -> !f.copy).count();
                item.setEnabled(!(l > 0));
            } else if (item.getItemId() == R.id.delete) {
                long l = list.stream().filter(f -> !f.delete).count();
                item.setEnabled(!(l > 0));
            } else if (item.getItemId() == R.id.upload) {
                long l = list.stream().filter(f -> !f.upload).count();
                item.setEnabled(!(l > 0));
            } else if (item.getItemId() == R.id.download) {
                long l = list.stream().filter(f -> !f.download).count();
                item.setEnabled(!(l > 0));
            } else if (item.getItemId() == R.id.share) {
                long l = list.stream().filter(f -> !f.download_external_link).count();
                item.setEnabled(!(l > 0));
            } else if (item.getItemId() == R.id.export) {
                long l = list.stream().filter(f -> !f.download).count();
                item.setEnabled(!(l > 0));
            } else if (item.getItemId() == R.id.open) {
                long l = list.stream().filter(f -> !f.download).count();
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
                    PermissionEntity permission = new PermissionEntity();
                    permission.id = model.id;
                    permission.name = model.name;
                    permission.description = model.description;

                    permission.create = model.permission.create;
                    permission.upload = model.permission.upload;
                    permission.download = model.permission.download;
                    permission.copy = model.permission.copy;
                    permission.delete = model.permission.delete;
                    permission.modify = model.permission.modify;
                    permission.download_external_link = model.permission.download_external_link;
                    permission.preview = model.permission.preview;
                    permission.repo_id = repoId;

                    list.add(permission);
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

            Map<String, RequestBody> bodyMap = generateRequestBody(requestDataMap);
            Single<Dirent2Model> single = HttpIO.getCurrentInstance().execute(StarredService.class).star(bodyMap);

            return single.toFlowable();
        } else {
            Single<ResultModel> single1 = HttpIO.getCurrentInstance().execute(StarredService.class).unStar(repoId, path);
            return single1.toFlowable();
        }
    }
}
