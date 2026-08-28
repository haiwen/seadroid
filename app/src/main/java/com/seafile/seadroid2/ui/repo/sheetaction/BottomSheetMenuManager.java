package com.seafile.seadroid2.ui.repo.sheetaction;

import android.app.Activity;
import android.content.Context;
import android.text.TextUtils;
import android.view.MenuInflater;
import android.view.MenuItem;

import com.blankj.utilcode.util.CollectionUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.config.ObjKey;
import com.seafile.seadroid2.config.RepoType;
import com.seafile.seadroid2.context.GlobalNavContext;
import com.seafile.seadroid2.enums.ObjSelectType;
import com.seafile.seadroid2.framework.db.AppDatabase;
import com.seafile.seadroid2.framework.db.entities.DirentModel;
import com.seafile.seadroid2.framework.db.entities.PermissionEntity;
import com.seafile.seadroid2.framework.db.entities.RepoModel;
import com.seafile.seadroid2.framework.model.BaseModel;
import com.seafile.seadroid2.framework.model.search.SearchModel;
import com.seafile.seadroid2.ui.bottomsheetmenu.ActionMenu;

import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import io.reactivex.Single;
import io.reactivex.SingleEmitter;
import io.reactivex.SingleOnSubscribe;
import io.reactivex.SingleSource;
import io.reactivex.android.schedulers.AndroidSchedulers;
import io.reactivex.disposables.CompositeDisposable;
import io.reactivex.functions.Consumer;
import io.reactivex.functions.Function;
import io.reactivex.schedulers.Schedulers;

public class BottomSheetMenuManager {
    private final Activity context;
    private final BottomSheetActionViewPager bottomSheetView;

    public BottomSheetMenuManager(Activity context, BottomSheetActionView.OnBottomSheetItemClickListener listener) {
        this.context = context;
        bottomSheetView = new BottomSheetActionViewPager(context);
        bottomSheetView.setOnItemClickListener(listener);
    }

    public void showMenu(String relatedAccount, String objKey, List<BaseModel> selectedItems) {
        List<Integer> disableMenuIds = getDisableMenuIds(objKey, selectedItems);
        List<Integer> removedMenuIds = getWillBeRemovedMenuIds(objKey, selectedItems);

        if (StringUtils.equals(ObjKey.SEARCH, objKey)) {
            inflateSearchMenuWithSelected(context, relatedAccount, selectedItems, disableMenuIds, removedMenuIds);
        } else if (StringUtils.equals(ObjKey.REPO, objKey)) {
            inflateRepoMenuWithSelected(context, relatedAccount, selectedItems, disableMenuIds, removedMenuIds);
        } else if (StringUtils.equals(ObjKey.DIRENT, objKey)) {
            inflateDirentMenuWithSelected(context, relatedAccount, selectedItems, disableMenuIds, removedMenuIds);
        }
    }

    public void dismiss() {
        compositeDisposable.clear();
        bottomSheetView.dismiss();
    }

    private final CompositeDisposable compositeDisposable = new CompositeDisposable();

    public <T> void addSingleDisposable(Single<T> single, Consumer<T> consumer) {
        compositeDisposable.add(single
                .subscribeOn(Schedulers.io())
                .observeOn(AndroidSchedulers.mainThread())
                .subscribe(consumer));
    }

    public void inflateRepoMenuWithSelected(Context context, String relatedAccount, List<BaseModel> selectedItems, List<Integer> disableMenuIds, List<Integer> removedMenuIds) {
        int menuId = R.menu.bottom_sheet_op_repo;

        if (CollectionUtils.isEmpty(selectedItems)) {
            toParseMenu(context, menuId, null, disableMenuIds, removedMenuIds);
            return;
        }

        List<RepoModel> models = selectedItems.stream()
                .map(b -> (RepoModel) b)
                .collect(Collectors.toList());

        List<PermissionEntity> permissionList = CollectionUtils.newArrayList();

        if (CollectionUtils.isEmpty(models)) {
            toParseMenu(context, menuId, permissionList, disableMenuIds, removedMenuIds);
        } else if (models.size() == 1) {
            RepoModel repoModel = models.get(0);
            Single<PermissionEntity> permissionSingle = getRepoPermission(relatedAccount, repoModel.repo_id);
            addSingleDisposable(permissionSingle, new Consumer<PermissionEntity>() {
                @Override
                public void accept(PermissionEntity permissionEntity) {
                    if (!repoModel.hasManageRepoPermission()) {
                        // only-read permission
                        permissionList.add(new PermissionEntity(repoModel.repo_id, "r"));
                    } else if (repoModel.isCustomPermission()) {
                        permissionList.add(permissionEntity);
                    } else {
                        permissionList.add(new PermissionEntity(repoModel.repo_id, repoModel.permission));
                    }

                    toParseMenu(context, menuId, permissionList, disableMenuIds, removedMenuIds);

                }
            });
        } else {
            //
            for (RepoModel repoModel : models) {
                //NOTICE this is a special permission("r"), not a real permission
                //because: currently, multiple repo lists cannot be deleted at the same time
                permissionList.add(new PermissionEntity(repoModel.repo_id, "r"));
            }
            toParseMenu(context, menuId, permissionList, disableMenuIds, removedMenuIds);

        }


    }

    public void inflateSearchMenuWithSelected(Context context, String relatedAccount, List<BaseModel> selectedItems, List<Integer> disableMenuIds, List<Integer> removedMenuIds) {
        int menuId = R.menu.bottom_sheet_op_dirent;

        if (CollectionUtils.isEmpty(selectedItems)) {
            toParseMenu(context, menuId, null, disableMenuIds, removedMenuIds);
            return;
        }

        List<SearchModel> models = selectedItems.stream()
                .map(b -> (SearchModel) b)
                .collect(Collectors.toList());

        SearchModel searchModel = models.get(0);

        Single<PermissionEntity> permissionSingle = getRepoPermission(relatedAccount, searchModel.repo_id);

        addSingleDisposable(permissionSingle, new Consumer<PermissionEntity>() {
            @Override
            public void accept(PermissionEntity permissionEntity) throws Exception {

                List<PermissionEntity> permissionList = new ArrayList<>();
                if (permissionEntity.isValid()) {
                    permissionList.add(permissionEntity);
                }

                toParseMenu(context, menuId, permissionList, disableMenuIds, removedMenuIds);
            }
        });
    }

    private Single<PermissionEntity> getRepoPermission(String relatedAccount, String repoId) {
        Single<List<RepoModel>> rSingle = AppDatabase.getInstance().repoDao().getRepoById(relatedAccount, repoId);
        return rSingle.flatMap(new Function<List<RepoModel>, SingleSource<PermissionEntity>>() {
            @Override
            public SingleSource<PermissionEntity> apply(List<RepoModel> repoModels) throws Exception {
                if (CollectionUtils.isEmpty(repoModels)) {
                    return Single.just(new PermissionEntity());
                }

                RepoModel repoModel = repoModels.get(0);
                if (repoModel.isCustomPermission()) {
                    return AppDatabase.getInstance().permissionDAO().getByRepoAndIdAsync(repoModel.repo_id, repoModel.getCustomPermissionNum())
                            .flatMap(new Function<List<PermissionEntity>, SingleSource<PermissionEntity>>() {
                                @Override
                                public SingleSource<PermissionEntity> apply(List<PermissionEntity> permissionEntities) throws Exception {
                                    PermissionEntity repoPerm;
                                    if (CollectionUtils.isEmpty(permissionEntities)) {
                                        repoPerm = permissionEntities.get(0);
                                    } else {
                                        repoPerm = new PermissionEntity(repoModel.repo_id, "r");
                                    }
                                    return Single.just(repoPerm);
                                }
                            });
                }
                return Single.just(new PermissionEntity(repoModel.repo_id, repoModel.permission));
            }
        });
    }

    public void inflateDirentMenuWithSelected(Context context, String relatedAccount, List<BaseModel> selectedItems, List<Integer> disableMenuIds, List<Integer> removedMenuIds) {
        int menuId = R.menu.bottom_sheet_op_dirent;

        if (CollectionUtils.isEmpty(selectedItems)) {
            toParseMenu(context, menuId, null, disableMenuIds, removedMenuIds);
            return;
        }

        List<DirentModel> models = selectedItems.stream()
                .map(b -> (DirentModel) b)
                .collect(Collectors.toList());

        DirentModel direntModel = models.get(0);
        Single<PermissionEntity> permissionSingle = getRepoPermission(relatedAccount, direntModel.repo_id);
        addSingleDisposable(permissionSingle, new Consumer<PermissionEntity>() {
            @Override
            public void accept(PermissionEntity permissionEntity) throws Exception {
                List<PermissionEntity> permissionList = CollectionUtils.newArrayList();
                if (models.size() == 1) {
                    if (direntModel.isCustomPermission()) {
                        if (direntModel.getCustomPermissionNum() == permissionEntity.id) {
                            permissionList.add(permissionEntity);
                        } else {

                        }
                    } else if (direntModel.permission.equals(permissionEntity.name)) {
                        permissionList.add(permissionEntity);
                    } else {
                        //dirent's permissions can only be one of these 5 permission: "rw"/"r"/"cloud-edit"/"cloud-preview"/"manage"
                        permissionList.add(new PermissionEntity(direntModel.repo_id, direntModel.permission));
                    }

                    toParseMenu(context, menuId, permissionList, disableMenuIds, removedMenuIds);
                } else {
                    for (DirentModel direntModel : models) {
                        if (direntModel.isCustomPermission()) {
                            //if selected size > 0, and direntModel is custom permission, set permission to "r":read-only
                            permissionList.add(new PermissionEntity(direntModel.repo_id, "r"));
                        } else {
                            permissionList.add(new PermissionEntity(direntModel.repo_id, direntModel.permission));
                        }
                    }
                    toParseMenu(context, menuId, permissionList, disableMenuIds, removedMenuIds);
                }
            }
        });
    }

    private void toParseMenu(Context context, int menuId, List<PermissionEntity> permissionList, List<Integer> disableMenuIds, List<Integer> removedMenuIds) {
        List<MenuItem> items = parseMenu(context, menuId, permissionList, disableMenuIds, removedMenuIds);

        bottomSheetView.show(items);
    }

    private List<MenuItem> parseMenu(Context context, int menuId, List<PermissionEntity> permissionList, List<Integer> disableMenuIds, List<Integer> removedMenuIds) {
        List<MenuItem> items = inflateMenu(context, menuId);

        //if no permission list, set to disable
        if (CollectionUtils.isEmpty(permissionList)) {
            //
            if (!CollectionUtils.isEmpty(removedMenuIds)) {
                items = items.stream().filter(item -> !removedMenuIds.contains(item.getItemId())).collect(Collectors.toList());
            }

            for (MenuItem item : items) {
                item.setEnabled(false);
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
            } else if (item.getItemId() == R.id.open_with) {
                long l = permissionList.stream().filter(f -> !f.download).count();
                item.setEnabled(!(l > 0));
            } else if (item.getItemId() == R.id.save_as) {
                long l = permissionList.stream().filter(f -> !f.download).count();
                item.setEnabled(!(l > 0));
            }
//            else if (item.getItemId() == R.id.profile) {
//                long l = permissionList.stream().filter(f -> !f.).count();
//                item.setEnabled(!(l > 0));
//            }

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


    public List<Integer> getDisableMenuIds(String objKey, List<BaseModel> selectedList) {
        if (selectedList == null || selectedList.isEmpty()) {
            return null;
        }
        if (StringUtils.isEmpty(objKey)) {
            return null;
        }

        // search
        if (StringUtils.equals(ObjKey.SEARCH, objKey)) {
            // Batch operations are not supported.
            if (selectedList.size() == 1) {
                SearchModel sm = (SearchModel) selectedList.get(0);
                if (sm.isDir()) {
                    return CollectionUtils.newArrayList(
                            R.id.star, R.id.share, R.id.rename, R.id.delete,
                            R.id.upload, R.id.export, R.id.open_with,
                            R.id.download, R.id.save_as, R.id.profile);
                } else {
                    // Only supported: Export, Copy, Move, Download, Open With, Save As
                    return CollectionUtils.newArrayList(
                            R.id.star, R.id.share, R.id.rename,
                            R.id.delete, R.id.upload, R.id.profile);
                }
            } else {
                return CollectionUtils.newArrayList(
                        R.id.star, R.id.share, R.id.export, R.id.rename,
                        R.id.delete, R.id.copy, R.id.move, R.id.upload,
                        R.id.download, R.id.open_with, R.id.save_as, R.id.profile);
            }
        } else if (StringUtils.equals(ObjKey.REPO, objKey)) {
            // repo
            if (selectedList.size() == 1) {
                RepoModel m = (RepoModel) selectedList.get(0);
                if (m.encrypted) {
                    return CollectionUtils.newArrayList(R.id.share);
                }
            } else {
                return CollectionUtils.newArrayList(R.id.rename, R.id.delete, R.id.share);
            }
        } else if (StringUtils.equals(ObjKey.DIRENT, objKey)) {
            // dirent
            if (selectedList.size() == 1) {
                DirentModel m = (DirentModel) selectedList.get(0);
                if (m.isDir()) {
                    return CollectionUtils.newArrayList(R.id.export, R.id.open_with, R.id.upload, R.id.save_as, R.id.profile);
                }
                // all supported
                return null;
            } else {
                long selectedFolderCount = selectedList.stream()
                        .filter(f -> f instanceof DirentModel)
                        .map(m -> (DirentModel) m)
                        .filter(DirentModel::isDir)
                        .count();

                // multi folder
                if (selectedFolderCount > 0) {
                    return CollectionUtils.newArrayList(
                            R.id.share, R.id.export, R.id.open_with,
                            R.id.rename, R.id.upload, R.id.save_as,
                            R.id.profile);
                }

                return CollectionUtils.newArrayList(
                        R.id.share, R.id.export, R.id.open_with,
                        R.id.rename, R.id.save_as, R.id.profile);
            }
        }

        return null;
    }

    /**
     * Remove inappropriate menus
     */
    public List<Integer> getWillBeRemovedMenuIds(String objKey, List<BaseModel> selectedList) {
        ArrayList<Integer> retList = CollectionUtils.newArrayList();

        // default
        if (CollectionUtils.isEmpty(selectedList)) {
            retList.add(R.id.unstar);
            retList.add(R.id.leave_share);
            return retList;
        }

        if (StringUtils.equals(ObjKey.SEARCH, objKey)) {
            retList.add(R.id.unstar);
        } else if (StringUtils.equals(ObjKey.REPO, objKey)) {
            if (selectedList.size() == 1) {
                RepoModel m = (RepoModel) selectedList.get(0);
                if (StringUtils.equals(RepoType.TYPE_SHARED, m.type)) {
                    retList.add(R.id.share);
                } else {
                    retList.add(R.id.leave_share);
                }

                if (m.starred) {
                    retList.add(R.id.star);
                } else {
                    retList.add(R.id.unstar);
                }
            } else {
                retList.add(R.id.unstar);
                retList.add(R.id.leave_share);
            }

        } else if (StringUtils.equals(ObjKey.DIRENT, objKey)) {
            if (selectedList.size() == 1) {
                DirentModel m = (DirentModel) selectedList.get(0);
                if (m.starred) {
                    retList.add(R.id.star);
                } else {
                    retList.add(R.id.unstar);
                }
            } else {
                boolean isAllStarred = true;
                for (BaseModel baseModel : selectedList) {
                    if (baseModel instanceof RepoModel m) {
                        if (m.starred) {
                            continue;
                        }
                        isAllStarred = false;
                        break;
                    } else if (baseModel instanceof DirentModel m) {
                        if (m.starred) {
                            continue;
                        }
                        isAllStarred = false;
                        break;
                    }
                }
                if (isAllStarred) {
                    retList.add(R.id.star);
                } else {
                    retList.add(R.id.unstar);
                }
            }
        }
        return retList;
    }
}
