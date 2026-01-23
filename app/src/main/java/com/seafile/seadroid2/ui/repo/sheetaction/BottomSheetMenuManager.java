package com.seafile.seadroid2.ui.repo.sheetaction;

import android.app.Activity;
import android.content.Context;
import android.view.MenuInflater;
import android.view.MenuItem;

import com.blankj.utilcode.util.CollectionUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.context.GlobalNavContext;
import com.seafile.seadroid2.framework.db.AppDatabase;
import com.seafile.seadroid2.framework.db.entities.DirentModel;
import com.seafile.seadroid2.framework.db.entities.PermissionEntity;
import com.seafile.seadroid2.framework.db.entities.RepoModel;
import com.seafile.seadroid2.framework.model.BaseModel;
import com.seafile.seadroid2.framework.model.search.SearchModel;
import com.seafile.seadroid2.ui.bottomsheetmenu.ActionMenu;

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

    public void showMenu(List<BaseModel> selectedItems) {
        List<Integer> disableMenuIds = getDisableMenuIds(selectedItems);
        List<Integer> removedMenuIds = getWillBeRemovedMenuIds(selectedItems);
        boolean isInRepo = GlobalNavContext.getCurrentNavContext().inRepo();

        if (CollectionUtils.isEmpty(selectedItems)) {
            if (isInRepo) {
                justInflateDirentMenu(context);
            } else {
                justInflateRepoMenu(context);
            }
            return;
        }

        BaseModel baseModel = selectedItems.get(0);
        if (baseModel instanceof RepoModel) {
            List<RepoModel> models = selectedItems.stream()
                    .map(b -> (RepoModel) b)
                    .collect(Collectors.toList());
            inflateRepoMenuWithSelected(context, models, disableMenuIds, removedMenuIds);
        } else if (baseModel instanceof DirentModel) {
            List<DirentModel> models = selectedItems.stream()
                    .map(b -> (DirentModel) b)
                    .collect(Collectors.toList());
            inflateDirentMenuWithSelected(context, models, disableMenuIds, removedMenuIds);
        } else if (baseModel instanceof SearchModel) {
            List<SearchModel> models = selectedItems.stream()
                    .map(b -> (SearchModel) b)
                    .collect(Collectors.toList());
            inflateSearchMenuWithSelected(context, models, disableMenuIds, removedMenuIds);
        }
    }

    public void dismiss() {
        compositeDisposable.clear();
        bottomSheetView.dismiss();
    }

    public void justInflateRepoMenu(Context context) {
        toParseMenu(context, R.menu.bottom_sheet_op_repo, null, CollectionUtils.newArrayList(R.id.unstar));
    }

    public void justInflateDirentMenu(Context context) {
        toParseMenu(context, R.menu.bottom_sheet_op_dirent, null, null, CollectionUtils.newArrayList(R.id.unstar));
    }


    private final CompositeDisposable compositeDisposable = new CompositeDisposable();

    public <T> void addSingleDisposable(Single<T> single, Consumer<T> consumer) {
        compositeDisposable.add(single
                .subscribeOn(Schedulers.io())
                .observeOn(AndroidSchedulers.mainThread())
                .subscribe(consumer));
    }

    /**
     * @param selectedRepoModels
     */
    public void inflateRepoMenuWithSelected(Context context, List<RepoModel> selectedRepoModels, List<Integer> disableMenuIds, List<Integer> removedMenuIds) {
        if (CollectionUtils.isEmpty(selectedRepoModels)) {
            justInflateRepoMenu(context);
            return;
        }

        int menuId = R.menu.bottom_sheet_op_repo;
        if (selectedRepoModels.size() == 1) {
            RepoModel repoModel = selectedRepoModels.get(0);
            if (repoModel.isCustomPermission()) {
                Single<List<PermissionEntity>> permissionSingle = AppDatabase.getInstance().permissionDAO().getByRepoAndIdAsync(repoModel.repo_id, repoModel.getCustomPermissionNum());
                compositeDisposable.add(permissionSingle
                        .subscribeOn(Schedulers.io())
                        .observeOn(AndroidSchedulers.mainThread())
                        .subscribe(new Consumer<List<PermissionEntity>>() {
                            @Override
                            public void accept(List<PermissionEntity> permissionEntities) throws Exception {
                                if (CollectionUtils.isEmpty(permissionEntities) || !permissionEntities.get(0).isValid()) {
                                    toParseMenu(context, menuId, null, disableMenuIds, removedMenuIds);
                                } else {
                                    toParseMenu(context, menuId, permissionEntities, disableMenuIds, removedMenuIds);
                                }
                            }
                        }));
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

    public void inflateSearchMenuWithSelected(Context context, List<SearchModel> selectedDirentList, List<Integer> disableMenuIds, List<Integer> removedMenuIds) {
        if (CollectionUtils.isEmpty(selectedDirentList)) {
            justInflateDirentMenu(context);
            return;
        }

        SearchModel searchModel = selectedDirentList.get(0);

        Single<List<RepoModel>> rSingle = AppDatabase.getInstance().repoDao().getRepoById(searchModel.repo_id);
        Single<PermissionEntity> permissionSingle = rSingle.flatMap(new Function<List<RepoModel>, SingleSource<PermissionEntity>>() {
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


        addSingleDisposable(permissionSingle, new Consumer<PermissionEntity>() {
            @Override
            public void accept(PermissionEntity permissionEntity) throws Exception {
                int menuId = R.menu.bottom_sheet_op_dirent;

                List<PermissionEntity> permissionList = new ArrayList<>();
                if (permissionEntity.isValid()) {
                    permissionList.add(permissionEntity);
                }

                toParseMenu(context, menuId, permissionList, disableMenuIds, removedMenuIds);
            }
        });
    }


    public void inflateDirentMenuWithSelected(Context context, List<DirentModel> selectedDirentList, List<Integer> disableMenuIds, List<Integer> removedMenuIds) {
        if (CollectionUtils.isEmpty(selectedDirentList)) {
            justInflateDirentMenu(context);
            return;
        }

        RepoModel repoModel = GlobalNavContext.getCurrentNavContext().getRepoModel();
        Single<PermissionEntity> repoPermSingle;
        if (repoModel.isCustomPermission()) {
            Single<List<PermissionEntity>> permissionSingle = AppDatabase.getInstance().permissionDAO().getByRepoAndIdAsync(repoModel.repo_id, repoModel.getCustomPermissionNum());
            repoPermSingle = permissionSingle.flatMap(new Function<List<PermissionEntity>, SingleSource<PermissionEntity>>() {
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

        } else {
            repoPermSingle = Single.just(new PermissionEntity(repoModel.repo_id, repoModel.permission));
        }

        addSingleDisposable(repoPermSingle, new Consumer<PermissionEntity>() {
            @Override
            public void accept(PermissionEntity repoPerm) throws Exception {
                int menuId = R.menu.bottom_sheet_op_dirent;

                if (selectedDirentList.size() == 1) {
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
                    List<PermissionEntity> permissionList = new ArrayList<>();
                    for (DirentModel direntModel : selectedDirentList) {
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

    private void toParseMenu(Context context, int menuId, List<Integer> disableMenuIds, List<Integer> removedMenuIds) {
        List<MenuItem> items = parseMenu(context, menuId, null, disableMenuIds, removedMenuIds);
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


    public List<Integer> getDisableMenuIds(List<BaseModel> selectedList) {
        if (selectedList == null || selectedList.isEmpty()) {
            return null;
        }

        boolean isExistsSearchModel = selectedList.stream().anyMatch(m -> m instanceof SearchModel);
        if (isExistsSearchModel) {
            // Batch operations are not supported.
            if (selectedList.size() > 1) {
                return CollectionUtils.newArrayList(
                        R.id.star, R.id.share, R.id.export, R.id.rename,
                        R.id.delete, R.id.copy, R.id.move, R.id.upload,
                        R.id.download, R.id.open_with, R.id.save_as);
            } else {
                SearchModel sm = (SearchModel) selectedList.get(0);
                if (sm.isDir()) {
                    return CollectionUtils.newArrayList(
                            R.id.star, R.id.share, R.id.rename, R.id.delete,
                            R.id.upload, R.id.export, R.id.open_with,
                            R.id.download, R.id.save_as);
                } else {
                    //Only supported: Export, Copy, Move, Download, Open With, Save As
                    return CollectionUtils.newArrayList(
                            R.id.star, R.id.share, R.id.rename,
                            R.id.delete, R.id.upload);
                }
            }
        }

        if (selectedList.size() == 1) {
            BaseModel baseModel = selectedList.get(0);
            if (baseModel instanceof RepoModel m) {

            } else if (baseModel instanceof DirentModel m) {
                if (m.isDir()) {
                    return CollectionUtils.newArrayList(R.id.export, R.id.open_with, R.id.upload, R.id.save_as);
                }
            }

            return null;
        }

        long selectedRepoModelCount = selectedList.stream()
                .filter(f -> f instanceof RepoModel)
                .count();
        if (selectedRepoModelCount > 0) {
            return CollectionUtils.newArrayList(R.id.delete);
        }

        long selectedFolderCount = selectedList.stream()
                .filter(f -> f instanceof DirentModel)
                .map(m -> (DirentModel) m)
                .filter(DirentModel::isDir)
                .count();

        if (selectedFolderCount > 0) {
            return CollectionUtils.newArrayList(R.id.share, R.id.export, R.id.open_with, R.id.rename, R.id.upload, R.id.save_as);
        }

        long selectedDirentModelCount = selectedList.stream()
                .filter(f -> f instanceof DirentModel)
                .count();
        if (selectedDirentModelCount > 0) {
            return CollectionUtils.newArrayList(R.id.share, R.id.export, R.id.open_with, R.id.rename, R.id.save_as);
        }

        return CollectionUtils.newArrayList(R.id.share, R.id.export, R.id.open_with, R.id.rename);
    }

    /**
     *
     */
    public List<Integer> getWillBeRemovedMenuIds(List<BaseModel> selectedList) {
        if (CollectionUtils.isEmpty(selectedList)) {
            return CollectionUtils.newArrayList(R.id.unstar);
        }

        boolean isExistsSearchModel = selectedList.stream().anyMatch(m -> m instanceof SearchModel);
        if (isExistsSearchModel) {
            return CollectionUtils.newArrayList(R.id.unstar);
        }

        if (selectedList.size() == 1) {

            BaseModel baseModel = selectedList.get(0);
            if (baseModel instanceof RepoModel m) {
                return CollectionUtils.newArrayList(m.starred ? R.id.star : R.id.unstar);
            } else if (baseModel instanceof DirentModel m) {
                return CollectionUtils.newArrayList(m.starred ? R.id.star : R.id.unstar);
            }

            //remove all starred menu
            return CollectionUtils.newArrayList(R.id.star, R.id.unstar, R.id.upload, R.id.download);
        }

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
            return CollectionUtils.newArrayList(R.id.star);
        } else {
            return CollectionUtils.newArrayList(R.id.unstar);
        }
    }
}
