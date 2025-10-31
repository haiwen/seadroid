package com.seafile.seadroid2.framework.util;

import android.content.Context;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.content.pm.ResolveInfo;
import android.net.Uri;
import android.text.TextUtils;

import androidx.core.content.ContextCompat;
import androidx.core.content.FileProvider;
import androidx.fragment.app.Fragment;
import androidx.fragment.app.FragmentManager;

import com.blankj.utilcode.util.ClipboardUtils;
import com.blankj.utilcode.util.CloneUtils;
import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.NetworkUtils;
import com.google.common.collect.Lists;
import com.seafile.seadroid2.BuildConfig;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.config.RepoType;
import com.seafile.seadroid2.enums.ItemPositionEnum;
import com.seafile.seadroid2.enums.SortBy;
import com.seafile.seadroid2.framework.db.AppDatabase;
import com.seafile.seadroid2.framework.db.entities.DirentModel;
import com.seafile.seadroid2.framework.db.entities.PermissionEntity;
import com.seafile.seadroid2.framework.db.entities.RepoModel;
import com.seafile.seadroid2.framework.db.entities.StarredModel;
import com.seafile.seadroid2.framework.http.HttpIO;
import com.seafile.seadroid2.framework.model.BaseModel;
import com.seafile.seadroid2.framework.model.GroupItemModel;
import com.seafile.seadroid2.framework.model.dirents.CachedDirentModel;
import com.seafile.seadroid2.framework.model.objs.DirentShareLinkModel;
import com.seafile.seadroid2.framework.model.permission.PermissionWrapperModel;
import com.seafile.seadroid2.framework.model.repo.DirentWrapperModel;
import com.seafile.seadroid2.framework.model.repo.RepoWrapperModel;
import com.seafile.seadroid2.framework.model.star.StarredWrapperModel;
import com.seafile.seadroid2.listener.OnCreateDirentShareLinkListener;
import com.seafile.seadroid2.preferences.Settings;
import com.seafile.seadroid2.ui.WidgetUtils;
import com.seafile.seadroid2.ui.comparator.DirentNaturalOrderComparator;
import com.seafile.seadroid2.ui.comparator.RepoNaturalOrderComparator;
import com.seafile.seadroid2.ui.dialog_fragment.AppChoiceDialogFragment;
import com.seafile.seadroid2.ui.dialog_fragment.GetShareLinkPasswordDialogFragment;
import com.seafile.seadroid2.ui.repo.RepoService;
import com.seafile.seadroid2.ui.star.StarredService;

import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.TreeMap;
import java.util.stream.Collectors;

import io.reactivex.Completable;
import io.reactivex.Single;
import io.reactivex.SingleEmitter;
import io.reactivex.SingleOnSubscribe;
import io.reactivex.SingleSource;
import io.reactivex.functions.BiFunction;
import io.reactivex.functions.Function;

public class Objs {


    /// ///////////////////////////////starred////////////////////////////

    public static Single<List<StarredModel>> getStarredSingleFromServer(Account account) {
        Single<StarredWrapperModel> netSingle = HttpIO.getInstanceByAccount(account).execute(StarredService.class).getStarItems();
        Completable completable = AppDatabase.getInstance().starredDirentDAO().deleteAllByAccount(account.getSignature());
        Single<Integer> deleteSingle = completable.toSingleDefault(0);
        return Single.zip(netSingle, deleteSingle, new BiFunction<StarredWrapperModel, Integer, List<StarredModel>>() {
            @Override
            public List<StarredModel> apply(StarredWrapperModel starredWrapperModel, Integer integer) throws Exception {
                for (StarredModel starredModel : starredWrapperModel.starred_item_list) {
                    starredModel.related_account = account.getSignature();
                    if (!TextUtils.isEmpty(starredModel.mtime)) {
                        starredModel.mtime_long = Times.convertMtime2Long(starredModel.mtime);
                    }
                }

                return starredWrapperModel.starred_item_list;
            }
        }).flatMap(new Function<List<StarredModel>, SingleSource<List<StarredModel>>>() {
            @Override
            public SingleSource<List<StarredModel>> apply(List<StarredModel> starredModels) throws Exception {

                AppDatabase.getInstance().starredDirentDAO().insertAllSync(starredModels);
                return Single.just(starredModels);
            }
        });
    }


    /// ///////////////////////////////repo////////////////////////////

    public static Single<List<BaseModel>> getReposSingleFromServer(Account account) {
        Single<RepoWrapperModel> netSingle = HttpIO.getInstanceByAccount(account).execute(RepoService.class).getReposAsync();

        return netSingle.flatMap(new Function<RepoWrapperModel, SingleSource<List<RepoModel>>>() {
            @Override
            public SingleSource<List<RepoModel>> apply(RepoWrapperModel repoWrapperModel) throws Exception {
                //get data from server and convert to local data
                if (CollectionUtils.isEmpty(repoWrapperModel.repos)) {
                    return Single.just(Collections.emptyList());
                }

                for (RepoModel repoModel : repoWrapperModel.repos) {
                    repoModel.related_account = account.getSignature();
                    repoModel.last_modified_long = Times.convertMtime2Long(repoModel.last_modified);
                }

                return Single.just(repoWrapperModel.repos);
            }
        }).flatMap(new Function<List<RepoModel>, SingleSource<List<RepoModel>>>() {
            @Override
            public SingleSource<List<RepoModel>> apply(List<RepoModel> willSaveIntoLocalList) throws Exception {
                // delete local db
                Completable deleteCompletable = AppDatabase.getInstance().repoDao().deleteAllByAccount(account.getSignature());
                Single<Long> deleteSingle = deleteCompletable.toSingleDefault(0L);

                return deleteSingle.flatMap(new Function<Long, SingleSource<List<RepoModel>>>() {
                    @Override
                    public SingleSource<List<RepoModel>> apply(Long aLong) throws Exception {
                        return Single.just(willSaveIntoLocalList);
                    }
                });
            }
        }).flatMap(new Function<List<RepoModel>, SingleSource<List<RepoModel>>>() {
            @Override
            public SingleSource<List<RepoModel>> apply(List<RepoModel> willSaveIntoLocalList) throws Exception {
                //insert into db

                if (CollectionUtils.isEmpty(willSaveIntoLocalList)) {
                    return Single.just(willSaveIntoLocalList);
                }

                Completable insertCompletable = AppDatabase.getInstance().repoDao().insertAll(willSaveIntoLocalList);
                Single<Long> longSingle = insertCompletable.toSingleDefault(0L);
                return longSingle.flatMap(new Function<Long, SingleSource<List<RepoModel>>>() {
                    @Override
                    public SingleSource<List<RepoModel>> apply(Long aLong) throws Exception {
                        return Single.just(willSaveIntoLocalList);
                    }
                });
            }
        }).flatMap(new Function<List<RepoModel>, SingleSource<List<RepoModel>>>() {
            @Override
            public SingleSource<List<RepoModel>> apply(List<RepoModel> list) throws Exception {
                //insert into db

                List<RepoModel> cl = list.stream().filter(RepoModel::isCustomPermission).collect(Collectors.toList());
                if (CollectionUtils.isEmpty(cl)) {
                    return Single.just(list);
                }

                List<Single<PermissionEntity>> listSingle = new ArrayList<>();
                for (RepoModel repoModel : cl) {
                    Single<PermissionEntity> s = getSingleForLoadRepoPermissionFromRemote(repoModel.repo_id, repoModel.getCustomPermissionNum());
                    listSingle.add(s);
                }
                return Single.zip(listSingle, new Function<Object[], List<RepoModel>>() {
                    @Override
                    public List<RepoModel> apply(Object[] objs) throws Exception {
                        return list;
                    }
                });
            }
        }).flatMap(new Function<List<RepoModel>, SingleSource<List<BaseModel>>>() {
            @Override
            public SingleSource<List<BaseModel>> apply(List<RepoModel> savedIntoLocalList) throws Exception {
                //parse to adapter list data

                List<BaseModel> models = Objs.convertToAdapterList(savedIntoLocalList);
                return Single.just(models);
            }
        });
    }

    private static Single<PermissionEntity> getSingleForLoadRepoPermissionFromRemote(String repoId, int pNum) {
        Single<PermissionWrapperModel> single = HttpIO.getCurrentInstance().execute(RepoService.class).getCustomSharePermissionById(repoId, pNum);
        return single.flatMap(new Function<PermissionWrapperModel, SingleSource<PermissionEntity>>() {
            @Override
            public SingleSource<PermissionEntity> apply(PermissionWrapperModel wrapperModel) throws Exception {
                if (wrapperModel == null || wrapperModel.permission == null) {
                    return Single.just(new PermissionEntity());
                }

                return Single.create(new SingleOnSubscribe<PermissionEntity>() {
                    @Override
                    public void subscribe(SingleEmitter<PermissionEntity> emitter) throws Exception {
                        PermissionEntity permission = new PermissionEntity(repoId, wrapperModel.permission);
                        AppDatabase.getInstance().permissionDAO().insert(permission);
                        emitter.onSuccess(permission);
                    }
                });
            }
        });
    }

    public static List<BaseModel> convertToAdapterList(List<RepoModel> list) {
        return convertToAdapterList(list, false, false, null);
    }

    public static List<BaseModel> convertToAdapterList(List<RepoModel> list, boolean isFilterUnavailable) {
        return convertToAdapterList(list, isFilterUnavailable, false, null);
    }

    public static List<BaseModel> convertToAdapterList(List<RepoModel> list, boolean isFilterUnavailable, boolean isAddStarredGroup, List<String> filterIds) {
        if (CollectionUtils.isEmpty(list)) {
            return Collections.emptyList();
        }

        if (isFilterUnavailable) {
            list = list.stream()
                    .filter(f -> !f.encrypted && f.hasWritePermission())
                    .collect(Collectors.toList());
        }

        if (!CollectionUtils.isEmpty(filterIds)) {
            list = list.stream()
                    .filter(f -> filterIds.contains(f.repo_id))
                    .collect(Collectors.toList());
        }

        List<BaseModel> newRvList = CollectionUtils.newArrayList();

        TreeMap<String, List<RepoModel>> treeMap = groupRepos(list);

        // ShareToSeafileActivity used it, otherwise, we does not need to add it here
        if (isAddStarredGroup) {
            List<RepoModel> starredList = list.stream().filter(f -> f.starred).collect(Collectors.toList());
            if (!CollectionUtils.isEmpty(starredList)) {

                List<RepoModel> newRepoList = new ArrayList<>();
                for (RepoModel repoModel : starredList) {
                    RepoModel r = CloneUtils.deepClone(repoModel, RepoModel.class);
                    newRepoList.add(r);
                }

                List<RepoModel> sList = sortRepos(newRepoList);
                GroupItemModel groupItemModel = new GroupItemModel(R.string.starred_repos);
                for (RepoModel r : sList) {
                    //temp set group_name and group_id
                    r.group_name = groupItemModel.getTitle();
                    r.group_id = -1;
                }

                groupItemModel.addAllRepoList(sList);
                newRvList.add(groupItemModel);
                newRvList.addAll(sList);
            }
        }

        //mine
        List<RepoModel> mineList = treeMap.get(RepoType.TYPE_MINE);
        if (!CollectionUtils.isEmpty(mineList)) {
            List<RepoModel> sortedList = sortRepos(mineList);
            newRvList.add(new GroupItemModel(R.string.my_libraries, sortedList));
            newRvList.addAll(sortedList);
        }

        //shared
        List<RepoModel> sharedList = treeMap.get(RepoType.TYPE_SHARED);
        if (!CollectionUtils.isEmpty(sharedList)) {
            List<RepoModel> sortedList = sortRepos(sharedList);
            newRvList.add(new GroupItemModel(R.string.shared, sortedList));
            newRvList.addAll(sortedList);
        }


        //shared
        List<RepoModel> publicList = treeMap.get(RepoType.TYPE_PUBLIC);
        if (!CollectionUtils.isEmpty(publicList)) {
            List<RepoModel> sortedList = sortRepos(publicList);
            newRvList.add(new GroupItemModel(R.string.shared_with_all, sortedList));
            newRvList.addAll(sortedList);
        }

        for (String key : treeMap.keySet()) {
            if (TextUtils.equals(key, RepoType.TYPE_MINE)) {
            } else if (TextUtils.equals(key, RepoType.TYPE_SHARED)) {
            } else if (TextUtils.equals(key, RepoType.TYPE_PUBLIC)) {
            } else {
                List<RepoModel> groupList = treeMap.get(key);
                if (!CollectionUtils.isEmpty(groupList)) {
                    List<RepoModel> sortedList = sortRepos(groupList);
                    newRvList.add(new GroupItemModel(key, sortedList));
                    newRvList.addAll(sortedList);
                }
            }
        }

        return newRvList;
    }

    private static List<RepoModel> sortRepos(List<RepoModel> repos) {
        List<RepoModel> newRepos = new ArrayList<>();

        SortBy by = Settings.FILE_LIST_SORT_BY.queryValue();
        boolean isAscending = Settings.FILE_LIST_SORT_ASCENDING.queryValue();

        if (SortBy.NAME == by) {
            if (isAscending) {
                newRepos = repos.stream().sorted(new RepoNaturalOrderComparator()).collect(Collectors.toList());
            } else {
                newRepos = repos.stream().sorted(new RepoNaturalOrderComparator().reversed()).collect(Collectors.toList());
            }
        } else if (SortBy.TYPE == by) {
            newRepos = repos;
        } else if (SortBy.SIZE == by) {
            newRepos = repos.stream().sorted(new Comparator<RepoModel>() {
                @Override
                public int compare(RepoModel o1, RepoModel o2) {
                    if (isAscending) {
                        return Long.compare(o1.size, o2.size);
                    }
                    return -Long.compare(o1.size, o2.size);
                }
            }).collect(Collectors.toList());
        } else if (SortBy.LAST_MODIFIED == by) {
            newRepos = repos.stream().sorted(new Comparator<RepoModel>() {
                @Override
                public int compare(RepoModel o1, RepoModel o2) {
                    if (isAscending) {
                        return Long.compare(o1.last_modified_long, o2.last_modified_long);
                    }
                    return -Long.compare(o1.last_modified_long, o2.last_modified_long);
                }
            }).collect(Collectors.toList());
        }

        //calculate item_position
        if (CollectionUtils.isEmpty(newRepos)) {

        } else if (newRepos.size() == 1) {
            newRepos.get(0).item_position = ItemPositionEnum.ALL;
        } else {
            newRepos.get(0).item_position = ItemPositionEnum.START;
            newRepos.get(newRepos.size() - 1).item_position = ItemPositionEnum.END;
        }
        return newRepos;
    }

    private static TreeMap<String, List<RepoModel>> groupRepos(List<RepoModel> repos) {
        TreeMap<String, List<RepoModel>> map = new TreeMap<String, List<RepoModel>>();
        for (RepoModel repo : repos) {
            if (TextUtils.equals(repo.type, RepoType.TYPE_GROUP)) {
                if (repo.group_name == null) {
                    repo.group_name = "";
                }
                List<RepoModel> l = map.computeIfAbsent(repo.group_name, k -> Lists.newArrayList());
                l.add(repo);
            } else {
                List<RepoModel> l = map.computeIfAbsent(repo.type, k -> Lists.newArrayList());
                l.add(repo);
            }
        }
        return map;
    }


    /// ///////////////////////////////dirent////////////////////////////
    public static Single<List<DirentModel>> getDirentsSingleFromServer(Account account, String repoId, String repoName, String parentDir) {

        Single<DirentWrapperModel> netSingle = HttpIO.getInstanceByAccount(account).execute(RepoService.class).getDirentsAsync(repoId, parentDir);
        return netSingle.flatMap(new Function<DirentWrapperModel, SingleSource<List<DirentModel>>>() {
            @Override
            public SingleSource<List<DirentModel>> apply(DirentWrapperModel direntWrapperModel) throws Exception {
                return Single.create(new SingleOnSubscribe<List<DirentModel>>() {
                    @Override
                    public void subscribe(SingleEmitter<List<DirentModel>> emitter) throws Exception {
                        if (emitter.isDisposed()) {
                            return;
                        }
                        List<DirentModel> list = parseDirentsForDB(
                                direntWrapperModel.dirent_list,
                                direntWrapperModel.dir_id,
                                account.getSignature(),
                                repoId,
                                repoName);
                        emitter.onSuccess(list);
                    }
                });
            }
        }).flatMap(new Function<List<DirentModel>, SingleSource<List<DirentModel>>>() {
            @Override
            public SingleSource<List<DirentModel>> apply(List<DirentModel> netModels) throws Exception {
                Completable deleted = AppDatabase.getInstance().direntDao().deleteAllByParentPath(repoId, parentDir);
                Single<Long> deleteAllByPathSingle = deleted.toSingleDefault(0L);
                return deleteAllByPathSingle.flatMap(new Function<Long, SingleSource<List<DirentModel>>>() {
                    @Override
                    public SingleSource<List<DirentModel>> apply(Long aLong) throws Exception {
                        return Single.just(netModels);
                    }
                });
            }
        }).flatMap(new Function<List<DirentModel>, SingleSource<List<DirentModel>>>() {
            @Override
            public SingleSource<List<DirentModel>> apply(List<DirentModel> direntModels) throws Exception {
                if (CollectionUtils.isEmpty(direntModels)) {
                    return Single.just(direntModels);
                }

                Completable insertCompletable = AppDatabase.getInstance().direntDao().insertAll(direntModels);
                Single<Long> insertAllSingle = insertCompletable.toSingleDefault(0L);
                return insertAllSingle.flatMap(new Function<Long, SingleSource<List<DirentModel>>>() {
                    @Override
                    public SingleSource<List<DirentModel>> apply(Long aLong) throws Exception {
                        SLogs.d("getDirentsSingleFromServer()", "The list has been inserted into the local database");
                        return Single.just(direntModels);
                    }
                });
            }
        }).flatMap(new Function<List<DirentModel>, SingleSource<List<DirentModel>>>() {
            @Override
            public SingleSource<List<DirentModel>> apply(List<DirentModel> direntModels) throws Exception {
                if (CollectionUtils.isEmpty(direntModels)) {
                    return Single.just(direntModels);
                }

                List<CachedDirentModel> cachedDirentList = AppDatabase.getInstance().direntDao().getDirentsWithLocalFileIdSync(repoId, parentDir);

                List<DirentModel> newDirentModels = cachedDirentList.stream().map(new java.util.function.Function<CachedDirentModel, DirentModel>() {
                    @Override
                    public DirentModel apply(CachedDirentModel cachedDirentModel) {
                        cachedDirentModel.dirent.local_file_id = cachedDirentModel.local_file_id;
                        return cachedDirentModel.dirent;
                    }
                }).collect(Collectors.toList());

                //calculate item_position
                if (CollectionUtils.isEmpty(newDirentModels)) {

                } else if (newDirentModels.size() == 1) {
                    newDirentModels.get(0).item_position = ItemPositionEnum.ALL;
                } else if (newDirentModels.size() == 2) {
                    newDirentModels.get(0).item_position = ItemPositionEnum.START;
                    newDirentModels.get(1).item_position = ItemPositionEnum.END;
                } else {
                    newDirentModels.get(0).item_position = ItemPositionEnum.START;
                    newDirentModels.get(newDirentModels.size() - 1).item_position = ItemPositionEnum.END;
                }

                return Single.just(newDirentModels);
            }
        });
    }

    /**
     * Resolve the dirents of the local database
     */
    public static List<BaseModel> parseLocalDirents(List<DirentModel> list) {
        if (CollectionUtils.isEmpty(list)) {
            return Collections.emptyList();
        }

        TreeMap<String, List<DirentModel>> treeMap = groupDirents(list);
        List<DirentModel> dirModels = treeMap.get("dir");
        List<DirentModel> fileModels = treeMap.get("file");

        List<DirentModel> newList = new ArrayList<>();

        boolean isFolderFirst = Settings.FILE_LIST_SORT_FOLDER_FIRST.queryValue();
        if (isFolderFirst) {
            if (!CollectionUtils.isEmpty(dirModels)) {
                newList.addAll(sortDirents(dirModels));
            }

            if (!CollectionUtils.isEmpty(fileModels)) {
                newList.addAll(sortDirents(fileModels));
            }
        } else {
            if (!CollectionUtils.isEmpty(fileModels)) {
                newList.addAll(sortDirents(fileModels));
            }

            if (!CollectionUtils.isEmpty(dirModels)) {
                newList.addAll(sortDirents(dirModels));
            }
        }


        return new ArrayList<>(newList);
    }

    public static List<DirentModel> parseDirentsForDB(List<DirentModel> list,
                                                      String dir_id,
                                                      String related_account,
                                                      String repo_id,
                                                      String repo_name) {

        boolean isFolderFirst = Settings.FILE_LIST_SORT_FOLDER_FIRST.queryValue();
        return parseDirentsForDB(list, dir_id, related_account, repo_id, repo_name, isFolderFirst);
    }

    /**
     * Resolve to a list of local databases
     */
    public static List<DirentModel> parseDirentsForDB(List<DirentModel> list,
                                                      String dir_id,
                                                      String related_account,
                                                      String repo_id,
                                                      String repo_name,
                                                      boolean isFolderFirst) {
        if (CollectionUtils.isEmpty(list)) {
            return Collections.emptyList();
        }

        TreeMap<String, List<DirentModel>> treeMap = groupDirents(list);
        List<DirentModel> dirModels = treeMap.get("dir");
        List<DirentModel> fileModels = treeMap.get("file");

        List<DirentModel> newDbList = new ArrayList<>();


        List<DirentModel> dirList = null;
        List<DirentModel> fileList = null;

        if (!CollectionUtils.isEmpty(dirModels)) {
            for (int i = 0; i < dirModels.size(); i++) {
                //
                dirModels.get(i).last_modified_at = dirModels.get(i).mtime * 1000;
                dirModels.get(i).dir_id = dir_id;
                dirModels.get(i).related_account = related_account;
                dirModels.get(i).repo_id = repo_id;
                dirModels.get(i).repo_name = repo_name;
                dirModels.get(i).full_path = dirModels.get(i).parent_dir + dirModels.get(i).name;
                dirModels.get(i).uid = dirModels.get(i).getUID();
            }
            dirList = sortDirents(dirModels);
        }

        if (!CollectionUtils.isEmpty(fileModels)) {
            for (int i = 0; i < fileModels.size(); i++) {
                //
                fileModels.get(i).repo_id = repo_id;
                fileModels.get(i).repo_name = repo_name;
                fileModels.get(i).last_modified_at = fileModels.get(i).mtime * 1000;
                fileModels.get(i).dir_id = dir_id;
                fileModels.get(i).related_account = related_account;
                fileModels.get(i).full_path = fileModels.get(i).parent_dir + fileModels.get(i).name;
                fileModels.get(i).uid = fileModels.get(i).getUID();
            }
            fileList = sortDirents(fileModels);
        }

        if (isFolderFirst) {
            if (!CollectionUtils.isEmpty(dirList)) {
                newDbList.addAll(dirList);
            }
            if (!CollectionUtils.isEmpty(fileList)) {
                newDbList.addAll(fileList);
            }

        } else {
            if (!CollectionUtils.isEmpty(fileList)) {
                newDbList.addAll(fileList);
            }
            if (!CollectionUtils.isEmpty(dirList)) {
                newDbList.addAll(dirList);
            }
        }

        return newDbList;
    }

    private static TreeMap<String, List<DirentModel>> groupDirents(List<DirentModel> list) {
        TreeMap<String, List<DirentModel>> map = new TreeMap<String, List<DirentModel>>();
        for (DirentModel repo : list) {
            List<DirentModel> l = map.computeIfAbsent(repo.type, k -> Lists.newArrayList());
            l.add(repo);
        }
        return map;
    }

    private static List<DirentModel> sortDirents(List<DirentModel> list) {
        List<DirentModel> newList = new ArrayList<>();

        SortBy by = Settings.FILE_LIST_SORT_BY.queryValue();
        boolean isAscending = Settings.FILE_LIST_SORT_ASCENDING.queryValue();

        if (SortBy.NAME == by) {
            if (isAscending) {
                newList = list.stream().sorted(new DirentNaturalOrderComparator()).collect(Collectors.toList());
            } else {
                newList = list.stream().sorted(new DirentNaturalOrderComparator().reversed()).collect(Collectors.toList());
            }
        } else if (SortBy.TYPE == by) {
            if (isAscending) {
                newList = list.stream().sorted(Comparator
                                .comparing((DirentModel d) -> d.getFileExt().toLowerCase())
                                .thenComparing(d -> d.name.toLowerCase()))
                        .collect(Collectors.toList());
            } else {
                newList = list.stream().sorted(Comparator
                                .comparing((DirentModel d) -> d.getFileExt().toLowerCase(), Comparator.reverseOrder())
                                .thenComparing(d -> d.name.toLowerCase(), Comparator.reverseOrder()))
                        .collect(Collectors.toList());
            }

        } else if (SortBy.SIZE == by) {
            newList = list.stream().sorted(new Comparator<DirentModel>() {
                @Override
                public int compare(DirentModel o1, DirentModel o2) {
                    if (isAscending) {
                        return Long.compare(o1.size, o2.size);
                    }
                    return -Long.compare(o1.size, o2.size);
                }
            }).collect(Collectors.toList());
        } else if (SortBy.LAST_MODIFIED == by) {
            newList = list.stream().sorted(new Comparator<DirentModel>() {
                @Override
                public int compare(DirentModel o1, DirentModel o2) {
                    if (isAscending) {
                        return Long.compare(o1.mtime, o2.mtime);
                    }
                    return -Long.compare(o1.mtime, o2.mtime);
                }
            }).collect(Collectors.toList());
        }

        return newList;
    }


    public static void showChooseAppDialog(Context context, FragmentManager fragmentManager, DirentShareLinkModel shareLinkModel, boolean isDir) {
        String title = context.getString(isDir ? R.string.share_dir_link : R.string.share_file_link);

        Intent shareIntent = new Intent();
        shareIntent.setAction(Intent.ACTION_SEND);
        shareIntent.setType("text/plain");
        List<ResolveInfo> infos = WidgetUtils.getAppsByIntent(shareIntent);

        AppChoiceDialogFragment dialog = new AppChoiceDialogFragment();
        dialog.addCustomAction(0,
                ContextCompat.getDrawable(context, R.drawable.copy_link),
                context.getString(R.string.copy_link));
        dialog.init(title, infos, new AppChoiceDialogFragment.OnItemSelectedListener() {
            @Override
            public void onAppSelected(ResolveInfo appInfo) {
                String className = appInfo.activityInfo.name;
                String packageName = appInfo.activityInfo.packageName;
                shareIntent.setClassName(packageName, className);
                shareIntent.putExtra(Intent.EXTRA_TEXT, shareLinkModel.link);
                context.startActivity(shareIntent);
                dialog.dismiss();
            }

            @Override
            public void onCustomActionSelected(AppChoiceDialogFragment.CustomAction action) {
                ClipboardUtils.copyText(shareLinkModel.link);
                Toasts.show(R.string.link_ready_to_be_pasted);
                dialog.dismiss();
            }
        });
        dialog.show(fragmentManager, AppChoiceDialogFragment.class.getSimpleName());
    }

    public static void showCreateShareLinkDialog(Context context, FragmentManager fragmentManager, DirentModel direntModel, boolean isAdvance) {
        if (!NetworkUtils.isConnected()) {
            Toasts.show(R.string.network_error);
            return;
        }

        if (direntModel == null){
            return;
        }

        GetShareLinkPasswordDialogFragment dialogFragment = new GetShareLinkPasswordDialogFragment();
        dialogFragment.init(direntModel.repo_id, direntModel.full_path, isAdvance);
        dialogFragment.setOnCreateDirentShareLinkListener(new OnCreateDirentShareLinkListener() {
            @Override
            public void onCreateDirentShareLink(DirentShareLinkModel linkModel) {
                if (linkModel == null) {
                    dialogFragment.dismiss();
                    return;
                }
                showChooseAppDialog(context, fragmentManager, linkModel, direntModel.isDir());
                dialogFragment.dismiss();
            }
        });
        dialogFragment.show(fragmentManager, GetShareLinkPasswordDialogFragment.class.getSimpleName());
    }

    private static ResolveInfo getWeChatIntent(Intent intent) {
        PackageManager pm = SeadroidApplication.getAppContext().getPackageManager();
        List<ResolveInfo> infos = pm.queryIntentActivities(intent, 0);
        for (ResolveInfo info : infos) {
            if (info.activityInfo.packageName.equals("com.tencent.mm")) {
                return info;
            }
        }

        return null;
    }

    /**
     * share link to wechat
     */
    public static void shareDirToWeChat(Fragment context, String repo_id, String full_path) {
        Intent shareIntent = new Intent();
        shareIntent.setAction(Intent.ACTION_SEND);
        shareIntent.setType("text/plain");

        ResolveInfo weChatInfo = getWeChatIntent(shareIntent);
        if (weChatInfo == null) {
            Toasts.show(R.string.no_app_available);
            return;
        }

        String className = weChatInfo.activityInfo.name;
        String packageName = weChatInfo.activityInfo.packageName;
        shareIntent.setClassName(packageName, className);

        GetShareLinkPasswordDialogFragment dialogFragment = new GetShareLinkPasswordDialogFragment();
        dialogFragment.init(repo_id, full_path, false);
        dialogFragment.setOnCreateDirentShareLinkListener(new OnCreateDirentShareLinkListener() {
            @Override
            public void onCreateDirentShareLink(DirentShareLinkModel linkModel) {
                if (linkModel == null) {
                    dialogFragment.dismiss();
                    return;
                }

                shareIntent.putExtra(Intent.EXTRA_TEXT, linkModel.link);
                context.startActivity(shareIntent);
                dialogFragment.dismiss();
            }
        });
        dialogFragment.show(context.getChildFragmentManager(), GetShareLinkPasswordDialogFragment.class.getSimpleName());
    }

    /**
     * share file to wachat
     */
    public static void shareFileToWeChat(Fragment context, File file) {

        Uri uri = FileProvider.getUriForFile(context.requireContext(), BuildConfig.FILE_PROVIDER_AUTHORITIES, file);

        final Intent sendIntent = new Intent();
        sendIntent.setAction(Intent.ACTION_SEND);
        sendIntent.setType(Utils.getFileMimeType(file));
        sendIntent.putExtra(Intent.EXTRA_STREAM, uri);

        ResolveInfo weChatInfo = getWeChatIntent(sendIntent);
        if (weChatInfo == null) {
            Toasts.show(R.string.no_app_available);
            return;
        }

        String className = weChatInfo.activityInfo.name;
        String packageName = weChatInfo.activityInfo.packageName;
        sendIntent.setClassName(packageName, className);
        context.startActivity(sendIntent);
    }

    /**
     * Export a file.
     * 1. first ask the user to choose an app
     * 2. then download the latest version of the file
     * 3. start the choosen app
     */
    public static void exportFile(Fragment context, File localFile) {

        Uri uri = FileProvider.getUriForFile(context.requireContext(), BuildConfig.FILE_PROVIDER_AUTHORITIES, localFile);

        final Intent sendIntent = new Intent();
        sendIntent.setAction(Intent.ACTION_SEND);
        sendIntent.setType(Utils.getFileMimeType(localFile));
        sendIntent.putExtra(Intent.EXTRA_STREAM, uri);

        // Get a list of apps
        List<ResolveInfo> infos = WidgetUtils.getAppsByIntent(sendIntent);
        if (infos.isEmpty()) {
            Toasts.show(R.string.no_app_available);
            return;
        }

        AppChoiceDialogFragment dialog = new AppChoiceDialogFragment();
        dialog.init(context.getString(R.string.export_file), infos, new AppChoiceDialogFragment.OnItemSelectedListener() {
            @Override
            public void onCustomActionSelected(AppChoiceDialogFragment.CustomAction action) {
            }

            @Override
            public void onAppSelected(ResolveInfo appInfo) {
                String className = appInfo.activityInfo.name;
                String packageName = appInfo.activityInfo.packageName;
                sendIntent.setClassName(packageName, className);

                context.startActivity(sendIntent);
            }
        });
        dialog.show(context.getChildFragmentManager(), AppChoiceDialogFragment.class.getSimpleName());
    }
}
