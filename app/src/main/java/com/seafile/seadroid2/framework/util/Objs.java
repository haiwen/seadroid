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
import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.TimeUtils;
import com.blankj.utilcode.util.ToastUtils;
import com.google.common.collect.Lists;
import com.seafile.seadroid2.BuildConfig;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.config.RepoType;
import com.seafile.seadroid2.framework.data.db.AppDatabase;
import com.seafile.seadroid2.framework.data.db.entities.DirentModel;
import com.seafile.seadroid2.framework.data.db.entities.FileTransferEntity;
import com.seafile.seadroid2.framework.data.db.entities.RepoModel;
import com.seafile.seadroid2.framework.data.db.entities.StarredModel;
import com.seafile.seadroid2.framework.data.model.BaseModel;
import com.seafile.seadroid2.framework.data.model.GroupItemModel;
import com.seafile.seadroid2.framework.data.model.enums.TransferStatus;
import com.seafile.seadroid2.framework.data.model.objs.DirentShareLinkModel;
import com.seafile.seadroid2.framework.data.model.repo.DirentWrapperModel;
import com.seafile.seadroid2.framework.data.model.repo.RepoWrapperModel;
import com.seafile.seadroid2.framework.data.model.star.StarredWrapperModel;
import com.seafile.seadroid2.framework.datastore.sp.Sorts;
import com.seafile.seadroid2.framework.http.HttpIO;
import com.seafile.seadroid2.listener.OnCreateDirentShareLinkListener;
import com.seafile.seadroid2.ui.dialog_fragment.AppChoiceDialogFragment;
import com.seafile.seadroid2.ui.dialog_fragment.GetShareLinkPasswordDialogFragment;
import com.seafile.seadroid2.ui.repo.RepoService;
import com.seafile.seadroid2.ui.star.StarredService;

import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.stream.Collectors;

import io.reactivex.Completable;
import io.reactivex.Single;
import io.reactivex.SingleSource;
import io.reactivex.functions.BiFunction;
import io.reactivex.functions.Function;
import kotlin.Pair;
import kotlin.Triple;

public class Objs {

    ////////////////////////////
    //////starred
    ////////////////////////////

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


    ////////////////////////////
    //////repo
    ////////////////////////////
    public static Single<List<BaseModel>> getReposSingleFromServer(Account account) {
        Single<RepoWrapperModel> netSingle = HttpIO.getInstanceByAccount(account).execute(RepoService.class).getRepos();
        Single<List<RepoModel>> dbListSingle = AppDatabase.getInstance().repoDao().getListByAccount(account.getSignature());

        //load net data and load local data

        return Single.zip(netSingle, dbListSingle, new BiFunction<RepoWrapperModel, List<RepoModel>, Triple<RepoWrapperModel, List<RepoModel>, List<RepoModel>>>() {
            @Override
            public Triple<RepoWrapperModel, List<RepoModel>, List<RepoModel>> apply(RepoWrapperModel repoWrapperModel, List<RepoModel> dbModels) throws Exception {
                //get data from server and local

                List<RepoModel> net2dbList = Objs.parseRepoListForDB(repoWrapperModel.repos, account.getSignature());

                //diffs.first = delete list
                //diffs.second = insert db list
                Pair<List<RepoModel>, List<RepoModel>> diffs = Objs.diffRepos(net2dbList, dbModels);
                if (diffs == null) {
                    return new Triple<>(repoWrapperModel, null, null);
                }

                return new Triple<>(repoWrapperModel, diffs.getFirst(), diffs.getSecond());
            }
        }).flatMap(new Function<Triple<RepoWrapperModel, List<RepoModel>, List<RepoModel>>, SingleSource<Pair<RepoWrapperModel, List<RepoModel>>>>() {
            @Override
            public SingleSource<Pair<RepoWrapperModel, List<RepoModel>>> apply(Triple<RepoWrapperModel, List<RepoModel>, List<RepoModel>> triple) throws Exception {
                // delete local db

                if (CollectionUtils.isEmpty(triple.getSecond())) {
                    return Single.just(new Pair<>(triple.getFirst(), triple.getThird()));
                }

                List<String> ids = triple.getSecond().stream().map(m -> m.repo_id).collect(Collectors.toList());
                Completable deleteCompletable = AppDatabase.getInstance().repoDao().deleteAllByIds(ids);
                Single<Long> deleteSingle = deleteCompletable.toSingleDefault(0L);

                return deleteSingle.flatMap(new Function<Long, SingleSource<Pair<RepoWrapperModel, List<RepoModel>>>>() {
                    @Override
                    public SingleSource<Pair<RepoWrapperModel, List<RepoModel>>> apply(Long aLong) throws Exception {

                        return Single.just(new Pair<>(triple.getFirst(), triple.getThird()));
                    }
                });
            }
        }).flatMap(new Function<Pair<RepoWrapperModel, List<RepoModel>>, SingleSource<RepoWrapperModel>>() {
            @Override
            public SingleSource<RepoWrapperModel> apply(Pair<RepoWrapperModel, List<RepoModel>> pair) throws Exception {
                //insert into db

                if (CollectionUtils.isEmpty(pair.getSecond())) {
                    return Single.just(pair.getFirst());
                }

                Completable insertCompletable = AppDatabase.getInstance().repoDao().insertAll(pair.getSecond());
                Single<Long> longSingle = insertCompletable.toSingleDefault(0L);
                return longSingle.flatMap(new Function<Long, SingleSource<RepoWrapperModel>>() {
                    @Override
                    public SingleSource<RepoWrapperModel> apply(Long aLong) throws Exception {
                        return Single.just(pair.getFirst());
                    }
                });
            }
        }).flatMap(new Function<RepoWrapperModel, SingleSource<List<BaseModel>>>() {
            @Override
            public SingleSource<List<BaseModel>> apply(RepoWrapperModel repoWrapperModel) throws Exception {
                //parse to adapter list data

                List<BaseModel> models = Objs.parseRepoListForAdapter(repoWrapperModel.repos, account.getSignature(), false);
                return Single.just(models);
            }
        });
    }

    public static List<BaseModel> parseRepoListForAdapter(List<RepoModel> list, String related_account, boolean isFilterEncrypted) {
        if (CollectionUtils.isEmpty(list)) {
            return Collections.emptyList();
        }

        for (int i = 0; i < list.size(); i++) {
            list.get(i).related_account = related_account;
        }

        if (isFilterEncrypted) {
            list = list.stream().filter(f -> !f.encrypted).collect(Collectors.toList());
        }

        List<BaseModel> newRvList = CollectionUtils.newArrayList();

        TreeMap<String, List<RepoModel>> treeMap = groupRepos(list);

        //mine
        List<RepoModel> mineList = treeMap.get(RepoType.TYPE_MINE);
        if (!CollectionUtils.isEmpty(mineList)) {
            newRvList.add(new GroupItemModel(R.string.personal));
            for (RepoModel repoModel : mineList) {
                repoModel.last_modified_long = Times.convertMtime2Long(repoModel.last_modified);
            }

            List<RepoModel> sortedList = sortRepos(mineList);
            newRvList.addAll(sortedList);
        }

        //shared
        List<RepoModel> sharedList = treeMap.get(RepoType.TYPE_SHARED);
        if (!CollectionUtils.isEmpty(sharedList)) {
            newRvList.add(new GroupItemModel(R.string.shared));
            for (RepoModel repoModel : sharedList) {
                repoModel.last_modified_long = Times.convertMtime2Long(repoModel.last_modified);
            }

            List<RepoModel> sortedList = sortRepos(sharedList);
            newRvList.addAll(sortedList);
        }

        for (String key : treeMap.keySet()) {
            if (TextUtils.equals(key, RepoType.TYPE_MINE)) {
            } else if (TextUtils.equals(key, RepoType.TYPE_SHARED)) {
            } else {
                List<RepoModel> groupList = treeMap.get(key);
                if (!CollectionUtils.isEmpty(groupList)) {
                    newRvList.add(new GroupItemModel(key));
                    for (RepoModel repoModel : groupList) {
                        repoModel.last_modified_long = Times.convertMtime2Long(repoModel.last_modified);
                    }

                    List<RepoModel> sortedList = sortRepos(groupList);
                    newRvList.addAll(sortedList);
                }
            }
        }

        return newRvList;
    }

    public static List<RepoModel> parseRepoListForDB(List<RepoModel> list, String related_account) {
        if (CollectionUtils.isEmpty(list)) {
            return Collections.emptyList();
        }

        for (int i = 0; i < list.size(); i++) {
            list.get(i).related_account = related_account;
        }

        List<RepoModel> newDbList = CollectionUtils.newArrayList();

        TreeMap<String, List<RepoModel>> treeMap = groupRepos(list);

        //mine
        List<RepoModel> mineList = treeMap.get(RepoType.TYPE_MINE);
        if (!CollectionUtils.isEmpty(mineList)) {
            for (RepoModel repoModel : mineList) {
                repoModel.last_modified_long = Times.convertMtime2Long(repoModel.last_modified);
            }

            List<RepoModel> sortedList = sortRepos(mineList);
            newDbList.addAll(sortedList);
        }

        //shared
        List<RepoModel> sharedList = treeMap.get(RepoType.TYPE_SHARED);
        if (!CollectionUtils.isEmpty(sharedList)) {
            for (RepoModel repoModel : sharedList) {
                repoModel.last_modified_long = Times.convertMtime2Long(repoModel.last_modified);
            }

            List<RepoModel> sortedList = sortRepos(sharedList);
            newDbList.addAll(sortedList);
        }

        for (String key : treeMap.keySet()) {
            if (TextUtils.equals(key, RepoType.TYPE_MINE)) {
            } else if (TextUtils.equals(key, RepoType.TYPE_SHARED)) {
            } else {
                List<RepoModel> groupList = treeMap.get(key);
                if (!CollectionUtils.isEmpty(groupList)) {
                    for (RepoModel repoModel : groupList) {
                        repoModel.last_modified_long = Times.convertMtime2Long(repoModel.last_modified);
                    }

                    List<RepoModel> sortedList = sortRepos(groupList);
                    newDbList.addAll(sortedList);
                }
            }
        }
        return newDbList;
    }

    /**
     * Whether the dbList is included in the netList.<br>
     * pair.first is need to delete.<br>
     * pair.second is need to add.<br>
     */
    public static Pair<List<RepoModel>, List<RepoModel>> diffRepos(List<RepoModel> netList, List<RepoModel> dbList) {

        if (CollectionUtils.isEmpty(netList) && CollectionUtils.isEmpty(dbList)) {
            return null;
        }

        //if netList is empty, delete all local data.
        if (CollectionUtils.isEmpty(netList)) {
            return new Pair<>(dbList, null);
        }

        //if dbList is empty, insert all net data into DB.
        if (CollectionUtils.isEmpty(dbList)) {
            return new Pair<>(null, netList);
        }

        List<String> repoIds = netList.stream().map(m -> m.repo_id).collect(Collectors.toList());
        List<RepoModel> deleteList = dbList.stream().filter(f -> !repoIds.contains(f.repo_id)).collect(Collectors.toList());
        List<RepoModel> addList = dbList.stream().filter(f -> repoIds.contains(f.repo_id)).collect(Collectors.toList());

        for (RepoModel nModel : netList) {
            for (RepoModel repoModel : addList) {
                if (TextUtils.equals(nModel.repo_id, repoModel.repo_id)) {
                    nModel.root = repoModel.root;
                    nModel.magic = repoModel.magic;
                    nModel.random_key = repoModel.random_key;
                    nModel.enc_version = repoModel.enc_version;
                    nModel.file_count = repoModel.file_count;
                    break;
                }
            }
        }

        return new Pair<>(deleteList, netList);
    }

    private static List<RepoModel> sortRepos(List<RepoModel> repos) {
        List<RepoModel> newRepos = new ArrayList<>();

        int sortType = Sorts.getSortType();
        switch (sortType) {
            case 0: // sort by name, ascending
                newRepos = repos.stream().sorted(new Comparator<RepoModel>() {
                    @Override
                    public int compare(RepoModel o1, RepoModel o2) {
                        return o1.repo_name.compareTo(o2.repo_name);
                    }
                }).collect(Collectors.toList());

                break;
            case 1: // sort by name, descending
                newRepos = repos.stream().sorted(new Comparator<RepoModel>() {
                    @Override
                    public int compare(RepoModel o1, RepoModel o2) {
                        return -o1.repo_name.compareTo(o2.repo_name);
                    }
                }).collect(Collectors.toList());
                break;
            case 2: // sort by last modified time, ascending
                newRepos = repos.stream().sorted(new Comparator<RepoModel>() {
                    @Override
                    public int compare(RepoModel o1, RepoModel o2) {
                        return o1.last_modified_long < o2.last_modified_long ? -1 : 1;
                    }
                }).collect(Collectors.toList());
                break;
            case 3: // sort by last modified time, descending
                newRepos = repos.stream().sorted(new Comparator<RepoModel>() {
                    @Override
                    public int compare(RepoModel o1, RepoModel o2) {
                        return o1.last_modified_long > o2.last_modified_long ? -1 : 1;
                    }
                }).collect(Collectors.toList());
                break;
        }
        return newRepos;
    }

    private static TreeMap<String, List<RepoModel>> groupRepos(List<RepoModel> repos) {
        TreeMap<String, List<RepoModel>> map = new TreeMap<String, List<RepoModel>>();
        for (RepoModel repo : repos) {
            if (TextUtils.equals(repo.type, RepoType.TYPE_GROUP)) {
                List<RepoModel> l = map.computeIfAbsent(repo.group_name, k -> Lists.newArrayList());
                l.add(repo);
            } else {
                List<RepoModel> l = map.computeIfAbsent(repo.type, k -> Lists.newArrayList());
                l.add(repo);
            }
        }
        return map;
    }

    ////////////////////////////
    //////dirent
    ////////////////////////////

    public static Single<List<DirentModel>> getDirentsSingleFromServer(Account account, String repoId, String repoName, String parentDir) {
        Single<DirentWrapperModel> netSingle = HttpIO.getInstanceByAccount(account).execute(RepoService.class).getDirents(repoId, parentDir);
        Single<List<DirentModel>> dbSingle = AppDatabase.getInstance().direntDao().getListByParentPath(repoId, parentDir);

        return Single.zip(netSingle, dbSingle, new BiFunction<DirentWrapperModel, List<DirentModel>, List<DirentModel>>() {
            @Override
            public List<DirentModel> apply(DirentWrapperModel direntWrapperModel, List<DirentModel> direntModels) throws Exception {
                return Objs.parseDirentsForDB(
                        direntWrapperModel.dirent_list,
                        direntWrapperModel.dir_id,
                        account.getSignature(),
                        repoId,
                        repoName);
            }
        }).flatMap(new Function<List<DirentModel>, SingleSource<List<DirentModel>>>() {
            @Override
            public SingleSource<List<DirentModel>> apply(List<DirentModel> netModels) throws Exception {
                if (CollectionUtils.isEmpty(netModels)) {
                    return Single.just(netModels);
                }

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

                        SLogs.d("Dirents本地数据库已更新");
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

                Single<List<FileTransferEntity>> curParentDownloadedList = AppDatabase.getInstance().fileTransferDAO().getDownloadedListByParentAsync(repoId, parentDir);


                return curParentDownloadedList.flatMap(new Function<List<FileTransferEntity>, SingleSource<List<DirentModel>>>() {
                    @Override
                    public SingleSource<List<DirentModel>> apply(List<FileTransferEntity> fileTransferEntities) throws Exception {

                        Map<String, FileTransferEntity> transferMap = new HashMap<>(fileTransferEntities.size());
                        for (FileTransferEntity fileTransferEntity : fileTransferEntities) {
                            transferMap.put(fileTransferEntity.full_path, fileTransferEntity);
                        }

                        for (DirentModel direntModel : direntModels) {
                            String fullPath = direntModel.parent_dir + direntModel.name;

                            if (!transferMap.containsKey(fullPath)) {
                                continue;
                            }

                            FileTransferEntity entity = transferMap.get(fullPath);
                            if (entity.transfer_status == TransferStatus.SUCCEEDED) {
                                direntModel.transfer_status = entity.transfer_status;
                                direntModel.local_file_path = entity.target_path;
                            }
                        }

                        return Single.just(direntModels);
                    }
                });
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
        if (!CollectionUtils.isEmpty(dirModels)) {
            newList.addAll(sortDirents(dirModels));
        }

        if (!CollectionUtils.isEmpty(fileModels)) {
            newList.addAll(sortDirents(fileModels));
        }

        return new ArrayList<>(newList);
    }

    /**
     * Resolve to a list of local databases
     */
    public static List<DirentModel> parseDirentsForDB(List<DirentModel> list,
                                                      String dir_id,
                                                      String related_account,
                                                      String repo_id,
                                                      String repo_name) {
        if (CollectionUtils.isEmpty(list)) {
            return Collections.emptyList();
        }

        TreeMap<String, List<DirentModel>> treeMap = groupDirents(list);
        List<DirentModel> dirModels = treeMap.get("dir");
        List<DirentModel> fileModels = treeMap.get("file");

        List<DirentModel> newDbList = new ArrayList<>();
        long now = TimeUtils.getNowMills();
        if (!CollectionUtils.isEmpty(dirModels)) {
            for (int i = 0; i < dirModels.size(); i++) {
                //
                dirModels.get(i).last_modified_at = now;
                dirModels.get(i).dir_id = dir_id;
                dirModels.get(i).related_account = related_account;
                dirModels.get(i).repo_id = repo_id;
                dirModels.get(i).repo_name = repo_name;
                dirModels.get(i).full_path = dirModels.get(i).parent_dir + dirModels.get(i).name;
                dirModels.get(i).uid = dirModels.get(i).getUID();
            }
            newDbList.addAll(sortDirents(dirModels));
        }

        if (!CollectionUtils.isEmpty(fileModels)) {
            for (int i = 0; i < fileModels.size(); i++) {
                //
                fileModels.get(i).repo_id = repo_id;
                fileModels.get(i).repo_name = repo_name;
                fileModels.get(i).last_modified_at = now;
                fileModels.get(i).dir_id = dir_id;
                fileModels.get(i).related_account = related_account;
                fileModels.get(i).full_path = fileModels.get(i).parent_dir + fileModels.get(i).name;
                fileModels.get(i).uid = fileModels.get(i).getUID();
            }
            newDbList.addAll(sortDirents(fileModels));
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

        int sortType = Sorts.getSortType();
        switch (sortType) {
            case 0: // sort by name, ascending
                newList = list.stream().sorted(new Comparator<DirentModel>() {
                    @Override
                    public int compare(DirentModel o1, DirentModel o2) {
                        return o1.name.compareTo(o2.name);
                    }
                }).collect(Collectors.toList());

                break;
            case 1: // sort by name, descending
                newList = list.stream().sorted(new Comparator<DirentModel>() {
                    @Override
                    public int compare(DirentModel o1, DirentModel o2) {
                        return -o1.name.compareTo(o2.name);
                    }
                }).collect(Collectors.toList());
                break;
            case 2: // sort by last modified time, ascending
                newList = list.stream().sorted(new Comparator<DirentModel>() {
                    @Override
                    public int compare(DirentModel o1, DirentModel o2) {
                        return Long.compare(o1.mtime,o2.mtime);
                    }
                }).collect(Collectors.toList());
                break;
            case 3: // sort by last modified time, descending
                newList = list.stream().sorted(new Comparator<DirentModel>() {
                    @Override
                    public int compare(DirentModel o1, DirentModel o2) {

                        return -Long.compare(o1.mtime,o2.mtime);
                    }
                }).collect(Collectors.toList());
                break;
        }
        return newList;
    }


    public static List<ResolveInfo> getAppsByIntent(Intent intent) {
        PackageManager pm = SeadroidApplication.getAppContext().getPackageManager();
        List<ResolveInfo> infos = pm.queryIntentActivities(intent, 0);

        // Remove seafile app from the list
        String seadroidPackageName = SeadroidApplication.getAppContext().getPackageName();
        ResolveInfo info;
        Iterator<ResolveInfo> iter = infos.iterator();
        while (iter.hasNext()) {
            info = iter.next();
            if (info.activityInfo.packageName.equals(seadroidPackageName)) {
                iter.remove();
            }
        }

        return infos;
    }

    public static void showChooseAppDialog(Context context, FragmentManager fragmentManager, DirentShareLinkModel shareLinkModel, boolean isDir) {
        String title = context.getString(isDir ? R.string.share_dir_link : R.string.share_file_link);

        Intent shareIntent = new Intent();
        shareIntent.setAction(Intent.ACTION_SEND);
        shareIntent.setType("text/plain");
        List<ResolveInfo> infos = getAppsByIntent(shareIntent);

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
                ToastUtils.showLong(R.string.link_ready_to_be_pasted);
                dialog.dismiss();
            }
        });
        dialog.show(fragmentManager, AppChoiceDialogFragment.class.getSimpleName());
    }

    public static void showCreateShareLinkDialog(Context context, FragmentManager fragmentManager, DirentModel direntModel, boolean isAdvance) {
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
            ToastUtils.showLong(R.string.no_app_available);
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
            ToastUtils.showLong(R.string.no_app_available);
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
        List<ResolveInfo> infos = getAppsByIntent(sendIntent);
        if (infos.isEmpty()) {
            ToastUtils.showLong(R.string.no_app_available);
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
