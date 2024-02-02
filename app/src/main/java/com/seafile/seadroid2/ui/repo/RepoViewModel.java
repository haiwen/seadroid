package com.seafile.seadroid2.ui.repo;

import android.text.TextUtils;

import androidx.lifecycle.MutableLiveData;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.EncryptUtils;
import com.blankj.utilcode.util.TimeUtils;
import com.blankj.utilcode.util.ToastUtils;
import com.google.common.collect.Lists;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.config.Constants;
import com.seafile.seadroid2.data.db.AppDatabase;
import com.seafile.seadroid2.data.model.BaseModel;
import com.seafile.seadroid2.data.model.GroupItemModel;
import com.seafile.seadroid2.data.model.ResultModel;
import com.seafile.seadroid2.data.db.entities.ObjsModel;
import com.seafile.seadroid2.data.model.repo.DirentMiniModel;
import com.seafile.seadroid2.data.remote.api.RepoService;
import com.seafile.seadroid2.ui.base.viewmodel.BaseViewModel;
import com.seafile.seadroid2.context.NavContext;
import com.seafile.seadroid2.data.StorageManager;
import com.seafile.seadroid2.io.http.IO;
import com.seafile.seadroid2.data.db.entities.DirentModel;
import com.seafile.seadroid2.data.model.repo.DirentWrapperModel;
import com.seafile.seadroid2.data.db.entities.RepoModel;
import com.seafile.seadroid2.data.model.repo.RepoWrapperModel;
import com.seafile.seadroid2.util.SLogs;
import com.seafile.seadroid2.util.sp.Sorts;
import com.seafile.seadroid2.util.Times;

import java.io.File;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.stream.Collectors;

import io.reactivex.Completable;
import io.reactivex.Single;
import io.reactivex.functions.Action;
import io.reactivex.functions.Consumer;
import kotlin.Pair;
import okhttp3.RequestBody;

public class RepoViewModel extends BaseViewModel {

    private final MutableLiveData<List<BaseModel>> ObjsListLiveData = new MutableLiveData<>();
    private final MutableLiveData<Boolean> StarLiveData = new MutableLiveData<>();

    public MutableLiveData<Boolean> getStarLiveData() {
        return StarLiveData;
    }

    public MutableLiveData<List<BaseModel>> getObjsListLiveData() {
        return ObjsListLiveData;
    }

    public void getObjFromDB(String repoId, Consumer<ObjsModel> consumer) {
        Single<List<ObjsModel>> single = AppDatabase.getInstance().objDao().getByPath(repoId, Constants.ObjType.REPO);
        addSingleDisposable(single, new Consumer<List<ObjsModel>>() {
            @Override
            public void accept(List<ObjsModel> objsModels) throws Exception {
                if (!CollectionUtils.isEmpty(objsModels)) {
                    consumer.accept(objsModels.get(0));
                } else {
                    consumer.accept(null);
                }
            }
        });
    }

    public void loadData(NavContext context, boolean forceRefresh) {
        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        if (account == null) {
            return;
        }

        if (!context.isInRepo()) {
            loadReposFromDB(account, forceRefresh);
        } else {
            loadDirentsFromDb(account, context, forceRefresh);
        }
    }

    private void loadReposFromDB(Account account, boolean isForce) {
        Single<List<RepoModel>> singleDB = AppDatabase.getInstance().repoDao().getAllByAccount(account.email);
        addSingleDisposable(singleDB, new Consumer<List<RepoModel>>() {
            @Override
            public void accept(List<RepoModel> repoModels) throws Exception {
                if (!CollectionUtils.isEmpty(repoModels)) {
                    Pair<List<BaseModel>, List<RepoModel>> pair = parseRepos(repoModels, account.email);

                    getObjsListLiveData().setValue(null == pair ? null : pair.getFirst());

                    if (isForce) {
                        loadReposFromNet(account);
                    }
                } else {
                    loadReposFromNet(account);
                }
            }
        });
    }

    private void loadReposFromNet(Account account) {
        getRefreshLiveData().setValue(true);

        Single<RepoWrapperModel> singleNet = IO.getSingleton().execute(RepoService.class).getRepos();
        addSingleDisposable(singleNet, new Consumer<RepoWrapperModel>() {
            @Override
            public void accept(RepoWrapperModel repoWrapperModel) throws Exception {
                getRefreshLiveData().setValue(false);

                if (repoWrapperModel == null || CollectionUtils.isEmpty(repoWrapperModel.repos)) {
                    getObjsListLiveData().setValue(null);
                    return;
                }

                Pair<List<BaseModel>, List<RepoModel>> pair = parseRepos(repoWrapperModel.repos, account.email);

                if (null == pair) {
                    getObjsListLiveData().setValue(null);
                    return;
                }

                Completable completableDelete = AppDatabase.getInstance().repoDao().deleteAllByAccount(account.email);
                Completable completableInsert = AppDatabase.getInstance().repoDao().insertAll(pair.getSecond());
                Completable completable = Completable.mergeArray(completableDelete, completableInsert);
                addCompletableDisposable(completable, new Action() {
                    @Override
                    public void run() throws Exception {
                        SLogs.d("Dirents本地数据库已更新");
                    }
                });

                getObjsListLiveData().setValue(pair.getFirst());
            }
        }, new Consumer<Throwable>() {
            @Override
            public void accept(Throwable throwable) throws Exception {
                SLogs.d(throwable.getMessage());
            }
        });
    }

    private void loadDirentsFromDb(Account account, NavContext context, boolean isForce) {
        String repoId = context.getRepoModel().repo_id;
        String parentDir = context.getNavPath();

        Single<List<DirentModel>> singleDB = AppDatabase.getInstance().direntDao().getAllByParentPath(repoId, parentDir);
        addSingleDisposable(singleDB, new Consumer<List<DirentModel>>() {
            @Override
            public void accept(List<DirentModel> direntModels) throws Exception {
                if (!CollectionUtils.isEmpty(direntModels)) {
                    getObjsListLiveData().setValue(parseDirentsWithLocal(direntModels));

                    if (isForce) {
                        loadDirentsFromNet(account, context);
                    }
                } else {
                    loadDirentsFromNet(account, context);
                }
            }
        });
    }

    private void loadDirentsFromNet(Account account, NavContext context) {
        getRefreshLiveData().setValue(true);

        String repoId = context.getRepoModel().repo_id;
        String parentDir = context.getNavPath();

        Single<DirentWrapperModel> singleNet = IO.getSingleton().execute(RepoService.class).getDirents(repoId, parentDir);
        addSingleDisposable(singleNet, new Consumer<DirentWrapperModel>() {
            @Override
            public void accept(DirentWrapperModel direntWrapperModel) throws Exception {
                Pair<List<BaseModel>, List<DirentModel>> pair = parseDirents(
                        direntWrapperModel.dirent_list,
                        direntWrapperModel.dir_id,
                        account.email,
                        context.getRepoModel());

                if (null == pair) {
                    getObjsListLiveData().setValue(null);
                    getRefreshLiveData().setValue(false);
                    return;
                }

                Completable completableDelete = AppDatabase.getInstance().direntDao().deleteAllByPath(repoId, parentDir);
                Completable completableInsert = AppDatabase.getInstance().direntDao().insertAll(pair.getSecond());
                Completable completable = Completable.concat(CollectionUtils.newArrayList(completableDelete, completableInsert));

                addCompletableDisposable(completable, new Action() {
                    @Override
                    public void run() throws Exception {
                        SLogs.d("Dirents本地数据库已更新");
                    }
                });

                getObjsListLiveData().setValue(pair.getFirst());
                getRefreshLiveData().setValue(false);
            }
        }, new Consumer<Throwable>() {
            @Override
            public void accept(Throwable throwable) throws Exception {
                getRefreshLiveData().setValue(false);
                getExceptionLiveData().setValue(new Pair<>(400, SeafException.networkException));
                String msg = getErrorMsgByThrowable(throwable);
                ToastUtils.showLong(msg);
            }
        });
    }

    private Pair<List<BaseModel>, List<RepoModel>> parseRepos(List<RepoModel> list, String email) {
        if (CollectionUtils.isEmpty(list)) {
            return null;
        }
        for (int i = 0; i < list.size(); i++) {
            list.get(i).related_account_email = email;
        }

        List<BaseModel> newRvList = CollectionUtils.newArrayList();
        List<RepoModel> newDbList = CollectionUtils.newArrayList();

        TreeMap<String, List<RepoModel>> treeMap = groupRepos(list);

        //mine
        List<RepoModel> mineList = treeMap.get("mine");
        if (!CollectionUtils.isEmpty(mineList)) {
            newRvList.add(new GroupItemModel(R.string.personal));
            for (RepoModel repoModel : mineList) {
                repoModel.last_modified_long = Times.convertMtime2Long(repoModel.last_modified);
            }

            List<RepoModel> sortedList = sortRepos(mineList);
            newRvList.addAll(sortedList);
            newDbList.addAll(mineList);
        }

        //shared
        List<RepoModel> sharedList = treeMap.get("shared");
        if (!CollectionUtils.isEmpty(sharedList)) {
            newRvList.add(new GroupItemModel(R.string.shared));
            for (RepoModel repoModel : sharedList) {
                repoModel.last_modified_long = Times.convertMtime2Long(repoModel.last_modified);
            }

            List<RepoModel> sortedList = sortRepos(sharedList);
            newRvList.addAll(sortedList);
            newDbList.addAll(sharedList);
        }

        for (String key : treeMap.keySet()) {
            if (TextUtils.equals(key, "mine")) {
            } else if (TextUtils.equals(key, "shared")) {
            } else {
                List<RepoModel> groupList = treeMap.get(key);
                if (!CollectionUtils.isEmpty(groupList)) {
                    newRvList.add(new GroupItemModel(key));
                    for (RepoModel repoModel : groupList) {
                        repoModel.last_modified_long = Times.convertMtime2Long(repoModel.last_modified);
                    }

                    List<RepoModel> sortedList = sortRepos(groupList);
                    newRvList.addAll(sortedList);
                    newDbList.addAll(groupList);
                }
            }
        }
        return new Pair<>(newRvList, newDbList);
    }

    private Pair<List<BaseModel>, List<DirentModel>> parseDirents(List<DirentModel> list, String dir_id, String email, RepoModel repoModel) {
        if (CollectionUtils.isEmpty(list)) {
            return null;
        }

        TreeMap<String, List<DirentModel>> treeMap = groupDirents(list);
        List<DirentModel> dirModels = treeMap.get("dir");
        List<DirentModel> fileModels = treeMap.get("file");

        List<DirentModel> newDbList = new ArrayList<>();
        long now = TimeUtils.getNowMills();
        if (!CollectionUtils.isEmpty(dirModels)) {
            for (int i = 0; i < dirModels.size(); i++) {
                //
                dirModels.get(i).last_sync_time = now;
                dirModels.get(i).dir_id = dir_id;
                dirModels.get(i).related_account_email = email;
                dirModels.get(i).repo_id = repoModel.repo_id;
                dirModels.get(i).repo_name = repoModel.repo_name;
                dirModels.get(i).full_path = dirModels.get(i).parent_dir + dirModels.get(i).name;
                dirModels.get(i).hash_path = EncryptUtils.encryptMD5ToString(dirModels.get(i).repo_id + list.get(i).full_path);
            }
            newDbList.addAll(sortDirents(dirModels));
        }

        if (!CollectionUtils.isEmpty(fileModels)) {
            for (int i = 0; i < fileModels.size(); i++) {
                //
                fileModels.get(i).repo_id = repoModel.repo_id;
                fileModels.get(i).repo_name = repoModel.repo_name;
                fileModels.get(i).last_sync_time = now;
                fileModels.get(i).dir_id = dir_id;
                fileModels.get(i).related_account_email = email;
                fileModels.get(i).full_path = fileModels.get(i).parent_dir + fileModels.get(i).name;
                fileModels.get(i).hash_path = EncryptUtils.encryptMD5ToString(repoModel.repo_id + fileModels.get(i).full_path);
            }
            newDbList.addAll(sortDirents(fileModels));
        }

        List<BaseModel> newRvList = new ArrayList<>(newDbList);

        return new Pair<>(newRvList, newDbList);
    }

    private List<BaseModel> parseDirentsWithLocal(List<DirentModel> list) {
        if (CollectionUtils.isEmpty(list)) {
            return null;
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


    private List<RepoModel> sortRepos(List<RepoModel> repos) {
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

    private List<DirentModel> sortDirents(List<DirentModel> list) {
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
                        return o1.mtime < o2.mtime ? -1 : 1;
                    }
                }).collect(Collectors.toList());
                break;
            case 3: // sort by last modified time, descending
                newList = list.stream().sorted(new Comparator<DirentModel>() {
                    @Override
                    public int compare(DirentModel o1, DirentModel o2) {
                        return o1.mtime > o2.mtime ? -1 : 1;
                    }
                }).collect(Collectors.toList());
                break;
        }
        return newList;
    }

    public TreeMap<String, List<RepoModel>> groupRepos(List<RepoModel> repos) {
        TreeMap<String, List<RepoModel>> map = new TreeMap<String, List<RepoModel>>();
        for (RepoModel repo : repos) {
            if (TextUtils.equals(repo.type, "group")) {
                List<RepoModel> l = map.computeIfAbsent(repo.group_name, k -> Lists.newArrayList());
                l.add(repo);
            } else {
                List<RepoModel> l = map.computeIfAbsent(repo.type, k -> Lists.newArrayList());
                l.add(repo);
            }
        }
        return map;
    }

    public TreeMap<String, List<DirentModel>> groupDirents(List<DirentModel> list) {
        TreeMap<String, List<DirentModel>> map = new TreeMap<String, List<DirentModel>>();
        for (DirentModel repo : list) {
            List<DirentModel> l = map.computeIfAbsent(repo.type, k -> Lists.newArrayList());
            l.add(repo);
        }
        return map;
    }

    private final StorageManager storageManager = StorageManager.getInstance();

    private File getFileForReposCache() {
        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        String filename = "repos-" + (account.server + account.email).hashCode() + ".dat";
        return new File(storageManager.getJsonCacheDir(), filename);
    }

    private File getFileForDirentCache(String dirID) {
        String filename = "dirent-" + dirID + ".dat";
        return new File(storageManager.getJsonCacheDir() + "/" + filename);
    }

    private File getFileForBlockCache(String blockId) {
        String filename = "block-" + blockId + ".dat";
        return new File(storageManager.getTempDir() + "/" + filename);
    }


    //star
    public void star(String repoId, String path) {
        getRefreshLiveData().setValue(true);

        Map<String, String> requestDataMap = new HashMap<>();
        requestDataMap.put("repo_id", repoId);
        requestDataMap.put("path", path);
        Map<String, RequestBody> bodyMap = generateRequestBody(requestDataMap);

        Single<DirentMiniModel> single = IO.getSingleton().execute(RepoService.class).star(bodyMap);
        addSingleDisposable(single, new Consumer<DirentMiniModel>() {
            @Override
            public void accept(DirentMiniModel resultModel) throws Exception {
                getRefreshLiveData().setValue(false);

                getStarLiveData().setValue(true);
                ToastUtils.showLong(R.string.success);
            }
        }, new Consumer<Throwable>() {
            @Override
            public void accept(Throwable throwable) throws Exception {
                getRefreshLiveData().setValue(false);
                String errMsg = getErrorMsgByThrowable(throwable);
                ToastUtils.showLong(errMsg);
            }
        });
    }

    public void unStar(String repoId, String path) {
        getRefreshLiveData().setValue(true);

        Single<ResultModel> single = IO.getSingleton().execute(RepoService.class).unStar(repoId, path);
        addSingleDisposable(single, new Consumer<ResultModel>() {
            @Override
            public void accept(ResultModel resultModel) throws Exception {
                getRefreshLiveData().setValue(false);

                getStarLiveData().setValue(true);
                ToastUtils.showLong(R.string.success);
            }
        }, new Consumer<Throwable>() {
            @Override
            public void accept(Throwable throwable) throws Exception {
                getRefreshLiveData().setValue(false);
                String errMsg = getErrorMsgByThrowable(throwable);
                ToastUtils.showLong(errMsg);
            }
        });
    }
}
