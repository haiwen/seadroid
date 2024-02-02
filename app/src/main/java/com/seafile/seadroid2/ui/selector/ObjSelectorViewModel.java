package com.seafile.seadroid2.ui.selector;

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
import com.seafile.seadroid2.ui.base.viewmodel.BaseViewModel;
import com.seafile.seadroid2.context.NavContext;
import com.seafile.seadroid2.data.db.AppDatabase;
import com.seafile.seadroid2.data.db.entities.DirentModel;
import com.seafile.seadroid2.data.db.entities.RepoModel;
import com.seafile.seadroid2.data.model.BaseModel;
import com.seafile.seadroid2.data.model.GroupItemModel;
import com.seafile.seadroid2.data.model.repo.DirentWrapperModel;
import com.seafile.seadroid2.data.model.repo.RepoWrapperModel;
import com.seafile.seadroid2.data.remote.api.RepoService;
import com.seafile.seadroid2.io.http.IO;
import com.seafile.seadroid2.util.SLogs;
import com.seafile.seadroid2.util.Times;
import com.seafile.seadroid2.util.sp.Sorts;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import java.util.TreeMap;
import java.util.stream.Collectors;

import io.reactivex.Single;
import io.reactivex.functions.Consumer;
import kotlin.Pair;

public class ObjSelectorViewModel extends BaseViewModel {
    private final MutableLiveData<List<BaseModel>> ObjsListLiveData = new MutableLiveData<>();

    public MutableLiveData<List<BaseModel>> getObjsListLiveData() {
        return ObjsListLiveData;
    }

    public void loadAccount() {
        List<Account> list = SupportAccountManager.getInstance().getSignedInAccountList();
        getObjsListLiveData().setValue(new ArrayList<>(list));
        getRefreshLiveData().setValue(false);
    }

    public void loadReposFromNet(Account account) {
        getRefreshLiveData().setValue(true);

        Single<RepoWrapperModel> singleNet = IO.getNewInstance(account.server,account.token).execute(RepoService.class).getRepos();
        addSingleDisposable(singleNet, new Consumer<RepoWrapperModel>() {
            @Override
            public void accept(RepoWrapperModel repoWrapperModel) throws Exception {
                if (repoWrapperModel == null || CollectionUtils.isEmpty(repoWrapperModel.repos)) {
                    getObjsListLiveData().setValue(null);
                    getRefreshLiveData().setValue(false);
                    return;
                }

                List<BaseModel> list = parseRepos(repoWrapperModel.repos, account.email);
                getObjsListLiveData().setValue(list);
                getRefreshLiveData().setValue(false);
            }
        }, new Consumer<Throwable>() {
            @Override
            public void accept(Throwable throwable) throws Exception {
                SLogs.d(throwable.getMessage());
            }
        });
    }

    public void loadDirentsFromNet(Account account, NavContext context) {
        getRefreshLiveData().setValue(true);

        String repoId = context.getRepoModel().repo_id;
        String parentDir = context.getNavPath();

        Single<DirentWrapperModel> singleNet = IO.getNewInstance(account.server,account.token).execute(RepoService.class).getDirents(repoId, parentDir);
        addSingleDisposable(singleNet, new Consumer<DirentWrapperModel>() {
            @Override
            public void accept(DirentWrapperModel direntWrapperModel) throws Exception {
                List<BaseModel> list = parseDirents(
                        direntWrapperModel.dirent_list,
                        direntWrapperModel.dir_id,
                        account.email,
                        context.getRepoModel());

                getObjsListLiveData().setValue(list);
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

    private List<BaseModel> parseRepos(List<RepoModel> list, String email) {
        if (CollectionUtils.isEmpty(list)) {
            return null;
        }
        for (int i = 0; i < list.size(); i++) {
            list.get(i).related_account_email = email;
        }

        List<BaseModel> newRvList = CollectionUtils.newArrayList();

        TreeMap<String, List<RepoModel>> treeMap = groupRepos(list);

        //mine
        List<RepoModel> mineList = treeMap.get("mine");
        if (!CollectionUtils.isEmpty(mineList)) {
            newRvList.add(new GroupItemModel(R.string.personal));

            List<RepoModel> l = new ArrayList<>();
            for (RepoModel repoModel : mineList) {
                repoModel.last_modified_long = Times.convertMtime2Long(repoModel.last_modified);

                //check permission
                if (repoModel.hasWritePermission()) {
                    l.add(repoModel);
                }
            }

            List<RepoModel> sortedList = sortRepos(l);
            newRvList.addAll(sortedList);
        }

        //shared
        List<RepoModel> sharedList = treeMap.get("shared");
        if (!CollectionUtils.isEmpty(sharedList)) {
            newRvList.add(new GroupItemModel(R.string.shared));

            List<RepoModel> l = new ArrayList<>();

            for (RepoModel repoModel : sharedList) {
                repoModel.last_modified_long = Times.convertMtime2Long(repoModel.last_modified);

                //check permission
                if (repoModel.hasWritePermission()) {
                    l.add(repoModel);
                }
            }

            List<RepoModel> sortedList = sortRepos(l);
            newRvList.addAll(sortedList);
        }

        for (String key : treeMap.keySet()) {
            if (TextUtils.equals(key, "mine")) {
            } else if (TextUtils.equals(key, "shared")) {
            } else {
                List<RepoModel> groupList = treeMap.get(key);
                if (!CollectionUtils.isEmpty(groupList)) {
                    newRvList.add(new GroupItemModel(key));

                    List<RepoModel> l = new ArrayList<>();

                    for (RepoModel repoModel : groupList) {
                        repoModel.last_modified_long = Times.convertMtime2Long(repoModel.last_modified);

                        //check permission
                        if (repoModel.hasWritePermission()) {
                            l.add(repoModel);
                        }
                    }

                    List<RepoModel> sortedList = sortRepos(l);
                    newRvList.addAll(sortedList);
                }
            }
        }
        return newRvList;
    }

    private List<BaseModel> parseDirents(List<DirentModel> list, String dir_id, String email, RepoModel repoModel) {
        if (CollectionUtils.isEmpty(list)) {
            return null;
        }

        TreeMap<String, List<DirentModel>> treeMap = groupDirents(list);
        List<DirentModel> dirModels = treeMap.get("dir");
        List<DirentModel> fileModels = treeMap.get("file");

        List<BaseModel> newRvList = new ArrayList<>();

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
            newRvList.addAll(sortDirents(dirModels));
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
            newRvList.addAll(sortDirents(fileModels));
        }


        return newRvList;
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

    public void requestRepoModel(String repoId, Consumer<RepoModel> consumer) {
        getRefreshLiveData().setValue(true);

        //from db
        Single<List<RepoModel>> singleDb = AppDatabase.getInstance().repoDao().getRepoById(repoId);
        addSingleDisposable(singleDb, new Consumer<List<RepoModel>>() {
            @Override
            public void accept(List<RepoModel> repoModels) throws Exception {
                if (consumer != null) {
                    if (CollectionUtils.isEmpty(repoModels)) {
                        //no data in sqlite, request RepoApi again
                        requestRepoModelFromNet(repoId, consumer);
                    } else {
                        consumer.accept(repoModels.get(0));
                        getRefreshLiveData().setValue(false);
                    }
                } else {
                    getRefreshLiveData().setValue(false);
                }
            }
        }, new Consumer<Throwable>() {
            @Override
            public void accept(Throwable throwable) throws Exception {
                getRefreshLiveData().setValue(false);
                SLogs.e(throwable);
            }
        });
    }

    private void requestRepoModelFromNet(String repoId, Consumer<RepoModel> consumer) {
        //from net
        Single<RepoWrapperModel> singleNet = IO.getSingleton().execute(RepoService.class).getRepos();
        addSingleDisposable(singleNet, new Consumer<RepoWrapperModel>() {
            @Override
            public void accept(RepoWrapperModel repoWrapperModel) throws Exception {
                getRefreshLiveData().setValue(false);

                if (repoWrapperModel == null || CollectionUtils.isEmpty(repoWrapperModel.repos)) {
                    ToastUtils.showLong(R.string.search_library_not_found);
                    return;
                }

                Optional<RepoModel> optionalRepoModel = repoWrapperModel.repos
                        .stream()
                        .filter(f -> TextUtils.equals(f.repo_id, repoId))
                        .findFirst();
                if (optionalRepoModel.isPresent()) {
                    if (consumer != null) {
                        consumer.accept(optionalRepoModel.get());
                    }
                } else {
                    ToastUtils.showLong(R.string.search_library_not_found);
                }
            }
        }, new Consumer<Throwable>() {
            @Override
            public void accept(Throwable throwable) throws Exception {
                getRefreshLiveData().setValue(false);
                String msg = getErrorMsgByThrowable(throwable);
                ToastUtils.showLong(msg);
            }
        });
    }
}
