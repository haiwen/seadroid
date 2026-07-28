package com.seafile.seadroid2.ui.selector;

import android.util.Pair;

import androidx.annotation.Nullable;
import androidx.lifecycle.MutableLiveData;

import com.blankj.utilcode.util.CollectionUtils;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.baseviewmodel.BaseViewModel;
import com.seafile.seadroid2.framework.db.AppDatabase;
import com.seafile.seadroid2.framework.db.entities.PermissionEntity;
import com.seafile.seadroid2.framework.db.entities.RepoModel;
import com.seafile.seadroid2.framework.util.Utils;

import java.util.List;
import java.util.Stack;

import io.reactivex.Single;
import io.reactivex.SingleSource;
import io.reactivex.functions.Consumer;

public class OpSelectorViewModel extends BaseViewModel {

    /**
     * <code>
     * 0:repo_id/repo_name
     * 1:path1
     * 2:path2
     * <code/>
     */
    private final Stack<String> navStack = new Stack<>();
    private final MutableLiveData<String> nav_path_context_live_data = new MutableLiveData<>();

    public MutableLiveData<String> getStarredNavPathContext() {
        return nav_path_context_live_data;
    }

    public boolean isNavEmpty() {
        return navStack.isEmpty();
    }

    @Nullable
    public Pair<String, String> getStarredNavContextRepoIdAndName() {
        if (navStack.isEmpty()) {
            return null;
        }

        String repoIdAndName = navStack.get(0);
        String[] split = repoIdAndName.split("/");
        if (split.length != 2) {
            return null;
        }

        return new Pair<>(split[0], split[1]);
    }

    public void pushRepoAndName(String repoId, String repoName) {
        navStack.push(repoId + "/" + repoName);
        getStarredNavPathContext().setValue(getFullPath());
    }

    public String getStarredPath() {
        if (navStack.isEmpty()) {
            return "";
        } else {
            String p = "/";
            for (int i = 0; i < navStack.size(); i++) {
                if (i == 0) {
                    continue;
                }
                p = Utils.pathJoin(p, navStack.get(i));
            }
            return p;
        }
    }

    public void pushDir(String repoId, String repoName, String path) {
        if (navStack.isEmpty()) {
            navStack.push(repoId + "/" + repoName);
        }
        navStack.push(path);
        getStarredNavPathContext().setValue(getFullPath());
    }

    public void pop() {
        if (navStack.isEmpty()) {
            return;
        }
        navStack.pop();
        getStarredNavPathContext().setValue(getFullPath());
    }

    public void popAll() {
        navStack.clear();
        getStarredNavPathContext().setValue(getFullPath());
    }

    private String getFullPath() {
        if (navStack.isEmpty()) {
            return "";
        } else {
            StringBuilder sb = new StringBuilder();
            for (String s : navStack) {
                sb.append("/").append(s);
            }
            return sb.toString();
        }
    }


    public void getRepoModelAndPermissionEntity(Account account, String repoId, Consumer<Pair<RepoModel, PermissionEntity>> consumer) {
        Single<Pair<RepoModel, PermissionEntity>> r = getSingleForLoadRepoModelAndAllPermission(account, repoId);
        addSingleDisposable(r, new Consumer<android.util.Pair<RepoModel, PermissionEntity>>() {
            @Override
            public void accept(android.util.Pair<RepoModel, PermissionEntity> pair) throws Exception {
                if (consumer != null) {
                    consumer.accept(pair);
                }
            }
        });
    }

    /**
     * get the repoModel and repoMode‘s PermissionEntity from local, if not exist, get from remote.
     */
    private Single<android.util.Pair<RepoModel, PermissionEntity>> getSingleForLoadRepoModelAndAllPermission(Account account, String repoId) {
        Single<List<RepoModel>> repoSingle = AppDatabase.getInstance().repoDao().getRepoById(account.getSignature(), repoId);
        return repoSingle.flatMap(new io.reactivex.functions.Function<List<RepoModel>, SingleSource<Pair<RepoModel, PermissionEntity>>>() {
            @Override
            public SingleSource<android.util.Pair<RepoModel, PermissionEntity>> apply(List<RepoModel> repoModels) throws Exception {
                if (CollectionUtils.isEmpty(repoModels)) {
                    return Single.just(new android.util.Pair<>(null, null));
                }

                RepoModel repoModel = repoModels.get(0);
                if (!repoModel.isCustomPermission()) {
                    return Single.just(new android.util.Pair<>(repoModel, new PermissionEntity(repoId, repoModel.permission)));
                }

                Single<List<PermissionEntity>> pSingle = AppDatabase.getInstance().permissionDAO().getByRepoAndIdAsync(repoId, repoModel.getCustomPermissionNum());
                return pSingle.flatMap((io.reactivex.functions.Function<List<PermissionEntity>, SingleSource<android.util.Pair<RepoModel, PermissionEntity>>>) pList -> {
                    //no data in local db
                    if (CollectionUtils.isEmpty(pList)) {
                        return Single.just(new android.util.Pair<>(repoModel, new PermissionEntity(repoModel.repo_id, "r")));
                    }

                    //get first permission
                    return Single.just(new android.util.Pair<>(repoModel, pList.get(0)));
                });
            }
        });
    }

}
