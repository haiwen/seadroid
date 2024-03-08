package com.seafile.seadroid2.ui.main;

import android.text.TextUtils;

import androidx.fragment.app.Fragment;
import androidx.lifecycle.MutableLiveData;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.ToastUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.data.model.repo.DirentWrapperModel;
import com.seafile.seadroid2.ui.base.viewmodel.BaseViewModel;
import com.seafile.seadroid2.config.Constants;
import com.seafile.seadroid2.context.NavContext;
import com.seafile.seadroid2.data.ServerInfo;
import com.seafile.seadroid2.data.db.AppDatabase;
import com.seafile.seadroid2.data.db.entities.DirentModel;
import com.seafile.seadroid2.data.db.entities.ObjsModel;
import com.seafile.seadroid2.data.db.entities.RepoModel;
import com.seafile.seadroid2.data.model.repo.RepoWrapperModel;
import com.seafile.seadroid2.data.model.server.ServerInfoModel;
import com.seafile.seadroid2.ui.repo.RepoService;
import com.seafile.seadroid2.io.http.IO;
import com.seafile.seadroid2.ui.activities.AllActivitiesFragment;
import com.seafile.seadroid2.ui.repo.RepoQuickFragment;
import com.seafile.seadroid2.ui.star.StarredQuickFragment;
import com.seafile.seadroid2.util.SLogs;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import io.reactivex.Single;
import io.reactivex.functions.Consumer;
import kotlin.Pair;

public class MainViewModel extends BaseViewModel {
    private final MutableLiveData<Pair<String, String>> OnNewFileDownloadLiveData = new MutableLiveData<>();
    private final MutableLiveData<Integer> OnResortListLiveData = new MutableLiveData<>();

    //force refresh repo/dirents
    private final MutableLiveData<Boolean> OnForceRefreshRepoListLiveData = new MutableLiveData<>();

    //show swipeRefresh in Repo Fragment
    private final MutableLiveData<Boolean> OnShowRefreshLoadingInRepoLiveData = new MutableLiveData<>();


    private final MutableLiveData<Boolean> OnNavChangeListenerLiveData = new MutableLiveData<>();


    private final MutableLiveData<ServerInfo> ServerInfoLiveData = new MutableLiveData<>();

    public MutableLiveData<Pair<String, String>> getOnNewFileDownloadLiveData() {
        return OnNewFileDownloadLiveData;
    }

    public MutableLiveData<Boolean> getOnForceRefreshRepoListLiveData() {
        return OnForceRefreshRepoListLiveData;
    }

    public MutableLiveData<Boolean> getOnShowRefreshLoadingInRepoLiveData() {
        return OnShowRefreshLoadingInRepoLiveData;
    }

    public MutableLiveData<Boolean> getOnNavContextChangeListenerLiveData() {
        return OnNavChangeListenerLiveData;
    }


    public MutableLiveData<ServerInfo> getServerInfoLiveData() {
        return ServerInfoLiveData;
    }

    public MutableLiveData<Integer> getOnResortListLiveData() {
        return OnResortListLiveData;
    }

    private NavContext navContext = null;

    public NavContext getNavContext() {
        if (navContext == null) {
            navContext = new NavContext();
        }
        return navContext;
    }

    private final List<Fragment> fragments = new ArrayList<>();

    public List<Fragment> getFragments() {
        return fragments;
    }

    public MainViewModel() {
        getNavContext();

        fragments.add(RepoQuickFragment.newInstance());
        fragments.add(StarredQuickFragment.newInstance());
        fragments.add(AllActivitiesFragment.newInstance());
    }

    public void getServerInfo() {
        Single<ServerInfoModel> single = IO.getSingleton().execute(MainService.class).getServerInfo();
        addSingleDisposable(single, new Consumer<ServerInfoModel>() {
            @Override
            public void accept(ServerInfoModel serverInfo) throws Exception {
                Account account = SupportAccountManager.getInstance().getCurrentAccount();
                if (account == null) {
                    return;
                }

                ServerInfo serverInfo1 = new ServerInfo(account.server, serverInfo.version, serverInfo.getFeaturesString());
                SupportAccountManager.getInstance().setServerInfo(account, serverInfo1);

                getServerInfoLiveData().setValue(serverInfo1);
            }
        });
    }

    public void requestRepoModel(String repoId, Consumer<RepoModel> consumer) {
        getOnShowRefreshLoadingInRepoLiveData().setValue(true);

        //from db
        Single<List<RepoModel>> singleDb = AppDatabase.getInstance().repoDao().getRepoById(repoId);
        addSingleDisposable(singleDb, new Consumer<List<RepoModel>>() {
            @Override
            public void accept(List<RepoModel> repoModels) throws Exception {
                if (consumer != null) {
                    if (CollectionUtils.isEmpty(repoModels)) {
                        //no data in sqlite, request RepoApi again
                        requestRepoModelFromServer(repoId, consumer);
                    } else {
                        consumer.accept(repoModels.get(0));
                        getOnShowRefreshLoadingInRepoLiveData().setValue(false);
                    }
                } else {
                    getOnShowRefreshLoadingInRepoLiveData().setValue(false);
                }
            }
        }, new Consumer<Throwable>() {
            @Override
            public void accept(Throwable throwable) throws Exception {
                getOnShowRefreshLoadingInRepoLiveData().setValue(false);
                SLogs.e(throwable);
            }
        });
    }

    private void requestRepoModelFromServer(String repoId, Consumer<RepoModel> consumer) {
        //from net
        Single<RepoWrapperModel> singleNet = IO.getSingleton().execute(RepoService.class).getRepos();
        addSingleDisposable(singleNet, new Consumer<RepoWrapperModel>() {
            @Override
            public void accept(RepoWrapperModel repoWrapperModel) throws Exception {
                getOnShowRefreshLoadingInRepoLiveData().setValue(false);

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
                getOnShowRefreshLoadingInRepoLiveData().setValue(false);
                String msg = getErrorMsgByThrowable(throwable);
                ToastUtils.showLong(msg);
            }
        });
    }

    public void getObjFromDB(String repoName, Consumer<ObjsModel> consumer) {
        Single<List<ObjsModel>> single = AppDatabase.getInstance().objDao().getByPath(repoName, Constants.ObjType.REPO);
        addSingleDisposable(single, new Consumer<List<ObjsModel>>() {
            @Override
            public void accept(List<ObjsModel> objsModels) throws Exception {
                if (!CollectionUtils.isEmpty(objsModels)) {
                    consumer.accept(objsModels.get(0));
                }
            }
        }, new Consumer<Throwable>() {
            @Override
            public void accept(Throwable throwable) throws Exception {
                SLogs.e(throwable);
            }
        });
    }

    public void getDirentsFromLocal(String repoId, String parent_dir, Consumer<List<DirentModel>> consumer) {
        Single<List<DirentModel>> singleDB = AppDatabase.getInstance().direntDao().getAllByParentPath(repoId, parent_dir);
        addSingleDisposable(singleDB, new Consumer<List<DirentModel>>() {
            @Override
            public void accept(List<DirentModel> direntModels) throws Exception {
                if (consumer != null) {
                    consumer.accept(direntModels);
                }
            }
        });
    }

    public void getDirentsFromServer(String repoId, String parent_dir, Consumer<List<DirentModel>> consumer) {
        Single<DirentWrapperModel> singleServer = IO.getSingleton().execute(RepoService.class).getDirents(repoId, parent_dir);
        addSingleDisposable(singleServer, new Consumer<DirentWrapperModel>() {
            @Override
            public void accept(DirentWrapperModel direntWrapperModel) throws Exception {
                if (consumer != null) {
                    consumer.accept(direntWrapperModel.dirent_list);
                }
            }
        });
    }

}
