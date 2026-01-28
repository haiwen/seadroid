package com.seafile.seadroid2.ui.repo.repo_view;

import androidx.lifecycle.MutableLiveData;

import com.blankj.utilcode.util.CollectionUtils;
import com.seafile.seadroid2.baseviewmodel.BaseViewModel;
import com.seafile.seadroid2.framework.http.HttpIO;
import com.seafile.seadroid2.framework.model.repo.views.RepoViewWrapperModel;
import com.seafile.seadroid2.framework.model.repo.views.RepoViewModel;
import com.seafile.seadroid2.ui.repo.RepoService;

import java.util.List;

import io.reactivex.Single;
import io.reactivex.functions.Consumer;

public class RepoViewViewModel extends BaseViewModel {
    private final MutableLiveData<List<RepoViewModel>> repoViewsLiveData = new MutableLiveData<>();
    private final MutableLiveData<List<RepoViewModel>> repoTagsLiveData = new MutableLiveData<>();


    public MutableLiveData<List<RepoViewModel>> getRepoViewsLiveData() {
        return repoViewsLiveData;
    }

    public void loadRepoViews(String repoId) {
        Single<RepoViewWrapperModel> vSingle = HttpIO.getCurrentInstance().execute(RepoService.class).getRepoViews(repoId);
        addSingleDisposable(vSingle, new Consumer<RepoViewWrapperModel>() {
            @Override
            public void accept(RepoViewWrapperModel repoViewWrapperModel) throws Exception {
                getRepoViewsLiveData().setValue(repoViewWrapperModel.views);
            }
        }, new Consumer<Throwable>() {
            @Override
            public void accept(Throwable throwable) throws Exception {
                getRepoViewsLiveData().setValue(CollectionUtils.newArrayList());
            }
        });
    }
}
