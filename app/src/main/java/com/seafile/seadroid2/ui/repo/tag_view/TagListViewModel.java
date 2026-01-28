package com.seafile.seadroid2.ui.repo.tag_view;

import androidx.lifecycle.MutableLiveData;

import com.blankj.utilcode.util.CollectionUtils;
import com.seafile.seadroid2.baseviewmodel.BaseViewModel;
import com.seafile.seadroid2.framework.http.HttpIO;
import com.seafile.seadroid2.framework.model.repo.tags.TagResultModel;
import com.seafile.seadroid2.framework.model.repo.tags.TagWrapperModel;
import com.seafile.seadroid2.ui.repo.RepoService;

import java.util.List;

import io.reactivex.Single;
import io.reactivex.functions.Consumer;

public class TagListViewModel extends BaseViewModel {
    private final MutableLiveData<List<TagResultModel>> repoTagsLiveData = new MutableLiveData<>();


    public MutableLiveData<List<TagResultModel>> getRepoTagsLiveData() {
        return repoTagsLiveData;
    }

    public void loadRepoViews(String repoId) {

        Single<TagWrapperModel> vSingle = HttpIO.getCurrentInstance().execute(RepoService.class).getRepoTags(repoId);
        addSingleDisposable(vSingle, new Consumer<TagWrapperModel>() {
            @Override
            public void accept(TagWrapperModel wrapperModel) throws Exception {
                getRepoTagsLiveData().setValue(wrapperModel.results);
            }
        }, new Consumer<Throwable>() {
            @Override
            public void accept(Throwable throwable) throws Exception {
                getRepoTagsLiveData().setValue(CollectionUtils.newArrayList());
            }
        });
    }
}
