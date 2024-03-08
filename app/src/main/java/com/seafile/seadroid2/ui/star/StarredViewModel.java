package com.seafile.seadroid2.ui.star;

import androidx.lifecycle.MutableLiveData;

import com.blankj.utilcode.util.ToastUtils;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.ui.base.viewmodel.BaseViewModel;
import com.seafile.seadroid2.io.http.IO;
import com.seafile.seadroid2.data.model.ResultModel;
import com.seafile.seadroid2.data.model.star.StarredModel;
import com.seafile.seadroid2.data.model.star.StarredWrapperModel;

import java.util.List;

import io.reactivex.Single;
import io.reactivex.functions.Consumer;
import kotlin.Pair;

public class StarredViewModel extends BaseViewModel {
    private final MutableLiveData<List<StarredModel>> listLiveData = new MutableLiveData<>();
    private final MutableLiveData<Pair<String, ResultModel>> UnStarredResultLiveData = new MutableLiveData<>();

    public MutableLiveData<List<StarredModel>> getListLiveData() {
        return listLiveData;
    }

    public MutableLiveData<Pair<String, ResultModel>> getUnStarredResultLiveData() {
        return UnStarredResultLiveData;
    }

    public void loadData() {
        getRefreshLiveData().setValue(true);
        Single<StarredWrapperModel> flowable = IO.getSingleton().execute(StarredService.class).getStarItems();
        addSingleDisposable(flowable, new Consumer<StarredWrapperModel>() {
            @Override
            public void accept(StarredWrapperModel starredWrapperModel) throws Exception {
                getRefreshLiveData().setValue(false);

                if (starredWrapperModel == null) {
                    return;
                }

                getListLiveData().setValue(starredWrapperModel.starred_item_list);
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

    public void unStarItem(String repoId, String path) {
        Single<ResultModel> flowable = IO.getSingleton().execute(StarredService.class).unStarItem(repoId, path);
        addSingleDisposable(flowable, new Consumer<ResultModel>() {
            @Override
            public void accept(ResultModel resultModel) throws Exception {
                getUnStarredResultLiveData().setValue(new Pair<>(path, resultModel));
            }
        });
    }

}
