package com.seafile.seadroid2.ui.search;

import androidx.lifecycle.MutableLiveData;

import com.seafile.seadroid2.data.model.search.SearchModel;
import com.seafile.seadroid2.data.model.search.SearchWrapperModel;
import com.seafile.seadroid2.io.http.IO;
import com.seafile.seadroid2.ui.base.viewmodel.BaseViewModel;

import java.util.List;

import io.reactivex.Single;
import io.reactivex.functions.Consumer;

public class SearchViewModel extends BaseViewModel {

    private MutableLiveData<List<SearchModel>> ListLiveData = new MutableLiveData<>();

    public MutableLiveData<List<SearchModel>> getListLiveData() {
        return ListLiveData;
    }

    public void loadNext(String q, int pageNo,int pageSize) {
        getRefreshLiveData().setValue(true);

        Single<SearchWrapperModel> single = IO.getSingleton().execute(SearchService.class).search("all", q, "all", pageNo, pageSize);
        addSingleDisposable(single, new Consumer<SearchWrapperModel>() {
            @Override
            public void accept(SearchWrapperModel searchWrapperModel) throws Exception {
                getListLiveData().setValue(searchWrapperModel.results);
                getRefreshLiveData().setValue(false);
            }
        }, new Consumer<Throwable>() {
            @Override
            public void accept(Throwable throwable) throws Exception {
                getSeafExceptionLiveData().setValue(getExceptionByThrowable(throwable));

                getRefreshLiveData().setValue(false);
            }
        });
    }
}
