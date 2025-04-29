package com.seafile.seadroid2.ui.search;

import android.text.TextUtils;

import androidx.lifecycle.MutableLiveData;

import com.blankj.utilcode.util.CollectionUtils;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.enums.ItemPositionEnum;
import com.seafile.seadroid2.framework.db.AppDatabase;
import com.seafile.seadroid2.framework.db.entities.RepoModel;
import com.seafile.seadroid2.framework.model.search.SearchModel;
import com.seafile.seadroid2.framework.model.search.SearchWrapperModel;
import com.seafile.seadroid2.framework.http.HttpIO;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.ui.base.viewmodel.BaseViewModel;
import com.seafile.seadroid2.ui.repo.RepoService;

import java.util.List;

import io.reactivex.Single;
import io.reactivex.SingleSource;
import io.reactivex.functions.Consumer;
import io.reactivex.functions.Function;

public class SearchViewModel extends BaseViewModel {

    private final MutableLiveData<List<SearchModel>> mListLiveData = new MutableLiveData<>();

    public MutableLiveData<List<SearchModel>> getSearchListLiveData() {
        return mListLiveData;
    }

    public void searchNext(String q, int pageNo, int pageSize) {
        if (TextUtils.isEmpty(q)) {
            return;
        }

        getRefreshLiveData().setValue(true);

        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        Single<SearchWrapperModel> single = HttpIO.getCurrentInstance().execute(SearchService.class).search("all", q, "all", pageNo, pageSize);
        addSingleDisposable(single, new Consumer<SearchWrapperModel>() {
            @Override
            public void accept(SearchWrapperModel searchWrapperModel) throws Exception {

                if (searchWrapperModel == null || searchWrapperModel.results == null) {
                    getSearchListLiveData().setValue(CollectionUtils.newArrayList());
                    getRefreshLiveData().setValue(false);
                    return;
                }

                List<SearchModel> results = searchWrapperModel.results;

                for (SearchModel result : results) {
                    result.related_account = account.getSignature();
                }
                //calculate item_position
                if (CollectionUtils.isEmpty(results)) {

                } else if (results.size() == 1) {
                    results.get(0).item_position = ItemPositionEnum.ALL;
                } else if (results.size() == 2) {
                    results.get(0).item_position = ItemPositionEnum.START;
                    results.get(1).item_position = ItemPositionEnum.END;
                } else {
                    results.get(0).item_position = ItemPositionEnum.START;
                    results.get(results.size() - 1).item_position = ItemPositionEnum.END;
                }

                getSearchListLiveData().setValue(results);
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

    public void getRepoModel(String repoId, Consumer<RepoModel> consumer) {
        //from db
        Single<List<RepoModel>> singleDb = AppDatabase.getInstance().repoDao().getRepoById(repoId);

        Single<RepoModel> singleNet = HttpIO.getCurrentInstance().execute(RepoService.class).getRepoInfo(repoId);

        Single<RepoModel> single = singleDb.flatMap(new Function<List<RepoModel>, SingleSource<RepoModel>>() {
            @Override
            public SingleSource<RepoModel> apply(List<RepoModel> repoModels) throws Exception {
                if (CollectionUtils.isEmpty(repoModels)) {
                    return singleNet.flatMap(new Function<RepoModel, SingleSource<RepoModel>>() {
                        @Override
                        public SingleSource<RepoModel> apply(RepoModel repoModel) throws Exception {

                            //insert
                            AppDatabase.getInstance().repoDao().insert(repoModel);

                            return Single.just(repoModel);
                        }
                    });
                } else {
                    return Single.just(repoModels.get(0));
                }
            }
        });

        addSingleDisposable(single, new Consumer<RepoModel>() {
            @Override
            public void accept(RepoModel repoModel) throws Exception {
                if (consumer != null) {
                    consumer.accept(repoModel);
                }
            }
        }, new Consumer<Throwable>() {
            @Override
            public void accept(Throwable throwable) throws Exception {
                SLogs.e(throwable);
            }
        });
    }

}
