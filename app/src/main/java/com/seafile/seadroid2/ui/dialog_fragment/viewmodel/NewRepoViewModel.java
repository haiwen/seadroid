package com.seafile.seadroid2.ui.dialog_fragment.viewmodel;

import android.text.TextUtils;

import androidx.lifecycle.MutableLiveData;

import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.ui.base.viewmodel.BaseViewModel;
import com.seafile.seadroid2.framework.db.entities.RepoModel;
import com.seafile.seadroid2.framework.http.HttpIO;
import com.seafile.seadroid2.ui.dialog_fragment.DialogService;

import java.util.HashMap;
import java.util.Map;

import io.reactivex.Single;
import io.reactivex.functions.Consumer;
import okhttp3.RequestBody;

public class NewRepoViewModel extends BaseViewModel {
    private final MutableLiveData<RepoModel> createRepoLiveData = new MutableLiveData<>();

    public MutableLiveData<RepoModel> getCreateRepoLiveData() {
        return createRepoLiveData;
    }

    public void createNewRepo(String repoName, String description, String password) {
        if (TextUtils.isEmpty(repoName)) {
            return;
        }

        getRefreshLiveData().setValue(true);

        Map<String, String> requestDataMap = new HashMap<>();
        requestDataMap.put("name", repoName);

        if (!description.isEmpty()) {
            requestDataMap.put("desc", description);
        }
        if (!TextUtils.isEmpty(password)) {
            requestDataMap.put("passwd", password);
        }
        Map<String, RequestBody> bodyMap = genRequestBody(requestDataMap);
        Single<RepoModel> single = HttpIO.getCurrentInstance().execute(DialogService.class).createRepo(bodyMap);
        addSingleDisposable(single, new Consumer<RepoModel>() {
            @Override
            public void accept(RepoModel repoModel) throws Exception {

                getRefreshLiveData().setValue(false);

                getCreateRepoLiveData().setValue(repoModel);
            }
        }, new Consumer<Throwable>() {
            @Override
            public void accept(Throwable throwable) throws Exception {
                SeafException seafException = getSeafExceptionByThrowable(throwable);
                getSeafExceptionLiveData().setValue(seafException);
                getRefreshLiveData().setValue(false);
            }
        });
    }
}
