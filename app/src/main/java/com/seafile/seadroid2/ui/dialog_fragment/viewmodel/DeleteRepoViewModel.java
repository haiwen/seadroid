package com.seafile.seadroid2.ui.dialog_fragment.viewmodel;

import android.text.TextUtils;

import androidx.lifecycle.MutableLiveData;

import com.seafile.seadroid2.ui.base.viewmodel.BaseViewModel;
import com.seafile.seadroid2.framework.data.model.ResultModel;
import com.seafile.seadroid2.framework.http.HttpIO;
import com.seafile.seadroid2.ui.dialog_fragment.DialogService;

import io.reactivex.Single;
import io.reactivex.functions.Consumer;

public class DeleteRepoViewModel extends BaseViewModel {
    private final MutableLiveData<ResultModel> ActionLiveData = new MutableLiveData<>();

    public MutableLiveData<ResultModel> getActionLiveData() {
        return ActionLiveData;
    }

    public void deleteRepo(String repoId) {
        getRefreshLiveData().setValue(true);

        Single<String> single = HttpIO.getCurrentInstance().execute(DialogService.class).deleteRepo(repoId);
        addSingleDisposable(single, new Consumer<String>() {
            @Override
            public void accept(String resultModel) throws Exception {
                getRefreshLiveData().setValue(false);

                ResultModel resultModel1 = new ResultModel();
                if (TextUtils.equals("success", resultModel)) {
                    resultModel1.success = true;
                } else {
                    resultModel1.error_msg = resultModel;
                }

                getActionLiveData().setValue(resultModel1);
            }
        }, new Consumer<Throwable>() {
            @Override
            public void accept(Throwable throwable) throws Exception {
                getRefreshLiveData().setValue(false);

                ResultModel resultModel = new ResultModel();
                resultModel.error_msg = getErrorMsgByThrowable(throwable);
                getActionLiveData().setValue(resultModel);
            }
        });
    }


}
