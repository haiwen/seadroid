package com.seafile.seadroid2.ui.dialog_fragment.viewmodel;

import android.text.TextUtils;

import androidx.lifecycle.MutableLiveData;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.TimeUtils;
import com.seafile.seadroid2.ui.base.viewmodel.BaseViewModel;
import com.seafile.seadroid2.config.Constants;
import com.seafile.seadroid2.data.db.AppDatabase;
import com.seafile.seadroid2.data.model.ResultModel;
import com.seafile.seadroid2.data.db.entities.ObjsModel;
import com.seafile.seadroid2.data.db.entities.RepoModel;
import com.seafile.seadroid2.io.http.IO;
import com.seafile.seadroid2.ui.dialog_fragment.DialogService;
import com.seafile.seadroid2.util.SLogs;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import io.reactivex.Completable;
import io.reactivex.Single;
import io.reactivex.functions.Action;
import io.reactivex.functions.Consumer;
import okhttp3.RequestBody;

public class PasswordViewModel extends BaseViewModel {
    private final MutableLiveData<ResultModel> ActionLiveData = new MutableLiveData<>();

    public MutableLiveData<ResultModel> getActionLiveData() {
        return ActionLiveData;
    }

    public void setPassword(String repoId, String password) {
        if (TextUtils.isEmpty(password) || TextUtils.isEmpty(repoId)) {
            return;
        }

        getRefreshLiveData().setValue(true);

        Map<String, String> requestDataMap = new HashMap<>();
        requestDataMap.put("password", password);
        Map<String, RequestBody> bodyMap = generateRequestBody(requestDataMap);

        Single<ResultModel> single = IO.getSingleton().execute(DialogService.class).setPassword(repoId, bodyMap);
        addSingleDisposable(single, resultModel -> {

            long now = TimeUtils.getNowMills();
            now += 1000 * 60 * 60 * 24 * 1;//1 days

            //TODO 更新指定字段
            ObjsModel objsModel = new ObjsModel();
            objsModel.path = repoId;
            objsModel.decrypt_expire_time_long = now;
            objsModel.type = Constants.ObjType.REPO;

            Completable updateCompletable = AppDatabase.getInstance().objDao().insert(objsModel);
            addCompletableDisposable(updateCompletable, new Action() {
                @Override
                public void run() throws Exception {
                    getRefreshLiveData().setValue(false);
                    getActionLiveData().setValue(resultModel);
                }
            });


        }, throwable -> {
            getRefreshLiveData().setValue(false);

            ResultModel resultModel = new ResultModel();
            resultModel.error_msg = getErrorMsgByThrowable(throwable);
            getActionLiveData().setValue(resultModel);
        });
    }

    public void getRepoModelFromLocal(String repoId, Consumer<RepoModel> consumer) {
        Single<List<RepoModel>> singleDb = AppDatabase.getInstance().repoDao().getRepoById(repoId);
        addSingleDisposable(singleDb, new Consumer<List<RepoModel>>() {
            @Override
            public void accept(List<RepoModel> repoModels) throws Exception {
                if (consumer != null) {
                    if (CollectionUtils.isEmpty(repoModels)) {
                        //no data in sqlite
                    } else {
                        consumer.accept(repoModels.get(0));
                    }
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
