package com.seafile.seadroid2.ui.activities;

import androidx.lifecycle.MutableLiveData;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.NetworkUtils;
import com.blankj.utilcode.util.ToastUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.framework.data.db.AppDatabase;
import com.seafile.seadroid2.framework.data.db.entities.RepoModel;
import com.seafile.seadroid2.ui.base.viewmodel.BaseViewModel;
import com.seafile.seadroid2.enums.OpType;
import com.seafile.seadroid2.framework.http.IO;
import com.seafile.seadroid2.framework.data.model.activities.ActivityModel;
import com.seafile.seadroid2.framework.data.model.activities.ActivityWrapperModel;
import com.seafile.seadroid2.framework.util.SLogs;

import java.util.List;

import io.reactivex.Single;
import io.reactivex.functions.Consumer;
import kotlin.Pair;

public class ActivityViewModel extends BaseViewModel {
    private final MutableLiveData<List<ActivityModel>> listLiveData = new MutableLiveData<>();

    public MutableLiveData<List<ActivityModel>> getListLiveData() {
        return listLiveData;
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

    public void loadAllData(int page) {
        getRefreshLiveData().setValue(true);
        Single<ActivityWrapperModel> flowable = IO.getInstanceWithLoggedIn().execute(ActivityService.class).getActivities(page);
        addSingleDisposable(flowable, new Consumer<ActivityWrapperModel>() {
            @Override
            public void accept(ActivityWrapperModel wrapperModel) throws Exception {
                getRefreshLiveData().setValue(false);

                if (wrapperModel == null) {
                    return;
                }
                for (ActivityModel event : wrapperModel.events) {
                    switch (event.op_type) {
                        case "create":
                            event.opType = OpType.CREATE;
                            break;
                        case "edit":
                            event.opType = OpType.EDIT;
                            break;
                        case "rename":
                            event.opType = OpType.RENAME;
                            break;
                        case "delete":
                            event.opType = OpType.DELETE;
                            break;
                        case "restore":
                            event.opType = OpType.RESTORE;
                            break;
                        case "move":
                            event.opType = OpType.MOVE;
                            break;
                        case "update":
                            event.opType = OpType.UPDATE;
                            break;
                        case "public":
                            event.opType = OpType.PUBLISH;
                            break;
                    }
                }
                getListLiveData().setValue(wrapperModel.events);
            }
        }, new Consumer<Throwable>() {
            @Override
            public void accept(Throwable throwable) throws Exception {
                getRefreshLiveData().setValue(false);
                SeafException seafException = getExceptionByThrowable(throwable);

                if (seafException == SeafException.remoteWipedException) {
                    //post a request
                    completeRemoteWipe();
                }

                getSeafExceptionLiveData().setValue(seafException);
            }
        });
    }
}
