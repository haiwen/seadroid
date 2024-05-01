package com.seafile.seadroid2.ui.dialog_fragment.viewmodel;

import android.text.TextUtils;

import androidx.lifecycle.MutableLiveData;

import com.seafile.seadroid2.ui.base.viewmodel.BaseViewModel;
import com.seafile.seadroid2.framework.data.model.dirents.FileCreateModel;
import com.seafile.seadroid2.framework.http.IO;
import com.seafile.seadroid2.ui.dialog_fragment.DialogService;

import java.util.HashMap;
import java.util.Map;

import io.reactivex.Single;
import io.reactivex.functions.Consumer;
import okhttp3.RequestBody;

public class RenameRepoViewModel extends BaseViewModel {
    private final MutableLiveData<String> actionLiveData = new MutableLiveData<>();
    private final MutableLiveData<FileCreateModel> renameFileLiveData = new MutableLiveData<>();

    public MutableLiveData<FileCreateModel> getRenameFileLiveData() {
        return renameFileLiveData;
    }

    public MutableLiveData<String> getActionLiveData() {
        return actionLiveData;
    }

    public void renameRepo(String repoName, String repoId) {

        if (TextUtils.isEmpty(repoName)) {
            return;
        }

        getRefreshLiveData().setValue(true);

        Map<String, String> requestDataMap = new HashMap<>();
        requestDataMap.put("repo_name", repoName);
        Map<String, RequestBody> bodyMap = generateRequestBody(requestDataMap);

        Single<String> single = IO.getInstanceWithLoggedIn().execute(DialogService.class).renameRepo(repoId, bodyMap);

        addSingleDisposable(single, new Consumer<String>() {
            @Override
            public void accept(String resultModel) throws Exception {
                getRefreshLiveData().setValue(false);

                getActionLiveData().setValue(resultModel);
            }
        }, new Consumer<Throwable>() {
            @Override
            public void accept(Throwable throwable) throws Exception {
                getRefreshLiveData().setValue(false);
                getActionLiveData().setValue(getErrorMsgByThrowable(throwable));
            }
        });
    }

    public void renameDir(String repoId, String curPath, String newName) {
        getRefreshLiveData().setValue(true);

        Map<String, String> requestDataMap = new HashMap<>();
        requestDataMap.put("operation", "rename");
        requestDataMap.put("newname", newName);
        Map<String, RequestBody> bodyMap = generateRequestBody(requestDataMap);

        Single<String> single = IO.getInstanceWithLoggedIn().execute(DialogService.class).renameDir(repoId, curPath, bodyMap);

        addSingleDisposable(single, new Consumer<String>() {
            @Override
            public void accept(String resultModel) throws Exception {
                getRefreshLiveData().setValue(false);

                getActionLiveData().setValue(resultModel);
            }
        }, new Consumer<Throwable>() {
            @Override
            public void accept(Throwable throwable) throws Exception {
                getRefreshLiveData().setValue(false);
                getActionLiveData().setValue(getErrorMsgByThrowable(throwable));
            }
        });
    }

    public void renameFile(String repoId, String curPath, String newName) {
        getRefreshLiveData().setValue(true);

        Map<String, String> requestDataMap = new HashMap<>();
        requestDataMap.put("operation", "rename");
        requestDataMap.put("newname", newName);
        Map<String, RequestBody> bodyMap = generateRequestBody(requestDataMap);

        Single<FileCreateModel> single = IO.getInstanceWithLoggedIn().execute(DialogService.class).renameFile(repoId, curPath, bodyMap);

        addSingleDisposable(single, new Consumer<FileCreateModel>() {
            @Override
            public void accept(FileCreateModel resultModel) throws Exception {
                getRefreshLiveData().setValue(false);

                getRenameFileLiveData().setValue(resultModel);
            }
        }, new Consumer<Throwable>() {
            @Override
            public void accept(Throwable throwable) throws Exception {
                getRefreshLiveData().setValue(false);
                String msg = getErrorMsgByThrowable(throwable);
                FileCreateModel model = new FileCreateModel();
                model.error_msg = msg;
                getRenameFileLiveData().setValue(model);
            }
        });
    }
}
