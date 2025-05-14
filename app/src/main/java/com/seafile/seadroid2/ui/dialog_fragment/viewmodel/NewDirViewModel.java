package com.seafile.seadroid2.ui.dialog_fragment.viewmodel;

import android.text.TextUtils;

import androidx.lifecycle.MutableLiveData;

import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.ui.base.viewmodel.BaseViewModel;
import com.seafile.seadroid2.framework.model.ResultModel;
import com.seafile.seadroid2.framework.model.dirents.FileCreateModel;
import com.seafile.seadroid2.framework.http.HttpIO;
import com.seafile.seadroid2.ui.dialog_fragment.DialogService;

import java.util.HashMap;
import java.util.Map;

import io.reactivex.Single;
import io.reactivex.functions.Consumer;
import okhttp3.RequestBody;

public class NewDirViewModel extends BaseViewModel {
    private final MutableLiveData<ResultModel> createDirLiveData = new MutableLiveData<>();
    private final MutableLiveData<FileCreateModel> createFileLiveData = new MutableLiveData<>();

    public MutableLiveData<ResultModel> getCreateDirLiveData() {
        return createDirLiveData;
    }

    public MutableLiveData<FileCreateModel> getCreateFileLiveData() {
        return createFileLiveData;
    }

    public void createNewDir(String p, String repo_id) {
        if (TextUtils.isEmpty(p)) {
            return;
        }

        getRefreshLiveData().setValue(true);

        Map<String, String> bodyMap = new HashMap<>();
        bodyMap.put("operation", "mkdir");

        Single<String> single = HttpIO.getCurrentInstance().execute(DialogService.class).createDir2(repo_id, p, bodyMap);
        addSingleDisposable(single, new Consumer<String>() {
            @Override
            public void accept(String resultModel) throws Exception {
                getRefreshLiveData().setValue(false);

                if (TextUtils.equals("success", resultModel)) {
                    ResultModel resultModel1 = new ResultModel();
                    resultModel1.success = true;
                    getCreateDirLiveData().setValue(resultModel1);
                } else {
                    getCreateDirLiveData().setValue(null);
                }
            }
        }, new Consumer<Throwable>() {
            @Override
            public void accept(Throwable throwable) throws Exception {
                getRefreshLiveData().setValue(false);

                SeafException seafException = getExceptionByThrowable(throwable);
                getSeafExceptionLiveData().setValue(seafException);
                getRefreshLiveData().setValue(false);
            }
        });
    }

    public void createNewFile(String filePathName, String repo_id) {
        if (TextUtils.isEmpty(filePathName)) {
            return;
        }

        getRefreshLiveData().setValue(true);

        Map<String, String> requestDataMap = new HashMap<>();
        requestDataMap.put("operation", "create");

        Map<String, RequestBody> bodyMap = genRequestBody(requestDataMap);

        Single<FileCreateModel> single = HttpIO.getCurrentInstance().execute(DialogService.class).createFile(repo_id, filePathName, bodyMap);
        addSingleDisposable(single, new Consumer<FileCreateModel>() {
            @Override
            public void accept(FileCreateModel resultModel) throws Exception {
                getRefreshLiveData().setValue(false);

                if (resultModel != null) {
                    getCreateFileLiveData().setValue(resultModel);
                }
            }
        }, new Consumer<Throwable>() {
            @Override
            public void accept(Throwable throwable) throws Exception {
                getRefreshLiveData().setValue(false);

                SeafException seafException = getExceptionByThrowable(throwable);
                getSeafExceptionLiveData().setValue(seafException);
                getRefreshLiveData().setValue(false);
            }
        });
    }
}
