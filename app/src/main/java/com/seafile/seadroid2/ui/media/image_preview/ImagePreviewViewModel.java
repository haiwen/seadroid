package com.seafile.seadroid2.ui.media.image_preview;

import androidx.lifecycle.MutableLiveData;

import com.blankj.utilcode.util.ToastUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.data.db.AppDatabase;
import com.seafile.seadroid2.data.db.entities.DirentModel;
import com.seafile.seadroid2.data.model.ResultModel;
import com.seafile.seadroid2.data.model.repo.DirentMiniModel;
import com.seafile.seadroid2.ui.repo.RepoService;
import com.seafile.seadroid2.io.http.IO;
import com.seafile.seadroid2.ui.base.viewmodel.BaseViewModel;
import com.seafile.seadroid2.util.Utils;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import io.reactivex.Single;
import io.reactivex.functions.Consumer;
import okhttp3.RequestBody;

public class ImagePreviewViewModel extends BaseViewModel {
    private MutableLiveData<List<DirentModel>> ListLiveData = new MutableLiveData<>();
    private final MutableLiveData<Boolean> StarLiveData = new MutableLiveData<>();

    public MutableLiveData<Boolean> getStarLiveData() {
        return StarLiveData;
    }

    public MutableLiveData<List<DirentModel>> getListLiveData() {
        return ListLiveData;
    }

    public void loadData(String repoID, String dirPath) {
        if (!dirPath.endsWith("/")) {
            dirPath += "/";
        }

        Single<List<DirentModel>> single = AppDatabase.getInstance().direntDao().getAllByParentPath(repoID, dirPath);
        addSingleDisposable(single, new Consumer<List<DirentModel>>() {
            @Override
            public void accept(List<DirentModel> direntModels) throws Exception {
                getListLiveData().setValue(parseList(direntModels));
            }
        });
    }

    private List<DirentModel> parseList(List<DirentModel> direntModels) {
        if (direntModels == null) {
            return null;
        }

        return direntModels
                .stream()
                .filter(f -> !f.isDir() && Utils.isViewableImage(f.name))
                .collect(Collectors.toList());

    }


    //star
    public void star(String repoId, String path) {
        getRefreshLiveData().setValue(true);

        Map<String, String> requestDataMap = new HashMap<>();
        requestDataMap.put("repo_id", repoId);
        requestDataMap.put("path", path);
        Map<String, RequestBody> bodyMap = generateRequestBody(requestDataMap);

        Single<DirentMiniModel> single = IO.getSingleton().execute(RepoService.class).star(bodyMap);
        addSingleDisposable(single, new Consumer<DirentMiniModel>() {
            @Override
            public void accept(DirentMiniModel resultModel) throws Exception {
                getRefreshLiveData().setValue(false);

                getStarLiveData().setValue(true);
                ToastUtils.showLong(R.string.success);
            }
        }, new Consumer<Throwable>() {
            @Override
            public void accept(Throwable throwable) throws Exception {
                getRefreshLiveData().setValue(false);
                getStarLiveData().setValue(false);
                String errMsg = getErrorMsgByThrowable(throwable);
                ToastUtils.showLong(errMsg);
            }
        });
    }

    public void unStar(String repoId, String path) {
        getRefreshLiveData().setValue(true);

        Single<ResultModel> single = IO.getSingleton().execute(RepoService.class).unStar(repoId, path);
        addSingleDisposable(single, new Consumer<ResultModel>() {
            @Override
            public void accept(ResultModel resultModel) throws Exception {
                getRefreshLiveData().setValue(false);

                getStarLiveData().setValue(true);
                ToastUtils.showLong(R.string.success);
            }
        }, new Consumer<Throwable>() {
            @Override
            public void accept(Throwable throwable) throws Exception {
                getRefreshLiveData().setValue(false);
                String errMsg = getErrorMsgByThrowable(throwable);
                ToastUtils.showLong(errMsg);
            }
        });
    }
}
