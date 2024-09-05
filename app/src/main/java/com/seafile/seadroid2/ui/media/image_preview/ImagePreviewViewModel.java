package com.seafile.seadroid2.ui.media.image_preview;

import android.text.TextUtils;

import androidx.lifecycle.MutableLiveData;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.ToastUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.framework.data.db.AppDatabase;
import com.seafile.seadroid2.framework.data.db.entities.DirentModel;
import com.seafile.seadroid2.framework.data.db.entities.RepoModel;
import com.seafile.seadroid2.framework.data.model.ResultModel;
import com.seafile.seadroid2.framework.data.model.repo.Dirent2Model;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.framework.worker.BackgroundJobManagerImpl;
import com.seafile.seadroid2.ui.repo.RepoService;
import com.seafile.seadroid2.framework.http.HttpIO;
import com.seafile.seadroid2.ui.base.viewmodel.BaseViewModel;

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


    public void getRepoModelFromDB(String repoId, Consumer<RepoModel> consumer) {
        //from db
        Single<List<RepoModel>> singleDb = AppDatabase.getInstance().repoDao().getRepoById(repoId);
        addSingleDisposable(singleDb, new Consumer<List<RepoModel>>() {
            @Override
            public void accept(List<RepoModel> repoModels) throws Exception {
                if (consumer != null) {
                    if (CollectionUtils.isEmpty(repoModels)) {
                        //no data in sqlite, request RepoApi again
                        consumer.accept(null);
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

    public void loadData(String repoID, String parentPath) {
        if (TextUtils.isEmpty(parentPath)) {
            getListLiveData().setValue(CollectionUtils.newArrayList());
            return;
        }

        Single<List<DirentModel>> single = AppDatabase.getInstance().direntDao().getListByParentPath(repoID, parentPath);
        addSingleDisposable(single, new Consumer<List<DirentModel>>() {
            @Override
            public void accept(List<DirentModel> direntModels) throws Exception {

                List<DirentModel> ds = direntModels.stream()
                        .filter(f -> !f.isDir() && Utils.isViewableImage(f.name))
                        .collect(Collectors.toList());

                getListLiveData().setValue(ds);
            }
        });
    }

    public void download(String repoID, String fullPath) {

        Single<List<DirentModel>> single = AppDatabase.getInstance().direntDao().getListByFullPath(repoID, fullPath);
        addSingleDisposable(single, new Consumer<List<DirentModel>>() {
            @Override
            public void accept(List<DirentModel> direntModels) throws Exception {
                if (CollectionUtils.isEmpty(direntModels)) {
                    return;
                }

                BackgroundJobManagerImpl
                        .getInstance()
                        .startDownloadChainWorker(new String[]{direntModels.get(0).uid});

            }
        });
    }

    //star
    public void star(String repoId, String path) {
        getRefreshLiveData().setValue(true);

        Map<String, String> requestDataMap = new HashMap<>();
        requestDataMap.put("repo_id", repoId);
        requestDataMap.put("path", path);
        Map<String, RequestBody> bodyMap = generateRequestBody(requestDataMap);

        Single<Dirent2Model> single = HttpIO.getCurrentInstance().execute(RepoService.class).star(bodyMap);
        addSingleDisposable(single, new Consumer<Dirent2Model>() {
            @Override
            public void accept(Dirent2Model resultModel) throws Exception {
                getRefreshLiveData().setValue(false);

                getStarLiveData().setValue(true);
                ToastUtils.showLong(R.string.star_file_succeed);
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

        Single<ResultModel> single = HttpIO.getCurrentInstance().execute(RepoService.class).unStar(repoId, path);
        addSingleDisposable(single, new Consumer<ResultModel>() {
            @Override
            public void accept(ResultModel resultModel) throws Exception {
                getRefreshLiveData().setValue(false);

                getStarLiveData().setValue(true);
                ToastUtils.showLong(R.string.unstar);
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
