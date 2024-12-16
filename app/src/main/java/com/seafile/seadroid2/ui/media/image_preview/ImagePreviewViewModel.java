package com.seafile.seadroid2.ui.media.image_preview;

import android.text.TextUtils;
import android.util.Pair;

import androidx.lifecycle.MutableLiveData;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.ToastUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.framework.data.db.AppDatabase;
import com.seafile.seadroid2.framework.data.db.entities.DirentModel;
import com.seafile.seadroid2.framework.data.db.entities.RepoModel;
import com.seafile.seadroid2.framework.data.model.ResultModel;
import com.seafile.seadroid2.framework.data.model.repo.Dirent2Model;
import com.seafile.seadroid2.framework.data.model.sdoc.FileDetailModel;
import com.seafile.seadroid2.framework.data.model.sdoc.MetadataConfigModel;
import com.seafile.seadroid2.framework.data.model.sdoc.FileProfileConfigModel;
import com.seafile.seadroid2.framework.data.model.user.UserWrapperModel;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.framework.worker.BackgroundJobManagerImpl;
import com.seafile.seadroid2.framework.http.HttpIO;
import com.seafile.seadroid2.ui.base.viewmodel.BaseViewModel;
import com.seafile.seadroid2.ui.sdoc.DocsCommentService;
import com.seafile.seadroid2.ui.star.StarredService;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import io.reactivex.Single;
import io.reactivex.functions.BiFunction;
import io.reactivex.functions.Consumer;
import io.reactivex.functions.Function3;
import okhttp3.RequestBody;

public class ImagePreviewViewModel extends BaseViewModel {
    private final MutableLiveData<List<DirentModel>> ListLiveData = new MutableLiveData<>();
    private final MutableLiveData<Boolean> StarLiveData = new MutableLiveData<>();
    private final MutableLiveData<Pair<RepoModel, List<DirentModel>>> _repoAndListLiveData = new MutableLiveData<>();

    public MutableLiveData<Pair<RepoModel, List<DirentModel>>> getRepoAndListLiveData() {
        return _repoAndListLiveData;
    }

    public MutableLiveData<Boolean> getStarLiveData() {
        return StarLiveData;
    }

    public MutableLiveData<List<DirentModel>> getListLiveData() {
        return ListLiveData;
    }

    private final MutableLiveData<FileProfileConfigModel> _fileProfileConfigLiveData = new MutableLiveData<>();

    public MutableLiveData<FileProfileConfigModel> getFileDetailLiveData() {
        return _fileProfileConfigLiveData;
    }

    public void getFileDetail(String repoId, String path) {
        getRefreshLiveData().setValue(true);

        Single<UserWrapperModel> userSingle = HttpIO.getCurrentInstance().execute(DocsCommentService.class).getRelatedUsers(repoId);
        Single<MetadataConfigModel> metadataSingle = HttpIO.getCurrentInstance().execute(DocsCommentService.class).getMetadata(repoId);
        Single<FileDetailModel> detailSingle = HttpIO.getCurrentInstance().execute(DocsCommentService.class).getFileDetail(repoId, path);

        Single<FileProfileConfigModel> s = Single.zip(detailSingle, userSingle, metadataSingle, new Function3<FileDetailModel, UserWrapperModel, MetadataConfigModel, FileProfileConfigModel>() {
            @Override
            public FileProfileConfigModel apply(FileDetailModel docDetailModel, UserWrapperModel userWrapperModel, MetadataConfigModel metadataConfigModel) throws Exception {
                FileProfileConfigModel configModel = new FileProfileConfigModel();
                configModel.setDetail(docDetailModel);
                configModel.setUsers(userWrapperModel);
                configModel.setMetadata(metadataConfigModel);
                return configModel;
            }
        });

        addSingleDisposable(s, new Consumer<FileProfileConfigModel>() {
            @Override
            public void accept(FileProfileConfigModel fileProfileConfigModel) throws Exception {
                getFileDetailLiveData().setValue(fileProfileConfigModel);
                getRefreshLiveData().setValue(false);
            }
        }, new Consumer<Throwable>() {
            @Override
            public void accept(Throwable throwable) throws Exception {
                getRefreshLiveData().setValue(false);
            }
        });
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

    public void load(String repoId, String parentPath, String name, boolean isLoadOtherImagesInSameDirectory) {
        if (TextUtils.isEmpty(repoId) || TextUtils.isEmpty(parentPath) || TextUtils.isEmpty(name)) {
            return;
        }

        getRefreshLiveData().setValue(true);

        Single<List<RepoModel>> repoSingle = AppDatabase.getInstance().repoDao().getRepoById(repoId);


        Single<List<DirentModel>> fileSingle;
        if (isLoadOtherImagesInSameDirectory) {
            fileSingle = AppDatabase.getInstance().direntDao().getFileListByParentPath(repoId, parentPath);
        } else {
            String fullPath = Utils.pathJoin(parentPath, name);
            fileSingle = AppDatabase.getInstance().direntDao().getListByFullPathAsync(repoId, fullPath);
        }

        Single<Pair<RepoModel, List<DirentModel>>> single = Single.zip(repoSingle, fileSingle, new BiFunction<List<RepoModel>, List<DirentModel>, Pair<RepoModel, List<DirentModel>>>() {
            @Override
            public Pair<RepoModel, List<DirentModel>> apply(List<RepoModel> models, List<DirentModel> direntModels) throws Exception {
                if (CollectionUtils.isEmpty(models)) {
                    throw SeafException.notFoundException;
                }

                RepoModel repoModel = models.get(0);
                List<DirentModel> dirents = direntModels.stream()
                        .filter(f -> Utils.isViewableImage(f.name))
                        .collect(Collectors.toList());

                return new Pair<>(repoModel, dirents);
            }
        });

        addSingleDisposable(single, new Consumer<Pair<RepoModel, List<DirentModel>>>() {
            @Override
            public void accept(Pair<RepoModel, List<DirentModel>> repoModelListPair) throws Exception {
                getRefreshLiveData().setValue(false);
                getRepoAndListLiveData().setValue(repoModelListPair);
            }
        }, new Consumer<Throwable>() {
            @Override
            public void accept(Throwable throwable) throws Exception {
                getRefreshLiveData().setValue(false);
                SeafException seafException = getExceptionByThrowable(throwable);
                getSeafExceptionLiveData().setValue(seafException);
            }
        });


    }

    public void loadData(String repoID, String parentPath) {
        if (TextUtils.isEmpty(parentPath)) {
            getListLiveData().setValue(CollectionUtils.newArrayList());
            return;
        }

        Single<List<DirentModel>> single = AppDatabase.getInstance().direntDao().getListByParentPathAsync(repoID, parentPath);
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

        Single<List<DirentModel>> single = AppDatabase.getInstance().direntDao().getListByFullPathAsync(repoID, fullPath);
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
        Map<String, RequestBody> bodyMap = genRequestBody(requestDataMap);

        Single<Dirent2Model> single = HttpIO.getCurrentInstance().execute(StarredService.class).star(bodyMap);
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

        Single<ResultModel> single = HttpIO.getCurrentInstance().execute(StarredService.class).unStar(repoId, path);
        addSingleDisposable(single, new Consumer<ResultModel>() {
            @Override
            public void accept(ResultModel resultModel) throws Exception {
                getRefreshLiveData().setValue(false);

                getStarLiveData().setValue(false);
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
