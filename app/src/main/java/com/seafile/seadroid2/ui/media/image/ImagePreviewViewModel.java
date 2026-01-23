package com.seafile.seadroid2.ui.media.image;

import android.text.TextUtils;
import android.util.Pair;

import androidx.lifecycle.MutableLiveData;

import com.blankj.utilcode.util.CollectionUtils;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.framework.db.AppDatabase;
import com.seafile.seadroid2.framework.db.entities.DirentModel;
import com.seafile.seadroid2.framework.db.entities.RepoModel;
import com.seafile.seadroid2.framework.http.HttpIO;
import com.seafile.seadroid2.framework.model.ResultModel;
import com.seafile.seadroid2.framework.model.dirents.DirentFileModel;
import com.seafile.seadroid2.framework.model.repo.Dirent2Model;
import com.seafile.seadroid2.framework.util.Toasts;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.baseviewmodel.BaseViewModel;
import com.seafile.seadroid2.ui.file.FileService;
import com.seafile.seadroid2.ui.star.StarredService;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

import io.reactivex.Completable;
import io.reactivex.Single;
import io.reactivex.SingleSource;
import io.reactivex.functions.BiFunction;
import io.reactivex.functions.Consumer;
import io.reactivex.functions.Function;
import okhttp3.RequestBody;

public class ImagePreviewViewModel extends BaseViewModel {
    private final MutableLiveData<Boolean> _starredLiveData = new MutableLiveData<>();
    private final MutableLiveData<Integer> _tapLiveData = new MutableLiveData<>();

    private final MutableLiveData<Pair<RepoModel, List<DirentModel>>> _repoAndListLiveData = new MutableLiveData<>();
    public MutableLiveData<Integer> getTapLiveData() {
        return _tapLiveData;
    }

    private final MutableLiveData<DetailLayoutShowModel> _scrolling = new MutableLiveData<>();

    public MutableLiveData<DetailLayoutShowModel> getScrolling() {
        return _scrolling;
    }

    public MutableLiveData<Pair<RepoModel, List<DirentModel>>> getRepoAndListLiveData() {
        return _repoAndListLiveData;
    }

    public MutableLiveData<Boolean> getStarredLiveData() {
        return _starredLiveData;
    }

    public void load(String repoId, String repoName, String parentPath, String name, boolean isLoadOtherImagesInSameDirectory) {
        if (TextUtils.isEmpty(repoId) || TextUtils.isEmpty(parentPath) || TextUtils.isEmpty(name)) {
            return;
        }

        getRefreshLiveData().setValue(true);

        String fullPath = Utils.pathJoin(parentPath, name);


        Single<List<RepoModel>> repoSingle = AppDatabase.getInstance().repoDao().getRepoById(repoId);

        Single<List<DirentModel>> direntSingle;
        if (isLoadOtherImagesInSameDirectory) {
            direntSingle = AppDatabase.getInstance().direntDao().getFileListByParentPath(repoId, parentPath);
        } else {
            direntSingle = AppDatabase.getInstance().direntDao().getListByFullPathAsync(repoId, fullPath);
        }

        Single<Pair<RepoModel, List<DirentModel>>> single = Single.zip(repoSingle, direntSingle, new BiFunction<List<RepoModel>, List<DirentModel>, Pair<RepoModel, List<DirentModel>>>() {
            @Override
            public Pair<RepoModel, List<DirentModel>> apply(List<RepoModel> repoModels, List<DirentModel> direntModels) throws Exception {
                if (CollectionUtils.isEmpty(repoModels)) {
                    throw SeafException.NOT_FOUND_EXCEPTION;
                }

                RepoModel repoModel = repoModels.get(0);
                if (CollectionUtils.isEmpty(direntModels)) {
                    return new Pair<>(repoModel, null);
                }

                List<DirentModel> dirents = direntModels.stream()
                        .filter(f -> Utils.isViewableImage(f.name))
                        .collect(Collectors.toList());
                return new Pair<>(repoModel, dirents);
            }
        }).flatMap(new Function<Pair<RepoModel, List<DirentModel>>, SingleSource<Pair<RepoModel, List<DirentModel>>>>() {
            @Override
            public SingleSource<Pair<RepoModel, List<DirentModel>>> apply(Pair<RepoModel, List<DirentModel>> pair) throws Exception {
                if (!CollectionUtils.isEmpty(pair.second)) {
                    return Single.just(pair);
                }

                Single<DirentFileModel> detailSingle = HttpIO.getCurrentInstance()
                        .execute(FileService.class)
                        .getFileDetail(repoId, fullPath)
                        .onErrorReturnItem(new DirentFileModel("an error occurred"));
                return detailSingle.flatMap(new Function<DirentFileModel, SingleSource<Pair<RepoModel, List<DirentModel>>>>() {
                    @Override
                    public SingleSource<Pair<RepoModel, List<DirentModel>>> apply(DirentFileModel direntFileModel) throws Exception {
                        if (!TextUtils.isEmpty(direntFileModel.error_msg)) {
                            throw new SeafException(0, direntFileModel.error_msg);
                        }

                        DirentModel direntModel = DirentModel.convertDetailModelToThis(direntFileModel, fullPath, repoId, repoName);
                        direntModel.related_account = pair.first.related_account;

                        List<DirentModel> dirents = CollectionUtils.newArrayList(direntModel);
                        return Single.just(new Pair<>(pair.first, dirents));
                    }
                });
            }
        }).flatMap(new Function<Pair<RepoModel, List<DirentModel>>, SingleSource<? extends Pair<RepoModel, List<DirentModel>>>>() {
            @Override
            public SingleSource<? extends Pair<RepoModel, List<DirentModel>>> apply(Pair<RepoModel, List<DirentModel>> pair1) throws Exception {
                if (CollectionUtils.isEmpty(pair1.second)) {
                    return Single.just(pair1);
                }

                //insert into db
                Completable completable = AppDatabase.getInstance().direntDao().insertAll(pair1.second);
                Single<Integer> f = completable.toSingleDefault(0);
                return f.flatMap(new Function<Integer, SingleSource<Pair<RepoModel, List<DirentModel>>>>() {
                    @Override
                    public SingleSource<Pair<RepoModel, List<DirentModel>>> apply(Integer integer) throws Exception {
                        return Single.just(pair1);
                    }
                });
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
                SeafException seafException = getSeafExceptionByThrowable(throwable);
                getSeafExceptionLiveData().setValue(seafException);
                Toasts.show(seafException.getMessage());
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

        Single<Dirent2Model> single = HttpIO.getCurrentInstance()
                .execute(StarredService.class)
                .star(bodyMap)
                .delay(200, TimeUnit.MILLISECONDS);
        addSingleDisposable(single, new Consumer<Dirent2Model>() {
            @Override
            public void accept(Dirent2Model resultModel) throws Exception {
                getRefreshLiveData().setValue(false);

                getStarredLiveData().setValue(true);
            }
        }, new Consumer<Throwable>() {
            @Override
            public void accept(Throwable throwable) throws Exception {
                getRefreshLiveData().setValue(false);

                String errMsg = getErrorMsgByThrowable(throwable);
                Toasts.show(errMsg);
            }
        });
    }

    public void unStar(String repoId, String path) {
        getRefreshLiveData().setValue(true);

        Single<ResultModel> single = HttpIO.getCurrentInstance()
                .execute(StarredService.class)
                .unStar(repoId, path)
                .delay(200, TimeUnit.MILLISECONDS);
        addSingleDisposable(single, new Consumer<ResultModel>() {
            @Override
            public void accept(ResultModel resultModel) throws Exception {
                getRefreshLiveData().setValue(false);

                getStarredLiveData().setValue(false);
            }
        }, new Consumer<Throwable>() {
            @Override
            public void accept(Throwable throwable) throws Exception {
                getRefreshLiveData().setValue(false);

                String errMsg = getErrorMsgByThrowable(throwable);
                Toasts.show(errMsg);
            }
        });
    }
}
