package com.seafile.seadroid2.ui.repo;

import android.text.TextUtils;

import androidx.lifecycle.MutableLiveData;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.ToastUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.config.Constants;
import com.seafile.seadroid2.data.db.AppDatabase;
import com.seafile.seadroid2.data.model.BaseModel;
import com.seafile.seadroid2.data.db.entities.FileTransferEntity;
import com.seafile.seadroid2.data.model.ResultModel;
import com.seafile.seadroid2.data.db.entities.ObjsModel;
import com.seafile.seadroid2.data.model.enums.TransferAction;
import com.seafile.seadroid2.data.model.enums.TransferStatus;
import com.seafile.seadroid2.data.model.repo.DirentMiniModel;
import com.seafile.seadroid2.ui.base.viewmodel.BaseViewModel;
import com.seafile.seadroid2.context.NavContext;
import com.seafile.seadroid2.io.http.IO;
import com.seafile.seadroid2.data.db.entities.DirentModel;
import com.seafile.seadroid2.data.model.repo.DirentWrapperModel;
import com.seafile.seadroid2.data.db.entities.RepoModel;
import com.seafile.seadroid2.data.model.repo.RepoWrapperModel;
import com.seafile.seadroid2.util.Objs;
import com.seafile.seadroid2.util.SLogs;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

import io.reactivex.Completable;
import io.reactivex.Single;
import io.reactivex.SingleSource;
import io.reactivex.functions.BiFunction;
import io.reactivex.functions.Consumer;
import io.reactivex.functions.Function;
import kotlin.Pair;
import kotlin.Triple;
import okhttp3.RequestBody;

public class RepoViewModel extends BaseViewModel {

    private final MutableLiveData<List<BaseModel>> ObjsListLiveData = new MutableLiveData<>();
    private final MutableLiveData<Boolean> StarLiveData = new MutableLiveData<>();

    public MutableLiveData<Boolean> getStarLiveData() {
        return StarLiveData;
    }

    public MutableLiveData<List<BaseModel>> getObjsListLiveData() {
        return ObjsListLiveData;
    }

    public void getObjFromDB(String repoId, Consumer<ObjsModel> consumer) {
        Single<List<ObjsModel>> single = AppDatabase.getInstance().objDao().getByPath(repoId, Constants.ObjType.REPO);
        addSingleDisposable(single, new Consumer<List<ObjsModel>>() {
            @Override
            public void accept(List<ObjsModel> objsModels) throws Exception {
                if (!CollectionUtils.isEmpty(objsModels)) {
                    consumer.accept(objsModels.get(0));
                } else {
                    consumer.accept(null);
                }
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

    public void loadData(NavContext context, boolean forceRefresh) {
        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        if (account == null) {
            return;
        }

        if (!context.isInRepo()) {
            loadReposFromDB(account, forceRefresh);
        } else {
            loadDirentsFromDb(account, context, forceRefresh);
        }
    }

    private void loadReposFromDB(Account account, boolean isForce) {
        Single<List<RepoModel>> singleDB = AppDatabase.getInstance().repoDao().getAllByAccount(account.getSignature());
        addSingleDisposable(singleDB, new Consumer<List<RepoModel>>() {
            @Override
            public void accept(List<RepoModel> repoModels) {

                if (CollectionUtils.isEmpty(repoModels)) {
                    loadReposFromNet(account);
                    return;
                }

                List<BaseModel> list = Objs.parseRepoListForAdapter(repoModels, account.getSignature(),false);
                getObjsListLiveData().setValue(list);

                if (isForce) {
                    loadReposFromNet(account);
                }
            }
        });
    }

    private void loadReposFromNet(Account account) {
        getRefreshLiveData().setValue(true);

        Single<RepoWrapperModel> netSingle = IO.getSingleton().execute(RepoService.class).getRepos();
        Single<List<RepoModel>> dbListSingle = AppDatabase.getInstance().repoDao().getAllByAccount(account.getSignature());

        //load net data and load local data
        Single<List<BaseModel>> resultSingle = Single.zip(netSingle, dbListSingle, new BiFunction<RepoWrapperModel, List<RepoModel>, Triple<RepoWrapperModel, List<RepoModel>, List<RepoModel>>>() {
            @Override
            public Triple<RepoWrapperModel, List<RepoModel>, List<RepoModel>> apply(RepoWrapperModel repoWrapperModel, List<RepoModel> dbModels) throws Exception {
                List<RepoModel> net2dbList = Objs.parseRepoListForDB(repoWrapperModel.repos, account.getSignature());

                //diffs.first = delete list
                //diffs.second = insert db list
                Pair<List<RepoModel>, List<RepoModel>> diffs = Objs.diffRepos(net2dbList, dbModels);
                if (diffs == null) {
                    return new Triple<>(repoWrapperModel, null, null);
                }

                return new Triple<>(repoWrapperModel, diffs.getFirst(), diffs.getSecond());
            }
        }).flatMap(new Function<Triple<RepoWrapperModel, List<RepoModel>, List<RepoModel>>, SingleSource<Pair<RepoWrapperModel, List<RepoModel>>>>() {
            @Override
            public SingleSource<Pair<RepoWrapperModel, List<RepoModel>>> apply(Triple<RepoWrapperModel, List<RepoModel>, List<RepoModel>> triple) throws Exception {

                if (CollectionUtils.isEmpty(triple.getSecond())) {
                    return Single.just(new Pair<>(triple.getFirst(), triple.getThird()));
                }

                List<String> ids = triple.getSecond().stream().map(m -> m.repo_id).collect(Collectors.toList());
                Completable deleteCompletable = AppDatabase.getInstance().repoDao().deleteAllByIds(ids);
                Single<Long> deleteSingle = deleteCompletable.toSingleDefault(0L);
                return deleteSingle.flatMap(new Function<Long, SingleSource<Pair<RepoWrapperModel, List<RepoModel>>>>() {
                    @Override
                    public SingleSource<Pair<RepoWrapperModel, List<RepoModel>>> apply(Long aLong) throws Exception {

                        return Single.just(new Pair<>(triple.getFirst(), triple.getThird()));
                    }
                });
            }
        }).flatMap(new Function<Pair<RepoWrapperModel, List<RepoModel>>, SingleSource<RepoWrapperModel>>() {
            @Override
            public SingleSource<RepoWrapperModel> apply(Pair<RepoWrapperModel, List<RepoModel>> pair) throws Exception {
                if (CollectionUtils.isEmpty(pair.getSecond())) {
                    return Single.just(pair.getFirst());
                }

                Completable insertCompletable = AppDatabase.getInstance().repoDao().insertAll(pair.getSecond());
                Single<Long> longSingle = insertCompletable.toSingleDefault(0L);
                return longSingle.flatMap(new Function<Long, SingleSource<RepoWrapperModel>>() {
                    @Override
                    public SingleSource<RepoWrapperModel> apply(Long aLong) throws Exception {
                        return Single.just(pair.getFirst());
                    }
                });
            }
        }).flatMap(new Function<RepoWrapperModel, SingleSource<List<BaseModel>>>() {
            @Override
            public SingleSource<List<BaseModel>> apply(RepoWrapperModel repoWrapperModel) throws Exception {
                List<BaseModel> models = Objs.parseRepoListForAdapter(repoWrapperModel.repos, account.getSignature(),false);
                return Single.just(models);
            }
        });
        addSingleDisposable(resultSingle, new Consumer<List<BaseModel>>() {
            @Override
            public void accept(List<BaseModel> models) throws Exception {
                getObjsListLiveData().setValue(models);
                getRefreshLiveData().setValue(false);
            }
        }, new Consumer<Throwable>() {
            @Override
            public void accept(Throwable throwable) throws Exception {
                SLogs.d(throwable.getMessage());
            }
        });
    }

    private void loadDirentsFromDb(Account account, NavContext context, boolean isForce) {
        String repoId = context.getRepoModel().repo_id;
        String parentDir = context.getNavPath();

        Single<List<DirentModel>> direntDBSingle = AppDatabase.getInstance().direntDao().getAllByParentPath(repoId, parentDir);
        Single<List<FileTransferEntity>> transferDBSingle = direntDBSingle.flatMap(new Function<List<DirentModel>, SingleSource<List<FileTransferEntity>>>() {
            @Override
            public SingleSource<List<FileTransferEntity>> apply(List<DirentModel> direntModels) throws Exception {
                if (CollectionUtils.isEmpty(direntModels)) {
                    return Single.just(Collections.emptyList());
                }

                List<String> fullPaths = direntModels.stream().map(m -> m.full_path).collect(Collectors.toList());
                if (CollectionUtils.isEmpty(fullPaths)) {
                    return Single.just(Collections.emptyList());
                }

                return AppDatabase.getInstance().fileTransferDAO().getListByFullPathsAsync(account.getSignature(), fullPaths, TransferAction.DOWNLOAD);
            }
        });


        Single<List<DirentModel>> resultSingle = Single.zip(direntDBSingle, transferDBSingle, new BiFunction<List<DirentModel>, List<FileTransferEntity>, List<DirentModel>>() {
            @Override
            public List<DirentModel> apply(List<DirentModel> direntModels, List<FileTransferEntity> list) throws Exception {
                if (CollectionUtils.isEmpty(direntModels)) {
                    return direntModels;
                }

                for (DirentModel direntModel : direntModels) {
                    String fullPath = direntModel.parent_dir + direntModel.name;
                    Optional<FileTransferEntity> firstOp = list.stream().filter(f -> TextUtils.equals(fullPath, f.full_path)).findFirst();
                    if (firstOp.isPresent()) {
                        FileTransferEntity entity = firstOp.get();
                        if (entity.transfer_status == TransferStatus.TRANSFER_SUCCEEDED) {
                            direntModel.transfer_status = entity.transfer_status;
                        }
                    }
                }

                return direntModels;
            }
        });

        addSingleDisposable(resultSingle, new Consumer<List<DirentModel>>() {
            @Override
            public void accept(List<DirentModel> direntModels) throws Exception {
                if (!CollectionUtils.isEmpty(direntModels)) {
                    getObjsListLiveData().setValue(Objs.parseLocalDirents(direntModels));

                    if (isForce) {
                        loadDirentsFromNet(account, context);
                    }
                } else {
                    loadDirentsFromNet(account, context);
                }
            }
        });
    }

    private void loadDirentsFromNet(Account account, NavContext context) {
        getRefreshLiveData().setValue(true);

        String repoId = context.getRepoModel().repo_id;
        String parentDir = context.getNavPath();


        Single<DirentWrapperModel> netSingle = IO.getSingleton().execute(RepoService.class).getDirents(repoId, parentDir);
        Single<List<DirentModel>> dbSingle = AppDatabase.getInstance().direntDao().getAllByParentPath(repoId, parentDir);
        Single<List<DirentModel>> resultSingle = Single.zip(netSingle, dbSingle, new BiFunction<DirentWrapperModel, List<DirentModel>, List<DirentModel>>() {
            @Override
            public List<DirentModel> apply(DirentWrapperModel direntWrapperModel, List<DirentModel> direntModels) throws Exception {
                return Objs.parseDirentsForDB(
                        direntWrapperModel.dirent_list,
                        direntWrapperModel.dir_id,
                        account.getSignature(),
                        context.getRepoModel().repo_id,
                        context.getRepoModel().repo_name);
            }
        }).flatMap(new Function<List<DirentModel>, SingleSource<List<DirentModel>>>() {
            @Override
            public SingleSource<List<DirentModel>> apply(List<DirentModel> netModels) throws Exception {
                if (CollectionUtils.isEmpty(netModels)) {
                    return Single.just(netModels);
                }

                Completable deleted = AppDatabase.getInstance().direntDao().deleteAllByParentPath(repoId, parentDir);
                Single<Long> deleteAllByPathSingle = deleted.toSingleDefault(0L);
                return deleteAllByPathSingle.flatMap(new Function<Long, SingleSource<List<DirentModel>>>() {
                    @Override
                    public SingleSource<List<DirentModel>> apply(Long aLong) throws Exception {
                        return Single.just(netModels);
                    }
                });
            }
        }).flatMap(new Function<List<DirentModel>, SingleSource<List<DirentModel>>>() {
            @Override
            public SingleSource<List<DirentModel>> apply(List<DirentModel> direntModels) throws Exception {
                if (CollectionUtils.isEmpty(direntModels)) {
                    return Single.just(direntModels);
                }

                Completable insertCompletable = AppDatabase.getInstance().direntDao().insertAll(direntModels);
                Single<Long> insertAllSingle = insertCompletable.toSingleDefault(0L);
                return insertAllSingle.flatMap(new Function<Long, SingleSource<List<DirentModel>>>() {
                    @Override
                    public SingleSource<List<DirentModel>> apply(Long aLong) throws Exception {

                        SLogs.d("Dirents本地数据库已更新");
                        return Single.just(direntModels);
                    }
                });
            }
        }).flatMap(new Function<List<DirentModel>, SingleSource<List<DirentModel>>>() {
            @Override
            public SingleSource<List<DirentModel>> apply(List<DirentModel> direntModels) throws Exception {
                if (CollectionUtils.isEmpty(direntModels)) {
                    return Single.just(direntModels);
                }


                List<String> fullPaths = direntModels.stream().map(m -> m.parent_dir + m.name).collect(Collectors.toList());
                Single<List<FileTransferEntity>> single = AppDatabase.getInstance().fileTransferDAO().getListByFullPathsAsync(account.getSignature(), fullPaths, TransferAction.DOWNLOAD);
                return single.flatMap(new Function<List<FileTransferEntity>, SingleSource<List<DirentModel>>>() {
                    @Override
                    public SingleSource<List<DirentModel>> apply(List<FileTransferEntity> fileTransferEntities) throws Exception {

                        for (DirentModel direntModel : direntModels) {
                            String fullPath = direntModel.parent_dir + direntModel.name;
                            Optional<FileTransferEntity> firstOp = fileTransferEntities.stream().filter(f -> TextUtils.equals(fullPath, f.full_path)).findFirst();
                            if (firstOp.isPresent()) {
                                FileTransferEntity entity = firstOp.get();
                                if (entity.transfer_status == TransferStatus.TRANSFER_SUCCEEDED) {
                                    direntModel.transfer_status = entity.transfer_status;
                                }
                            }
                        }

                        return Single.just(direntModels);
                    }
                });
            }
        });

        addSingleDisposable(resultSingle, new Consumer<List<DirentModel>>() {
            @Override
            public void accept(List<DirentModel> direntModels) throws Exception {
                getObjsListLiveData().setValue(new ArrayList<>(direntModels));
                getRefreshLiveData().setValue(false);
            }
        }, new Consumer<Throwable>() {
            @Override
            public void accept(Throwable throwable) throws Exception {
                getRefreshLiveData().setValue(false);
                getExceptionLiveData().setValue(new Pair<>(400, SeafException.networkException));
                String msg = getErrorMsgByThrowable(throwable);
                ToastUtils.showLong(msg);
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
