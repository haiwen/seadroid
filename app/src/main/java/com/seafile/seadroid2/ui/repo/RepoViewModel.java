package com.seafile.seadroid2.ui.repo;

import android.text.TextUtils;

import androidx.lifecycle.MutableLiveData;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.NetworkUtils;
import com.blankj.utilcode.util.ToastUtils;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.context.NavContext;
import com.seafile.seadroid2.enums.FileViewType;
import com.seafile.seadroid2.framework.data.db.AppDatabase;
import com.seafile.seadroid2.framework.data.db.entities.DirentModel;
import com.seafile.seadroid2.framework.data.db.entities.EncKeyCacheEntity;
import com.seafile.seadroid2.framework.data.db.entities.FileTransferEntity;
import com.seafile.seadroid2.framework.data.db.entities.RepoModel;
import com.seafile.seadroid2.framework.data.model.BaseModel;
import com.seafile.seadroid2.framework.data.model.ResultModel;
import com.seafile.seadroid2.enums.TransferStatus;
import com.seafile.seadroid2.framework.data.model.repo.Dirent2Model;
import com.seafile.seadroid2.framework.http.HttpIO;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.preferences.Settings;
import com.seafile.seadroid2.ui.base.viewmodel.BaseViewModel;
import com.seafile.seadroid2.framework.util.Objs;
import com.seafile.seadroid2.framework.util.SLogs;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import io.reactivex.Single;
import io.reactivex.functions.BiFunction;
import io.reactivex.functions.Consumer;
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

    public void getEncCacheDB(String repoId, Consumer<EncKeyCacheEntity> consumer) {
        Single<List<EncKeyCacheEntity>> single = AppDatabase.getInstance().encKeyCacheDAO().getListByRepoIdAsync(repoId);
        addSingleDisposable(single, new Consumer<List<EncKeyCacheEntity>>() {
            @Override
            public void accept(List<EncKeyCacheEntity> list) throws Exception {
                if (CollectionUtils.isEmpty(list)) {
                    consumer.accept(null);
                } else {
                    consumer.accept(list.get(0));
                }
            }
        });
    }


    public void getRepoModelFromLocal(String repoId, Consumer<RepoModel> consumer) {
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

        if (!context.inRepo()) {
            loadReposFromLocal(account, forceRefresh);
        } else if (forceRefresh) {
            loadDirentsFromRemote(account, context);
        } else {
            FileViewType fileViewType = Settings.FILE_LIST_VIEW_TYPE.queryValue();
            if (FileViewType.GALLERY == fileViewType) {
                loadDirentsFromLocalWithGalleryViewType(account, context);
            } else {
                loadDirentsFromLocal(account, context);
            }
        }
    }

    private void loadReposFromLocal(Account account, boolean isForce) {
        if (isForce) {
            getRefreshLiveData().setValue(true);
        }

        Single<List<RepoModel>> singleDB = AppDatabase.getInstance().repoDao().getListByAccount(account.getSignature());
        addSingleDisposable(singleDB, new Consumer<List<RepoModel>>() {
            @Override
            public void accept(List<RepoModel> repoModels) {

                if (CollectionUtils.isEmpty(repoModels)) {
                    loadReposFromRemote(account);
                    return;
                }

                List<BaseModel> list = Objs.parseRepoListForAdapter(repoModels, account.getSignature(), false);
                getObjsListLiveData().setValue(list);

                if (isForce) {
                    loadReposFromRemote(account);
                } else {
                    getRefreshLiveData().setValue(false);
                }
            }
        });
    }

    private void loadReposFromRemote(Account account) {
        if (!NetworkUtils.isConnected()) {
            getRefreshLiveData().setValue(false);
            return;
        }

        //load net data and load local data
        Single<List<BaseModel>> resultSingle = Objs.getReposSingleFromServer(account);

        addSingleDisposable(resultSingle, new Consumer<List<BaseModel>>() {
            @Override
            public void accept(List<BaseModel> models) throws Exception {
                getObjsListLiveData().setValue(models);
                getRefreshLiveData().setValue(false);
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

    private void loadDirentsFromLocalWithGalleryViewType(Account account, NavContext context) {
        getRefreshLiveData().setValue(true);

        String repoId = context.getRepoModel().repo_id;
        String parentDir = context.getNavPath();

        Single<List<DirentModel>> direntDBSingle = AppDatabase.getInstance().direntDao().getListByParentPath(repoId, parentDir);
        addSingleDisposable(direntDBSingle, new Consumer<List<DirentModel>>() {
            @Override
            public void accept(List<DirentModel> direntModels) throws Exception {

                List<DirentModel> rets = CollectionUtils.newArrayList();
                for (DirentModel direntModel : direntModels) {
                    if (Utils.isViewableImage(direntModel.name) || Utils.isVideoFile(direntModel.name)) {
                        rets.add(direntModel);
                    }
                }

                if (CollectionUtils.isEmpty(rets)) {
                    loadDirentsFromRemote(account, context);
                } else {
                    getObjsListLiveData().setValue(Objs.parseLocalDirents(rets));
                    getRefreshLiveData().setValue(false);
                }
            }
        });
    }

    private void loadDirentsFromLocal(Account account, NavContext context) {
        getRefreshLiveData().setValue(true);

        String repoId = context.getRepoModel().repo_id;
        String parentDir = context.getNavPath();

        Single<List<DirentModel>> direntDBSingle = AppDatabase.getInstance().direntDao().getListByParentPath(repoId, parentDir);
        Single<List<FileTransferEntity>> curParentDownloadedList = AppDatabase.getInstance().fileTransferDAO().getDownloadedListByParentAsync(repoId, parentDir);

        Single<List<DirentModel>> resultSingle = Single.zip(direntDBSingle, curParentDownloadedList, new BiFunction<List<DirentModel>, List<FileTransferEntity>, List<DirentModel>>() {
            @Override
            public List<DirentModel> apply(List<DirentModel> direntModels, List<FileTransferEntity> cur_parent_downloaded_list) throws Exception {
                if (CollectionUtils.isEmpty(direntModels)) {
                    return direntModels;
                }

                for (DirentModel direntModel : direntModels) {
                    String fullPath = direntModel.parent_dir + direntModel.name;
                    Optional<FileTransferEntity> firstOp = cur_parent_downloaded_list.stream().filter(f -> TextUtils.equals(fullPath, f.full_path)).findFirst();
                    if (firstOp.isPresent()) {
                        FileTransferEntity entity = firstOp.get();
                        if (entity.transfer_status == TransferStatus.SUCCEEDED) {
                            direntModel.transfer_status = entity.transfer_status;
                            direntModel.local_file_path = entity.target_path;
                        }
                    }
                }

                return direntModels;
            }
        });

        addSingleDisposable(resultSingle, new Consumer<List<DirentModel>>() {
            @Override
            public void accept(List<DirentModel> direntModels) throws Exception {
                if (CollectionUtils.isEmpty(direntModels)) {
                    loadDirentsFromRemote(account, context);
                } else {
                    getObjsListLiveData().setValue(Objs.parseLocalDirents(direntModels));
                    getRefreshLiveData().setValue(false);
                }
            }
        });
    }

    private void loadDirentsFromRemote(Account account, NavContext context) {
        if (!NetworkUtils.isConnected()) {
            getRefreshLiveData().setValue(false);
            return;
        }

        String repoId = context.getRepoModel().repo_id;
        String repoName = context.getRepoModel().repo_name;
        String parentDir = context.getNavPath();

        Single<List<DirentModel>> resultSingle = Objs.getDirentsSingleFromServer(account, repoId, repoName, parentDir);

        addSingleDisposable(resultSingle, new Consumer<List<DirentModel>>() {
            @Override
            public void accept(List<DirentModel> direntModels) throws Exception {

                FileViewType fileViewType = Settings.FILE_LIST_VIEW_TYPE.queryValue();
                if (FileViewType.GALLERY == fileViewType) {
                    List<DirentModel> rets = CollectionUtils.newArrayList();
                    for (DirentModel direntModel : direntModels) {
                        if (Utils.isViewableImage(direntModel.name) || Utils.isVideoFile(direntModel.name)) {
                            rets.add(direntModel);
                        }
                    }

                    getObjsListLiveData().setValue(new ArrayList<>(rets));
                } else {
                    getObjsListLiveData().setValue(new ArrayList<>(direntModels));
                }

                getRefreshLiveData().setValue(false);
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
                getStarLiveData().setValue(true);
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

        Single<ResultModel> single = HttpIO.getCurrentInstance().execute(RepoService.class).unStar(repoId, path);
        addSingleDisposable(single, new Consumer<ResultModel>() {
            @Override
            public void accept(ResultModel resultModel) throws Exception {
                getStarLiveData().setValue(true);
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
