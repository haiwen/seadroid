package com.seafile.seadroid2.ui.selector;

import androidx.lifecycle.MutableLiveData;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.ToastUtils;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.enums.ItemPositionEnum;
import com.seafile.seadroid2.framework.db.AppDatabase;
import com.seafile.seadroid2.framework.db.entities.DirentModel;
import com.seafile.seadroid2.framework.db.entities.EncKeyCacheEntity;
import com.seafile.seadroid2.framework.db.entities.PermissionEntity;
import com.seafile.seadroid2.framework.db.entities.RepoModel;
import com.seafile.seadroid2.framework.model.permission.PermissionWrapperModel;
import com.seafile.seadroid2.framework.model.repo.RepoWrapperModel;
import com.seafile.seadroid2.framework.util.Times;
import com.seafile.seadroid2.ui.base.viewmodel.BaseViewModel;
import com.seafile.seadroid2.context.NavContext;
import com.seafile.seadroid2.framework.model.BaseModel;
import com.seafile.seadroid2.framework.model.repo.DirentWrapperModel;
import com.seafile.seadroid2.ui.repo.RepoService;
import com.seafile.seadroid2.framework.http.HttpIO;
import com.seafile.seadroid2.framework.util.Objs;
import com.seafile.seadroid2.framework.util.SLogs;

import java.util.ArrayList;
import java.util.List;

import io.reactivex.Single;
import io.reactivex.SingleSource;
import io.reactivex.functions.Consumer;
import io.reactivex.functions.Function;
import kotlin.Pair;

public class ObjSelectorViewModel extends BaseViewModel {
    private final MutableLiveData<List<BaseModel>> ObjsListLiveData = new MutableLiveData<>();

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


    public void loadAccount() {
        List<Account> results = SupportAccountManager.getInstance().getSignedInAccountList();
        if (!CollectionUtils.isEmpty(results)) {
            if (results.size() == 1) {
                results.get(0).item_position = ItemPositionEnum.ALL;
            } else if (results.size() == 2) {
                results.get(0).item_position = ItemPositionEnum.START;
                results.get(1).item_position = ItemPositionEnum.END;
            } else {
                results.get(0).item_position = ItemPositionEnum.START;
                results.get(results.size() - 1).item_position = ItemPositionEnum.END;
            }
        }

        getObjsListLiveData().setValue(new ArrayList<>(results));
        getRefreshLiveData().setValue(false);
    }

    /**
     * @param isFilterUnavailable Filter out encrypted and read-only repo
     */
    public void loadReposFromNet(Account account, boolean isFilterUnavailable) {
        getRefreshLiveData().setValue(true);

        Single<RepoWrapperModel> single = HttpIO.getInstanceByAccount(account).execute(RepoService.class).getReposAsync();
        addSingleDisposable(single, new Consumer<RepoWrapperModel>() {
            @Override
            public void accept(RepoWrapperModel repoWrapperModel) throws Exception {
                if (repoWrapperModel == null || CollectionUtils.isEmpty(repoWrapperModel.repos)) {
                    getObjsListLiveData().setValue(null);
                    getRefreshLiveData().setValue(false);
                    return;
                }

                List<RepoModel> list1 = new ArrayList<>();
                if (!CollectionUtils.isEmpty(repoWrapperModel.repos)) {
                    list1.addAll(repoWrapperModel.repos);
                }

                for (RepoModel repoModel : list1) {
                    repoModel.related_account = account.getSignature();
                    repoModel.last_modified_long = Times.convertMtime2Long(repoModel.last_modified);
                }

                List<BaseModel> list2 = Objs.convertToAdapterList(list1, isFilterUnavailable);
                getObjsListLiveData().setValue(list2);
                getRefreshLiveData().setValue(false);
            }
        }, new Consumer<Throwable>() {
            @Override
            public void accept(Throwable throwable) throws Exception {
                SLogs.d(throwable.getMessage());
            }
        });
    }

    public void loadDirentsFromNet(Account account, NavContext context) {
        getRefreshLiveData().setValue(true);

        String repoId = context.getRepoModel().repo_id;
        String parentDir = context.getNavPath();

        Single<DirentWrapperModel> singleNet = HttpIO.getInstanceByAccount(account).execute(RepoService.class).getDirentsAsync(repoId, parentDir);
        addSingleDisposable(singleNet, new Consumer<DirentWrapperModel>() {
            @Override
            public void accept(DirentWrapperModel direntWrapperModel) throws Exception {

                List<DirentModel> newDirentModels = Objs.parseDirentsForDB(
                        direntWrapperModel.dirent_list,
                        direntWrapperModel.dir_id,
                        account.getSignature(),
                        context.getRepoModel().repo_id,
                        context.getRepoModel().repo_name, true);

                //calculate item_position
                if (CollectionUtils.isEmpty(newDirentModels)) {

                } else if (newDirentModels.size() == 1) {
                    newDirentModels.get(0).item_position = ItemPositionEnum.ALL;
                } else if (newDirentModels.size() == 2) {
                    newDirentModels.get(0).item_position = ItemPositionEnum.START;
                    newDirentModels.get(1).item_position = ItemPositionEnum.END;
                } else {
                    newDirentModels.get(0).item_position = ItemPositionEnum.START;
                    newDirentModels.get(newDirentModels.size() - 1).item_position = ItemPositionEnum.END;
                }

                getObjsListLiveData().setValue(new ArrayList<>(newDirentModels));
                getRefreshLiveData().setValue(false);
            }
        }, new Consumer<Throwable>() {
            @Override
            public void accept(Throwable throwable) {
                getRefreshLiveData().setValue(false);
                getExceptionLiveData().setValue(new Pair<>(400, SeafException.NETWORK_EXCEPTION));
                String msg = getErrorMsgByThrowable(throwable);
                ToastUtils.showLong(msg);
            }
        });
    }


    public void getPermissionFromLocal(String repoId, int pNum, Consumer<PermissionEntity> consumer) {
        Single<List<PermissionEntity>> pSingle = AppDatabase.getInstance().permissionDAO().getByRepoAndIdAsync(repoId, pNum);
        Single<PermissionEntity> s = pSingle.flatMap(new Function<List<PermissionEntity>, SingleSource<PermissionEntity>>() {
            @Override
            public SingleSource<PermissionEntity> apply(List<PermissionEntity> pList) throws Exception {

                if (CollectionUtils.isEmpty(pList)) {
                    return Single.just(new PermissionEntity());
                }

                return Single.just(pList.get(0));
            }
        }).flatMap(new Function<PermissionEntity, SingleSource<PermissionEntity>>() {
            @Override
            public SingleSource<PermissionEntity> apply(PermissionEntity entity) throws Exception {

                if (entity.isValid()) {
                    return Single.just(entity);
                }

                Single<PermissionEntity> r = getLoadRepoPermissionFromRemoteSingle(repoId, pNum);
                return r.flatMap(new Function<PermissionEntity, SingleSource<PermissionEntity>>() {
                    @Override
                    public SingleSource<PermissionEntity> apply(PermissionEntity permission) throws Exception {

                        if (permission == null) {
                            return Single.just(new PermissionEntity());
                        }

                        return Single.just(permission);
                    }
                });
            }
        });

        addSingleDisposable(s, new Consumer<PermissionEntity>() {
            @Override
            public void accept(PermissionEntity entity) throws Exception {
                if (consumer != null) {
                    consumer.accept(entity);
                }
            }
        });
    }


    private Single<PermissionEntity> getLoadRepoPermissionFromRemoteSingle(String repoId, int pNum) {
        Single<PermissionWrapperModel> single = HttpIO.getCurrentInstance().execute(RepoService.class).getCustomSharePermissionById(repoId, pNum);
        return single.flatMap(new Function<PermissionWrapperModel, SingleSource<PermissionEntity>>() {
            @Override
            public SingleSource<PermissionEntity> apply(PermissionWrapperModel wrapperModel) throws Exception {
                if (wrapperModel == null || wrapperModel.permission == null) {
                    return Single.just(new PermissionEntity());
                }
                PermissionEntity permission = new PermissionEntity(repoId, wrapperModel.permission);

                AppDatabase.getInstance().permissionDAO().insert(permission);
                SLogs.d("The list has been inserted into the local database");

                return Single.just(permission);
            }
        });
    }

}
