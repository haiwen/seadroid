package com.seafile.seadroid2.ui.selector;

import androidx.lifecycle.MutableLiveData;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.ToastUtils;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.framework.data.db.AppDatabase;
import com.seafile.seadroid2.framework.data.db.entities.DirentModel;
import com.seafile.seadroid2.framework.data.db.entities.EncKeyCacheEntity;
import com.seafile.seadroid2.framework.data.db.entities.PermissionEntity;
import com.seafile.seadroid2.framework.data.model.permission.PermissionListWrapperModel;
import com.seafile.seadroid2.framework.data.model.permission.PermissionWrapperModel;
import com.seafile.seadroid2.ui.base.viewmodel.BaseViewModel;
import com.seafile.seadroid2.context.NavContext;
import com.seafile.seadroid2.framework.data.model.BaseModel;
import com.seafile.seadroid2.framework.data.model.repo.DirentWrapperModel;
import com.seafile.seadroid2.framework.data.model.repo.RepoWrapperModel;
import com.seafile.seadroid2.ui.repo.RepoService;
import com.seafile.seadroid2.framework.http.HttpIO;
import com.seafile.seadroid2.framework.util.Objs;
import com.seafile.seadroid2.framework.util.SLogs;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import io.reactivex.Completable;
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
        List<Account> list = SupportAccountManager.getInstance().getSignedInAccountList();
        getObjsListLiveData().setValue(new ArrayList<>(list));
        getRefreshLiveData().setValue(false);
    }

    /**
     * @param isFilter Filter out encrypted and read-only repo
     */
    public void loadReposFromNet(Account account, boolean isFilter) {
        getRefreshLiveData().setValue(true);
        Single<RepoWrapperModel> singleNet = HttpIO.getInstanceByAccount(account).execute(RepoService.class).getRepos();

        addSingleDisposable(singleNet, new Consumer<RepoWrapperModel>() {
            @Override
            public void accept(RepoWrapperModel repoWrapperModel) throws Exception {
                if (repoWrapperModel == null || CollectionUtils.isEmpty(repoWrapperModel.repos)) {
                    getObjsListLiveData().setValue(null);
                    getRefreshLiveData().setValue(false);
                    return;
                }

                List<BaseModel> list = Objs.parseRepoListForAdapter(repoWrapperModel.repos, account.getSignature(), isFilter);
                getObjsListLiveData().setValue(list);
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

        Single<DirentWrapperModel> singleNet = HttpIO.getInstanceByAccount(account).execute(RepoService.class).getDirents(repoId, parentDir);
        addSingleDisposable(singleNet, new Consumer<DirentWrapperModel>() {
            @Override
            public void accept(DirentWrapperModel direntWrapperModel) throws Exception {

                List<DirentModel> list = Objs.parseDirentsForDB(
                        direntWrapperModel.dirent_list,
                        direntWrapperModel.dir_id,
                        account.getSignature(),
                        context.getRepoModel().repo_id,
                        context.getRepoModel().repo_name, true);

                getObjsListLiveData().setValue(new ArrayList<>(list));
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


    public void getPermissionFromLocal(String repoId, int pNum, Consumer<PermissionEntity> consumer) {
        Single<List<PermissionEntity>> pSingle = AppDatabase.getInstance().permissionDAO().getWithAsync(repoId, pNum);
        Single<PermissionEntity> s = pSingle.flatMap(new Function<List<PermissionEntity>, SingleSource<PermissionEntity>>() {
            @Override
            public SingleSource<PermissionEntity> apply(List<PermissionEntity> pList) throws Exception {

                if (CollectionUtils.isEmpty(pList)) {
                    return null;
                }

                return Single.just(pList.get(0));
            }
        }).flatMap(new Function<PermissionEntity, SingleSource<PermissionEntity>>() {
            @Override
            public SingleSource<PermissionEntity> apply(PermissionEntity entity) throws Exception {
                Single<List<PermissionEntity>> r = getLoadRepoPermissionFromRemoteSingle(repoId);

                return r.flatMap(new Function<List<PermissionEntity>, SingleSource<? extends PermissionEntity>>() {
                    @Override
                    public SingleSource<? extends PermissionEntity> apply(List<PermissionEntity> permissionEntities) throws Exception {
                        if (CollectionUtils.isEmpty(permissionEntities)) {
                            return null;

                        }
                        Optional<PermissionEntity> p = permissionEntities.stream().filter(f -> f.id == pNum).findFirst();
                        if (p.isPresent()) {
                            return Single.just(p.get());
                        }
                        return null;
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


    private Single<List<PermissionEntity>> getLoadRepoPermissionFromRemoteSingle(String repoId) {
        Single<PermissionListWrapperModel> single = HttpIO.getCurrentInstance().execute(RepoService.class).getCustomSharePermissions(repoId);
        return single.flatMap(new Function<PermissionListWrapperModel, SingleSource<List<PermissionEntity>>>() {
            @Override
            public SingleSource<List<PermissionEntity>> apply(PermissionListWrapperModel wrapperModel) throws Exception {

                List<PermissionEntity> list = CollectionUtils.newArrayList();

                for (PermissionWrapperModel model : wrapperModel.permission_list) {
                    list.add(new PermissionEntity(repoId, model));
                }

                Completable insertCompletable = AppDatabase.getInstance().permissionDAO().insertAllAsync(list);
                Single<Long> insertAllSingle = insertCompletable.toSingleDefault(0L);
                return insertAllSingle.flatMap(new Function<Long, SingleSource<List<PermissionEntity>>>() {
                    @Override
                    public SingleSource<List<PermissionEntity>> apply(Long aLong) throws Exception {
                        SLogs.d("The list has been inserted into the local database");
                        return Single.just(list);
                    }
                });
            }
        });
    }

}
