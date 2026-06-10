package com.seafile.seadroid2.ui.selector.obj;

import android.text.TextUtils;

import androidx.lifecycle.MutableLiveData;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.TimeUtils;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.baseviewmodel.BaseViewModel;
import com.seafile.seadroid2.context.NavContext;
import com.seafile.seadroid2.enums.ItemPositionEnum;
import com.seafile.seadroid2.enums.RepoDecryptResult;
import com.seafile.seadroid2.framework.crypto.SecurePasswordManager;
import com.seafile.seadroid2.framework.datastore.sp.SettingsManager;
import com.seafile.seadroid2.framework.db.AppDatabase;
import com.seafile.seadroid2.framework.db.entities.DirentModel;
import com.seafile.seadroid2.framework.db.entities.EncKeyCacheEntity;
import com.seafile.seadroid2.framework.db.entities.PermissionEntity;
import com.seafile.seadroid2.framework.db.entities.RepoModel;
import com.seafile.seadroid2.framework.http.HttpManager;
import com.seafile.seadroid2.framework.model.BaseModel;
import com.seafile.seadroid2.framework.model.ResultModel;
import com.seafile.seadroid2.framework.model.permission.PermissionWrapperModel;
import com.seafile.seadroid2.framework.model.repo.DirentWrapperModel;
import com.seafile.seadroid2.framework.model.repo.RepoWrapperModel;
import com.seafile.seadroid2.framework.util.ExceptionUtils;
import com.seafile.seadroid2.framework.util.Objs;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.util.Times;
import com.seafile.seadroid2.framework.util.Toasts;
import com.seafile.seadroid2.ui.dialog_fragment.DialogService;
import com.seafile.seadroid2.ui.repo.RepoService;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import io.reactivex.Single;
import io.reactivex.SingleSource;
import io.reactivex.functions.Consumer;
import io.reactivex.functions.Function;
import kotlin.Pair;
import okhttp3.RequestBody;

public class ObjSelectorViewModel extends BaseViewModel {
    private final MutableLiveData<List<BaseModel>> ObjsListLiveData = new MutableLiveData<>();

    public MutableLiveData<List<BaseModel>> getObjsListLiveData() {
        return ObjsListLiveData;
    }

    public void loadAccount() {
        List<Account> results = SupportAccountManager.getInstance().getSignedInAccountList();
        if (!CollectionUtils.isEmpty(results)) {
            if (results.size() == 1) {
                results.get(0).item_position = ItemPositionEnum.ALL;
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
    public void loadReposFromNet(Account account, boolean isFilterUnavailable, boolean isFilterEncryptRepo, boolean isAddStarredGroup) {
        loadReposFromNet(account, isFilterUnavailable, isAddStarredGroup, isFilterEncryptRepo, null);
    }

    public void loadReposFromNet(Account account, boolean isFilterUnavailable, boolean isFilterEncryptRepo, boolean isAddStarredGroup, List<String> filterIds) {

        getRefreshLiveData().setValue(true);

        Single<RepoWrapperModel> single = HttpManager.getHttpWithAccount(account).execute(RepoService.class).getReposAsync();
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

                List<BaseModel> list2 = Objs.convertToAdapterList(list1, isFilterUnavailable, isAddStarredGroup, isFilterEncryptRepo, filterIds);
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
        if (context.getRepoModel() == null) {
            return;
        }

        String repoId = context.getRepoModel().repo_id;
        String parentDir = context.getNavPath();

        Single<DirentWrapperModel> singleNet = HttpManager.getHttpWithAccount(account).execute(RepoService.class).getDirentsAsync(repoId, parentDir);
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
                Toasts.show(msg);
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
        Single<PermissionWrapperModel> single = HttpManager.getCurrentHttp().execute(RepoService.class).getCustomSharePermissionById(repoId, pNum);
        return single.flatMap(new Function<PermissionWrapperModel, SingleSource<PermissionEntity>>() {
            @Override
            public SingleSource<PermissionEntity> apply(PermissionWrapperModel wrapperModel) throws Exception {
                if (wrapperModel == null || wrapperModel.permission == null) {
                    return Single.just(new PermissionEntity());
                }
                PermissionEntity permission = new PermissionEntity(repoId, wrapperModel.permission);

                AppDatabase.getInstance().permissionDAO().insert(permission);
                SLogs.d("ObjSelectorViewModel - The list has been inserted into the local database");

                return Single.just(permission);
            }
        });
    }


    public void decryptRepo(RepoModel repoModel, Consumer<RepoDecryptResult> consumer) {
        if (repoModel == null || !repoModel.encrypted) {
            return;
        }

        // query the local database to get the password cache
        Single<List<EncKeyCacheEntity>> encSingle = AppDatabase.getInstance().encKeyCacheDAO().getListByRepoIdAsync(repoModel.repo_id);

        Single<RepoDecryptResult> decryptSingle = encSingle.flatMap(new Function<List<EncKeyCacheEntity>, SingleSource<RepoDecryptResult>>() {
            @Override
            public SingleSource<RepoDecryptResult> apply(List<EncKeyCacheEntity> encKeyCacheEntities) throws Exception {
                if (CollectionUtils.isEmpty(encKeyCacheEntities)) {
                    return Single.just(RepoDecryptResult.NEED_PASSWORD);
                }

                EncKeyCacheEntity encKeyCacheEntity = encKeyCacheEntities.get(0);
                long now = TimeUtils.getNowMills();
                boolean isExpired = encKeyCacheEntity.expire_time_long == 0 || now > encKeyCacheEntity.expire_time_long;

                if (!isExpired) {
                    // The password has not expired, and it will be returned to success
                    return Single.just(RepoDecryptResult.SUCCESS);
                }

                // the password has expired
                if (TextUtils.isEmpty(encKeyCacheEntity.enc_key) || TextUtils.isEmpty(encKeyCacheEntity.enc_iv)) {
                    // There is no valid encryption key and you need to re-enter your password
                    return Single.just(RepoDecryptResult.NEED_PASSWORD);
                }

                String cachedPassword = SecurePasswordManager.decryptPassword(encKeyCacheEntity.enc_key, encKeyCacheEntity.enc_iv);
                if (TextUtils.isEmpty(cachedPassword)) {
                    return Single.just(RepoDecryptResult.NEED_PASSWORD);
                }

                // There is an encryption key, which is attempted to decrypt and verify remotely
                return getRemoteVerifySingle(repoModel.repo_id, cachedPassword)
                        .flatMap(new Function<ResultModel, SingleSource<RepoDecryptResult>>() {
                            @Override
                            public SingleSource<RepoDecryptResult> apply(ResultModel resultModel) throws Exception {
                                return Single.just(resultModel.success ? RepoDecryptResult.SUCCESS : RepoDecryptResult.NEED_PASSWORD);
                            }
                        });
            }
        });

        Single<RepoDecryptResult> resultSingle = decryptSingle.flatMap(new Function<RepoDecryptResult, SingleSource<RepoDecryptResult>>() {
            @Override
            public SingleSource<RepoDecryptResult> apply(RepoDecryptResult result) throws Exception {
                return Single.just(result);
            }
        });

        addSingleDisposable(resultSingle, new Consumer<RepoDecryptResult>() {
            @Override
            public void accept(RepoDecryptResult repoDecryptResult) throws Exception {
                if (consumer != null) {
                    consumer.accept(repoDecryptResult);
                }
            }
        }, new Consumer<Throwable>() {
            @Override
            public void accept(Throwable throwable) throws Exception {
                // an exception occurs and the return fails
                SeafException seafException = ExceptionUtils.parseByThrowable(throwable);
                SLogs.e(seafException);
                if (consumer != null) {
                    consumer.accept(RepoDecryptResult.FAILED);
                }
            }
        });
    }

    /**
     * do remote authentication and update the local password cache
     */
    public Single<ResultModel> getRemoteVerifySingle(String repoId, String password) {
        Map<String, String> requestDataMap = new HashMap<>();
        requestDataMap.put("password", password);
        Map<String, RequestBody> bodyMap = genRequestBody(requestDataMap);

        Single<ResultModel> netSingle = HttpManager.getCurrentHttp().execute(DialogService.class).setPassword(repoId, bodyMap);

        return netSingle.flatMap(new Function<ResultModel, SingleSource<ResultModel>>() {
            @Override
            public SingleSource<ResultModel> apply(ResultModel resultModel) throws Exception {
                if (resultModel.success) {
                    return updateLocalPasswordCache(repoId, password)
                            .map(new Function<SeafException, ResultModel>() {
                                @Override
                                public ResultModel apply(SeafException aVoid) throws Exception {
                                    return resultModel;
                                }
                            });
                }
                return Single.just(resultModel);
            }
        });
    }

    private Single<SeafException> updateLocalPasswordCache(String repoId, String password) {
        return Single.create(emitter -> {
            try {
                EncKeyCacheEntity encEntity = new EncKeyCacheEntity();
                encEntity.v = 2;
                encEntity.repo_id = repoId;

                android.util.Pair<String, String> p = SecurePasswordManager.encryptPassword(password);
                if (p != null) {
                    encEntity.enc_key = p.first;
                    encEntity.enc_iv = p.second;

                    long expire = TimeUtils.getNowMills();
                    expire += SettingsManager.DECRYPTION_EXPIRATION_TIME;
                    encEntity.expire_time_long = expire;

                    AppDatabase.getInstance().encKeyCacheDAO().insert(encEntity);
                }

                emitter.onSuccess(SeafException.SUCCESS);
            } catch (Exception e) {
                emitter.onError(SeafException.UNSUPPORTED_ENC_VERSION);
            }
        });
    }

}
