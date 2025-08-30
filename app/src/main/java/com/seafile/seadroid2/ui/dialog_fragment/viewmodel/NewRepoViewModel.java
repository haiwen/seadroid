package com.seafile.seadroid2.ui.dialog_fragment.viewmodel;

import android.text.TextUtils;
import android.util.Pair;

import androidx.lifecycle.MutableLiveData;

import com.blankj.utilcode.util.TimeUtils;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.framework.crypto.SecurePasswordManager;
import com.seafile.seadroid2.framework.datastore.sp.SettingsManager;
import com.seafile.seadroid2.framework.db.AppDatabase;
import com.seafile.seadroid2.framework.db.entities.EncKeyCacheEntity;
import com.seafile.seadroid2.framework.model.repo.RepoInfoModel;
import com.seafile.seadroid2.baseviewmodel.BaseViewModel;
import com.seafile.seadroid2.framework.db.entities.RepoModel;
import com.seafile.seadroid2.framework.http.HttpIO;
import com.seafile.seadroid2.ui.dialog_fragment.DialogService;
import com.seafile.seadroid2.ui.repo.RepoService;

import java.util.HashMap;
import java.util.Map;

import io.reactivex.Single;
import io.reactivex.SingleEmitter;
import io.reactivex.SingleOnSubscribe;
import io.reactivex.SingleSource;
import io.reactivex.functions.Consumer;
import io.reactivex.functions.Function;
import okhttp3.RequestBody;

public class NewRepoViewModel extends BaseViewModel {
    private final MutableLiveData<RepoModel> createRepoLiveData = new MutableLiveData<>();

    public MutableLiveData<RepoModel> getCreateRepoLiveData() {
        return createRepoLiveData;
    }

    public void createNewRepo(String repoName, String description, String password) {
        if (TextUtils.isEmpty(repoName)) {
            return;
        }

        getRefreshLiveData().setValue(true);

        Map<String, String> requestDataMap = new HashMap<>();
        requestDataMap.put("name", repoName);

        if (!description.isEmpty()) {
            requestDataMap.put("desc", description);
        }
        if (!TextUtils.isEmpty(password)) {
            requestDataMap.put("passwd", password);
        }
        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        Map<String, RequestBody> bodyMap = genRequestBody(requestDataMap);
        Single<RepoModel> netSingle = HttpIO.getCurrentInstance().execute(DialogService.class).createRepo(bodyMap);
        Single<RepoModel> r = netSingle.flatMap(new Function<RepoModel, SingleSource<RepoModel>>() {
            @Override
            public SingleSource<RepoModel> apply(RepoModel createdRepoModel) throws Exception {
                return HttpIO.getCurrentInstance().execute(RepoService.class).getRepoInfo(createdRepoModel.repo_id)
                        .flatMap(new Function<RepoInfoModel, SingleSource<RepoModel>>() {
                            @Override
                            public SingleSource<RepoModel> apply(RepoInfoModel repoInfoModel) throws Exception {
                                createdRepoModel.enc_version = repoInfoModel.enc_version;
                                createdRepoModel.file_count = repoInfoModel.file_count;
                                createdRepoModel.magic = repoInfoModel.magic;
                                createdRepoModel.random_key = repoInfoModel.random_key;
                                createdRepoModel.salt = repoInfoModel.salt;
                                createdRepoModel.root = repoInfoModel.root;
                                return Single.just(createdRepoModel);
                            }
                        });
            }
        }).flatMap(new Function<RepoModel, SingleSource<RepoModel>>() {
            @Override
            public SingleSource<RepoModel> apply(RepoModel repoModel) throws Exception {
                if (!TextUtils.isEmpty(password)) {
                    return getInsertPasswordSingle(repoModel.repo_id, repoModel.enc_version, account.getSignature(), password)
                            .flatMap(new Function<SeafException, SingleSource<RepoModel>>() {
                                @Override
                                public SingleSource<RepoModel> apply(SeafException e) throws Exception {
                                    return Single.just(repoModel);
                                }
                            });
                } else {
                    return Single.just(repoModel);
                }
            }
        });
        addSingleDisposable(r, new Consumer<RepoModel>() {
            @Override
            public void accept(RepoModel repoModel) throws Exception {

                getRefreshLiveData().setValue(false);

                getCreateRepoLiveData().setValue(repoModel);
            }
        }, new Consumer<Throwable>() {
            @Override
            public void accept(Throwable throwable) throws Exception {
                SeafException seafException = getSeafExceptionByThrowable(throwable);
                getSeafExceptionLiveData().setValue(seafException);
                getRefreshLiveData().setValue(false);
            }
        });
    }

    private Single<SeafException> getInsertPasswordSingle(String repoId, int enc_version, String related_account, String password) {

        return Single.create(new SingleOnSubscribe<SeafException>() {
            @Override
            public void subscribe(SingleEmitter<SeafException> emitter) {
                if (emitter.isDisposed()) {
                    return;
                }

                try {
                    EncKeyCacheEntity entity = new EncKeyCacheEntity();
                    entity.v = 2; //A symmetrical algorithm is used
                    entity.repo_id = repoId;
                    entity.enc_version = enc_version;
                    entity.related_account = related_account;

                    Pair<String, String> p = SecurePasswordManager.encryptPassword(password);
                    if (p == null) {
                        emitter.onError(SeafException.ENCRYPT_EXCEPTION);
                        return;
                    }

                    entity.enc_key = p.first;
                    entity.enc_iv = p.second;
                    long expire = TimeUtils.getNowMills();
                    expire += SettingsManager.DECRYPTION_EXPIRATION_TIME;
                    entity.expire_time_long = expire;

                    AppDatabase.getInstance().encKeyCacheDAO().insertSync(entity);

                    emitter.onSuccess(SeafException.SUCCESS);
                } catch (Exception e) {
                    e.printStackTrace();
                    emitter.onError(e);
                }
            }
        });

    }
}
