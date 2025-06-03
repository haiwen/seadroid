package com.seafile.seadroid2.ui.dialog_fragment.viewmodel;

import android.text.TextUtils;
import android.util.Pair;

import androidx.lifecycle.MutableLiveData;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.TimeUtils;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.framework.crypto.Crypto;
import com.seafile.seadroid2.framework.crypto.SecurePasswordManager;
import com.seafile.seadroid2.framework.db.AppDatabase;
import com.seafile.seadroid2.framework.db.entities.EncKeyCacheEntity;
import com.seafile.seadroid2.framework.db.entities.RepoModel;
import com.seafile.seadroid2.framework.model.ResultModel;
import com.seafile.seadroid2.framework.model.TResultModel;
import com.seafile.seadroid2.framework.datastore.DataManager;
import com.seafile.seadroid2.framework.datastore.sp.SettingsManager;
import com.seafile.seadroid2.framework.http.HttpIO;
import com.seafile.seadroid2.framework.model.repo.RepoInfoModel;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.ui.base.viewmodel.BaseViewModel;
import com.seafile.seadroid2.ui.dialog_fragment.DialogService;
import com.seafile.seadroid2.ui.repo.RepoService;

import java.io.UnsupportedEncodingException;
import java.security.InvalidAlgorithmParameterException;
import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;
import java.security.spec.InvalidKeySpecException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.crypto.BadPaddingException;
import javax.crypto.IllegalBlockSizeException;
import javax.crypto.NoSuchPaddingException;

import io.reactivex.Single;
import io.reactivex.SingleEmitter;
import io.reactivex.SingleOnSubscribe;
import io.reactivex.SingleSource;
import io.reactivex.functions.BiFunction;
import io.reactivex.functions.Consumer;
import io.reactivex.functions.Function;
import okhttp3.RequestBody;

public class PasswordViewModel extends BaseViewModel {
    private final MutableLiveData<TResultModel<RepoModel>> ActionResultLiveData = new MutableLiveData<>();

    public MutableLiveData<TResultModel<RepoModel>> getActionResultLiveData() {
        return ActionResultLiveData;
    }

    public void verifyPwd(Account account, String repoId, String password) {

        Single<List<RepoModel>> singleOneDb = AppDatabase.getInstance().repoDao().getByIdAsync(repoId);
        addSingleDisposable(singleOneDb, new Consumer<List<RepoModel>>() {
            @Override
            public void accept(List<RepoModel> repoModels) throws Exception {
                if (CollectionUtils.isEmpty(repoModels) || TextUtils.isEmpty(repoModels.get(0).magic)) {
                    getRepoModel(account, repoId, new Consumer<RepoModel>() {
                        @Override
                        public void accept(RepoModel uRepoModel) {
                            remoteVerify(account, uRepoModel, password);
                        }
                    });
                } else {
                    remoteVerify(account, repoModels.get(0), password);
                }
            }
        });
    }

    public void getRepoModel(Account account, String repoId, Consumer<RepoModel> consumer) {
        Single<RepoInfoModel> singleNet = HttpIO.getInstanceByAccount(account).execute(RepoService.class).getRepoInfo(repoId);
        Single<List<RepoModel>> singleDb = AppDatabase.getInstance().repoDao().getByIdAsync(repoId);

        Single<RepoModel> sr = Single.zip(singleNet, singleDb, new BiFunction<RepoInfoModel, List<RepoModel>, RepoModel>() {
            @Override
            public RepoModel apply(RepoInfoModel netRepoModel, List<RepoModel> localRepoModels) {

                RepoModel localRepoModel;
                if (CollectionUtils.isEmpty(localRepoModels)) {
                    localRepoModel = RepoInfoModel.toRepoModel(account, netRepoModel);

                    AppDatabase.getInstance().repoDao().insert(localRepoModel);
                } else {
                    localRepoModel = localRepoModels.get(0);

                    //update local db
                    localRepoModel.magic = netRepoModel.magic;
                    localRepoModel.random_key = netRepoModel.random_key;
                    localRepoModel.enc_version = netRepoModel.enc_version;
                    localRepoModel.file_count = netRepoModel.file_count;
                    localRepoModel.root = netRepoModel.root;

                    AppDatabase.getInstance().repoDao().update(localRepoModel);
                }

                return localRepoModel;
            }
        });

        addSingleDisposable(sr, new Consumer<RepoModel>() {
            @Override
            public void accept(RepoModel repoModel) throws Exception {
                consumer.accept(repoModel);
            }
        }, new Consumer<Throwable>() {
            @Override
            public void accept(Throwable throwable) throws Exception {
                SeafException seafException = getExceptionByThrowable(throwable);
                getSeafExceptionLiveData().setValue(seafException);
            }
        });
    }

    private void remoteVerify(Account account, RepoModel repoModel, String password) {
        if (TextUtils.isEmpty(password) || TextUtils.isEmpty(repoModel.repo_id)) {
            getActionResultLiveData().setValue(new TResultModel<>());
            return;
        }

        getRefreshLiveData().setValue(true);

        Map<String, String> requestDataMap = new HashMap<>();
        requestDataMap.put("password", password);
        Map<String, RequestBody> bodyMap = genRequestBody(requestDataMap);

        Single<ResultModel> netSingle = HttpIO.getInstanceByAccount(account).execute(DialogService.class).setPassword(repoModel.repo_id, bodyMap);

        Single<Exception> insertEncSingle = Single.create(new SingleOnSubscribe<Exception>() {
            @Override
            public void subscribe(SingleEmitter<Exception> emitter) {
                if (emitter.isDisposed()){
                    return;
                }

                try {
                    EncKeyCacheEntity entity = new EncKeyCacheEntity();
                    entity.v = 2; //A symmetrical algorithm is used
                    entity.repo_id = repoModel.repo_id;
                    entity.enc_version = repoModel.enc_version;
                    entity.related_account = repoModel.related_account;

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

        Single<TResultModel<RepoModel>> single = netSingle.flatMap(new Function<ResultModel, SingleSource<TResultModel<RepoModel>>>() {
            @Override
            public SingleSource<TResultModel<RepoModel>> apply(ResultModel resultModel) throws Exception {
                TResultModel<RepoModel> tResultModel = new TResultModel<>();
                tResultModel.error_msg = resultModel.error_msg;
                tResultModel.success = resultModel.success;
                tResultModel.data = repoModel;

                if (!tResultModel.success) {
                    return Single.just(tResultModel);
                }

                return insertEncSingle.flatMap(new Function<Exception, SingleSource<TResultModel<RepoModel>>>() {
                    @Override
                    public SingleSource<TResultModel<RepoModel>> apply(Exception exception) {
                        if (exception != SeafException.SUCCESS) {
                            tResultModel.error_msg = getErrorMsgByThrowable(exception);
                        } else {
                            tResultModel.success = true;
                        }

                        return Single.just(tResultModel);
                    }
                });
            }
        });

        addSingleDisposable(single, tResultModel -> {
            getRefreshLiveData().setValue(false);
            getActionResultLiveData().setValue(tResultModel);
        }, throwable -> {

            SeafException seafException = getExceptionByThrowable(throwable);
            getSeafExceptionLiveData().setValue(seafException);
            getRefreshLiveData().setValue(false);
        });
    }

    @Deprecated
    private void localVerify(RepoModel repoModel, String password) {
        //local decrypt
        Single<Exception> verifySingle = Single.create(new SingleOnSubscribe<Exception>() {
            @Override
            public void subscribe(SingleEmitter<Exception> emitter) {
                if (emitter.isDisposed()){
                    return;
                }

                try {
                    Crypto.verifyRepoPassword(repoModel.repo_id, password, repoModel.enc_version, repoModel.magic);

                    emitter.onSuccess(SeafException.SUCCESS);
                } catch (SeafException seafException) {
                    emitter.onSuccess(seafException);

                } catch (NoSuchAlgorithmException | UnsupportedEncodingException |
                         InvalidKeySpecException |
                         NoSuchPaddingException | InvalidKeyException |
                         InvalidAlgorithmParameterException |
                         BadPaddingException | IllegalBlockSizeException e) {
                    e.printStackTrace();

                    emitter.onSuccess(e);
                }
            }
        });

        Single<Exception> insertEncSingle = Single.create(new SingleOnSubscribe<Exception>() {
            @Override
            public void subscribe(SingleEmitter<Exception> emitter) {
                if (emitter.isDisposed()){
                    return;
                }
                
                try {
                    Pair<String, String> pair = Crypto.generateKey(password, repoModel.random_key, repoModel.enc_version);

                    EncKeyCacheEntity entity = new EncKeyCacheEntity();
                    entity.enc_key = pair.first;
                    entity.enc_iv = pair.second;
                    entity.repo_id = repoModel.repo_id;
                    entity.related_account = repoModel.related_account;

                    long expire = TimeUtils.getNowMills();
                    expire += DataManager.SET_PASSWORD_INTERVAL;
                    entity.expire_time_long = expire;

                    AppDatabase.getInstance().encKeyCacheDAO().insertSync(entity);

                    emitter.onSuccess(SeafException.SUCCESS);
                } catch (NoSuchAlgorithmException | UnsupportedEncodingException e) {
                    SLogs.e(e);

                    emitter.onSuccess(e);
                }
            }
        });


        Single<Exception> longSingle = verifySingle.flatMap(new Function<Exception, SingleSource<Exception>>() {
            @Override
            public SingleSource<Exception> apply(Exception exception) {
                if (exception != SeafException.SUCCESS) {
                    return Single.just(exception);
                }

                return insertEncSingle;
            }
        });

        addSingleDisposable(longSingle, new Consumer<Exception>() {
            @Override
            public void accept(Exception exception) {
                TResultModel<RepoModel> resultModel = new TResultModel<>();
                if (exception != SeafException.SUCCESS) {
                    resultModel.error_msg = getErrorMsgByThrowable(exception);
                } else {
                    resultModel.success = true;
                    resultModel.data = repoModel;
                }

                getActionResultLiveData().setValue(resultModel);
                getRefreshLiveData().setValue(false);
            }
        });
    }


}
