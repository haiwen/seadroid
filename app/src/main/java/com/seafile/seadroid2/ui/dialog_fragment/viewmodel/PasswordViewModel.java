package com.seafile.seadroid2.ui.dialog_fragment.viewmodel;

import android.text.TextUtils;
import android.util.Pair;

import androidx.lifecycle.MutableLiveData;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.TimeUtils;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.framework.crypto.Crypto;
import com.seafile.seadroid2.framework.data.db.AppDatabase;
import com.seafile.seadroid2.framework.data.db.entities.EncKeyCacheEntity;
import com.seafile.seadroid2.framework.data.db.entities.RepoModel;
import com.seafile.seadroid2.framework.data.model.ResultModel;
import com.seafile.seadroid2.framework.data.model.TResultModel;
import com.seafile.seadroid2.framework.datastore.DataManager;
import com.seafile.seadroid2.framework.datastore.sp.SettingsManager;
import com.seafile.seadroid2.framework.http.HttpIO;
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

    public void verifyPwd(String repoId, String password) {
        Single<List<RepoModel>> singleOneDb = AppDatabase.getInstance().repoDao().getByIdAsync(repoId);
        addSingleDisposable(singleOneDb, new Consumer<List<RepoModel>>() {
            @Override
            public void accept(List<RepoModel> repoModels) throws Exception {
                if (CollectionUtils.isEmpty(repoModels) || TextUtils.isEmpty(repoModels.get(0).magic)) {
                    getRepoModel(repoId, new Consumer<RepoModel>() {
                        @Override
                        public void accept(RepoModel uRepoModel) {
                            verify(uRepoModel, password);
                        }
                    });
                } else {
                    verify(repoModels.get(0), password);
                }
            }
        });
    }

    public void getRepoModel(String repoId, Consumer<RepoModel> consumer) {
        Single<RepoModel> singleNet = HttpIO.getCurrentInstance().execute(RepoService.class).getRepoInfo(repoId);

        Single<List<RepoModel>> singleDb = AppDatabase.getInstance().repoDao().getByIdAsync(repoId);

        Single<RepoModel> sr = Single.zip(singleNet, singleDb, new BiFunction<RepoModel, List<RepoModel>, RepoModel>() {
            @Override
            public RepoModel apply(RepoModel netRepoModel, List<RepoModel> localRepoModels) {

                if (CollectionUtils.isEmpty(localRepoModels)) {
                    return null;
                }

                RepoModel localRepoModel = localRepoModels.get(0);

                //update local db
                localRepoModel.magic = netRepoModel.magic;
                localRepoModel.random_key = netRepoModel.random_key;
                localRepoModel.enc_version = netRepoModel.enc_version;
                localRepoModel.file_count = netRepoModel.file_count;
                localRepoModel.root = netRepoModel.root;

                AppDatabase.getInstance().repoDao().update(localRepoModel);

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

    private void verify(RepoModel repoModel, String password) {
        if (repoModel == null) {
            return;
        }

        //remote decrypt
        if (!repoModel.canLocalDecrypt()) {
            remoteVerify(repoModel, password);
        } else {
            localVerify(repoModel, password);
        }
    }

    private void remoteVerify(RepoModel repoModel, String password) {
        if (TextUtils.isEmpty(password) || TextUtils.isEmpty(repoModel.repo_id)) {

            getActionResultLiveData().setValue(new TResultModel<>());
            return;
        }

        getRefreshLiveData().setValue(true);

        Map<String, String> requestDataMap = new HashMap<>();
        requestDataMap.put("password", password);
        Map<String, RequestBody> bodyMap = genRequestBody(requestDataMap);

        Single<ResultModel> netSingle = HttpIO.getCurrentInstance().execute(DialogService.class).setPassword(repoModel.repo_id, bodyMap);

        Single<Exception> insertEncSingle = Single.create(new SingleOnSubscribe<Exception>() {
            @Override
            public void subscribe(SingleEmitter<Exception> emitter) {
                try {
                    EncKeyCacheEntity entity = new EncKeyCacheEntity();
                    entity.repo_id = repoModel.repo_id;
                    entity.enc_version = repoModel.enc_version;
                    entity.related_account = repoModel.related_account;

                    long expire = TimeUtils.getNowMills();
                    expire += SettingsManager.DECRYPTION_EXPIRATION_TIME;
                    entity.expire_time_long = expire;

                    //Check whether the local decryption cannot be done because version is not equal to 2
                    //If not, it may be because the user has turned off the local decryption switch
                    if (repoModel.enc_version == SettingsManager.REPO_ENC_VERSION) {
                        Pair<String, String> pair = Crypto.generateKey(password, repoModel.random_key, repoModel.enc_version);
                        entity.enc_key = pair.first;
                        entity.enc_iv = pair.second;
                    }

                    AppDatabase.getInstance().encKeyCacheDAO().insertSync(entity);

                    emitter.onSuccess(SeafException.SUCCESS);
                } catch (NoSuchAlgorithmException | UnsupportedEncodingException e) {
                    e.printStackTrace();

                    emitter.onSuccess(e);
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
            getRefreshLiveData().setValue(false);

            TResultModel<RepoModel> tResultModel = new TResultModel<>();
            tResultModel.error_msg = getErrorMsgByThrowable(throwable);
            getActionResultLiveData().setValue(tResultModel);
        });
    }

    private void localVerify(RepoModel repoModel, String password) {
        //local decrypt
        Single<Exception> verifySingle = Single.create(new SingleOnSubscribe<Exception>() {
            @Override
            public void subscribe(SingleEmitter<Exception> emitter) {
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
                    e.printStackTrace();

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
