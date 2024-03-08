package com.seafile.seadroid2.ui.dialog_fragment.viewmodel;

import android.text.TextUtils;
import android.util.Pair;

import androidx.lifecycle.MutableLiveData;

import com.blankj.utilcode.util.TimeUtils;
import com.nostra13.universalimageloader.utils.L;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.crypto.Crypto;
import com.seafile.seadroid2.data.db.entities.EncKeyCacheEntity;
import com.seafile.seadroid2.ui.repo.RepoService;
import com.seafile.seadroid2.ui.base.viewmodel.BaseViewModel;
import com.seafile.seadroid2.config.Constants;
import com.seafile.seadroid2.data.db.AppDatabase;
import com.seafile.seadroid2.data.model.ResultModel;
import com.seafile.seadroid2.data.db.entities.ObjsModel;
import com.seafile.seadroid2.data.db.entities.RepoModel;
import com.seafile.seadroid2.io.http.IO;
import com.seafile.seadroid2.ui.dialog_fragment.DialogService;

import java.io.UnsupportedEncodingException;
import java.security.InvalidAlgorithmParameterException;
import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;
import java.security.spec.InvalidKeySpecException;
import java.util.HashMap;
import java.util.Map;

import javax.crypto.BadPaddingException;
import javax.crypto.IllegalBlockSizeException;
import javax.crypto.NoSuchPaddingException;

import io.reactivex.Completable;
import io.reactivex.Single;
import io.reactivex.SingleEmitter;
import io.reactivex.SingleOnSubscribe;
import io.reactivex.SingleSource;
import io.reactivex.functions.BiFunction;
import io.reactivex.functions.Consumer;
import io.reactivex.functions.Function;
import okhttp3.RequestBody;

public class PasswordViewModel extends BaseViewModel {
    private final MutableLiveData<ResultModel> ActionLiveData = new MutableLiveData<>();

    public MutableLiveData<ResultModel> getActionLiveData() {
        return ActionLiveData;
    }

    public void verifyPwd(String repoId, String password) {
        Single<RepoModel> singleOneDb = AppDatabase.getInstance().repoDao().getOneByIdAsync(repoId);
        addSingleDisposable(singleOneDb, new Consumer<RepoModel>() {
            @Override
            public void accept(RepoModel repoModel) throws Exception {
                if (repoModel == null || TextUtils.isEmpty(repoModel.magic)) {
                    getRepoModel(repoId, new Consumer<RepoModel>() {
                        @Override
                        public void accept(RepoModel uRepoModel) throws Exception {
                            verify(uRepoModel, password);
                        }
                    });
                } else {
                    verify(repoModel, password);
                }
            }
        });

    }

    private void verify(RepoModel repoModel, String password) {
        if (!repoModel.canLocalDecrypt()) {
            setPassword(repoModel.repo_id, password);
            return;
        }

        Single<Exception> booleanSingle = Single.create(new SingleOnSubscribe<Exception>() {
            @Override
            public void subscribe(SingleEmitter<Exception> emitter) throws Exception {
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
            public void subscribe(SingleEmitter<Exception> emitter) throws Exception {
                try {
                    Crypto.verifyRepoPassword(repoModel.repo_id, password, repoModel.enc_version, repoModel.magic);

                    Pair<String, String> pair = Crypto.generateKey(password, repoModel.random_key, repoModel.enc_version);
                    EncKeyCacheEntity entity = new EncKeyCacheEntity();
                    entity.enc_key = pair.first;
                    entity.enc_iv = pair.second;
                    entity.repo_id = repoModel.repo_id;
                    entity.related_account = repoModel.related_account;

                    AppDatabase.getInstance().encKeyCacheDAO().insertSync(entity);

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


        Completable completable = getInsertObjs(repoModel.repo_id);
        Single<Long> insertSingle = completable.toSingleDefault(0L);

        Single<Exception> longSingle = booleanSingle.flatMap(new Function<Exception, SingleSource<Exception>>() {
            @Override
            public SingleSource<Exception> apply(Exception exception) throws Exception {
                if (exception != SeafException.SUCCESS) {
                    return Single.just(exception);
                }

                return insertSingle.flatMap(new Function<Long, SingleSource<? extends Exception>>() {
                    @Override
                    public SingleSource<? extends Exception> apply(Long aLong) throws Exception {
                        return Single.just(exception);
                    }
                });
            }
        }).flatMap(new Function<Exception, SingleSource<Exception>>() {
            @Override
            public SingleSource<Exception> apply(Exception exception) throws Exception {
                return insertEncSingle;
            }
        });

        addSingleDisposable(longSingle, new Consumer<Exception>() {
            @Override
            public void accept(Exception exception) throws Exception {
                ResultModel resultModel = new ResultModel();
                if (exception != SeafException.SUCCESS) {
                    resultModel.error_msg = getErrorMsgByThrowable(exception);
                } else {
                    resultModel.success = true;
                }

                getActionLiveData().setValue(resultModel);
                getRefreshLiveData().setValue(false);
            }
        });
    }

    public void getRepoModel(String repoId, Consumer<RepoModel> consumer) {
        Single<RepoModel> singleNet = IO.getSingleton().execute(RepoService.class).getRepoInfo(repoId);

        Single<RepoModel> singleDb = AppDatabase.getInstance().repoDao().getOneByIdAsync(repoId);

        Single<RepoModel> sr = Single.zip(singleNet, singleDb, new BiFunction<RepoModel, RepoModel, RepoModel>() {
            @Override
            public RepoModel apply(RepoModel netRepoModel, RepoModel localRepoModel) throws Exception {

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

    private void setPassword(String repoId, String password) {
        if (TextUtils.isEmpty(password) || TextUtils.isEmpty(repoId)) {
            return;
        }

        getRefreshLiveData().setValue(true);

        Map<String, String> requestDataMap = new HashMap<>();
        requestDataMap.put("password", password);
        Map<String, RequestBody> bodyMap = generateRequestBody(requestDataMap);

        Completable completable = getInsertObjs(repoId);
        Single<Long> insertSingle = completable.toSingleDefault(0L);

        Single<ResultModel> netSingle = IO.getSingleton().execute(DialogService.class).setPassword(repoId, bodyMap);
        Single<ResultModel> single = netSingle.flatMap(new Function<ResultModel, SingleSource<ResultModel>>() {
            @Override
            public SingleSource<ResultModel> apply(ResultModel resultModel) throws Exception {
                return insertSingle.flatMap(new Function<Long, SingleSource<ResultModel>>() {
                    @Override
                    public SingleSource<ResultModel> apply(Long aLong) throws Exception {
                        return Single.just(resultModel);
                    }
                });
            }
        });

        addSingleDisposable(single, resultModel -> {
            getRefreshLiveData().setValue(false);
            getActionLiveData().setValue(resultModel);
        }, throwable -> {
            getRefreshLiveData().setValue(false);

            ResultModel resultModel = new ResultModel();
            resultModel.error_msg = getErrorMsgByThrowable(throwable);
            getActionLiveData().setValue(resultModel);
        });
    }

    private Completable getInsertObjs(String repoId) {
        long expire = TimeUtils.getNowMills();
        expire += 1000 * 60 * 60 * 24 * 1;//1 days

        //TODO 更新指定字段
        ObjsModel objsModel = new ObjsModel();
        objsModel.path = repoId;
        objsModel.decrypt_expire_time_long = expire;
        objsModel.type = Constants.ObjType.REPO;

        return AppDatabase.getInstance().objDao().insert(objsModel);
    }
}
