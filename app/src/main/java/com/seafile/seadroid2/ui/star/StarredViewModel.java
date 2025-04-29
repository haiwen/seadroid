package com.seafile.seadroid2.ui.star;

import android.text.TextUtils;

import androidx.lifecycle.MutableLiveData;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.TimeUtils;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.framework.crypto.SecurePasswordManager;
import com.seafile.seadroid2.framework.db.AppDatabase;
import com.seafile.seadroid2.framework.db.entities.EncKeyCacheEntity;
import com.seafile.seadroid2.framework.db.entities.FileCacheStatusEntity;
import com.seafile.seadroid2.framework.db.entities.RepoModel;
import com.seafile.seadroid2.framework.model.TResultModel;
import com.seafile.seadroid2.framework.model.dirents.DirentFileModel;
import com.seafile.seadroid2.framework.datastore.sp.SettingsManager;
import com.seafile.seadroid2.framework.util.Objs;
import com.seafile.seadroid2.ui.base.viewmodel.BaseViewModel;
import com.seafile.seadroid2.framework.http.HttpIO;
import com.seafile.seadroid2.framework.model.ResultModel;
import com.seafile.seadroid2.framework.db.entities.StarredModel;
import com.seafile.seadroid2.ui.dialog_fragment.DialogService;
import com.seafile.seadroid2.ui.file.FileService;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import io.reactivex.Single;
import io.reactivex.SingleSource;
import io.reactivex.functions.Consumer;
import io.reactivex.functions.Function;
import kotlin.Pair;
import okhttp3.RequestBody;

public class StarredViewModel extends BaseViewModel {
    private final MutableLiveData<List<StarredModel>> listLiveData = new MutableLiveData<>();
    private final MutableLiveData<Pair<String, ResultModel>> UnStarredResultLiveData = new MutableLiveData<>();

    public MutableLiveData<List<StarredModel>> getListLiveData() {
        return listLiveData;
    }

    public MutableLiveData<Pair<String, ResultModel>> getUnStarredResultLiveData() {
        return UnStarredResultLiveData;
    }

    public void decryptRepo(String repoId, Consumer<String> consumer) {
        if (consumer == null) {
            throw new IllegalArgumentException("consumer is null");
        }

        Single<List<EncKeyCacheEntity>> encSingle = AppDatabase.getInstance().encKeyCacheDAO().getListByRepoIdAsync(repoId);
        Single<String> s = encSingle.flatMap(new Function<List<EncKeyCacheEntity>, SingleSource<String>>() {
            @Override
            public SingleSource<String> apply(List<EncKeyCacheEntity> encKeyCacheEntities) throws Exception {
                if (CollectionUtils.isEmpty(encKeyCacheEntities)) {
                    return Single.just("need-to-re-enter-password");//need password and save into database
                }

                long now = TimeUtils.getNowMills();
                EncKeyCacheEntity encKeyCacheEntity = encKeyCacheEntities.get(0);
                boolean isExpired = encKeyCacheEntity.expire_time_long == 0 || now > encKeyCacheEntity.expire_time_long;
                if (isExpired) {
                    if (TextUtils.isEmpty(encKeyCacheEntity.enc_key) || TextUtils.isEmpty(encKeyCacheEntity.enc_iv)) {
                        return Single.just("need-to-re-enter-password");//expired, need password
                    } else {
                        String decryptPassword = SecurePasswordManager.decryptPassword(encKeyCacheEntity.enc_key, encKeyCacheEntity.enc_iv);
                        return Single.just(decryptPassword);//expired, but no password
                    }
                }

                return Single.just("done");
            }
        });


        addSingleDisposable(s, new Consumer<String>() {
            @Override
            public void accept(String i) throws Exception {
                consumer.accept(i);
            }
        });
    }


    public void remoteVerify(String repoId, String password, Consumer<ResultModel> consumer) {
        if (consumer == null) {
            throw new IllegalArgumentException("consumer is null");
        }
        getRefreshLiveData().setValue(true);

        Map<String, String> requestDataMap = new HashMap<>();
        requestDataMap.put("password", password);
        Map<String, RequestBody> bodyMap = genRequestBody(requestDataMap);

        Single<ResultModel> netSingle = HttpIO.getCurrentInstance().execute(DialogService.class).setPassword(repoId, bodyMap);
        Single<ResultModel> single = netSingle.flatMap(new Function<ResultModel, SingleSource<ResultModel>>() {
            @Override
            public SingleSource<ResultModel> apply(ResultModel resultModel) throws Exception {
                //update local password and expire
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

                return Single.just(resultModel);
            }
        });

        addSingleDisposable(single, tResultModel -> {
            getRefreshLiveData().setValue(false);
            consumer.accept(tResultModel);
        }, throwable -> {
            getRefreshLiveData().setValue(false);

            TResultModel<RepoModel> tResultModel = new TResultModel<>();
            tResultModel.error_msg = getErrorMsgByThrowable(throwable);
            consumer.accept(tResultModel);
        });
    }

    public void checkRemoteAndOpen(String repo_id, String path, Consumer<String> consumer) {
        getSecondRefreshLiveData().setValue(true);
        Single<DirentFileModel> detailSingle = HttpIO.getCurrentInstance().execute(FileService.class).getFileDetail(repo_id, path);

        Single<List<FileCacheStatusEntity>> dbSingle = AppDatabase.getInstance().fileCacheStatusDAO().getByFullPath(repo_id, path);
        Single<String> fileIdSingle = dbSingle.flatMap(new Function<List<FileCacheStatusEntity>, SingleSource<String>>() {
            @Override
            public SingleSource<String> apply(List<FileCacheStatusEntity> f) {
                if (CollectionUtils.isEmpty(f)) {
                    return Single.just("");
                }

                if (TextUtils.isEmpty(f.get(0).file_id)) {
                    return Single.just("");
                }

                return Single.just(f.get(0).file_id);
            }
        }).flatMap(new Function<String, SingleSource<String>>() {
            @Override
            public SingleSource<String> apply(String local_file_id) throws Exception {
                if (TextUtils.isEmpty(local_file_id)) {
                    return Single.just("");
                }

                return detailSingle.flatMap(new Function<DirentFileModel, SingleSource<? extends String>>() {
                    @Override
                    public SingleSource<? extends String> apply(DirentFileModel direntFileModel) throws Exception {
                        if (direntFileModel == null) {
                            return Single.just("");
                        }
                        if (!direntFileModel.id.equals(local_file_id)) {
                            return Single.just("");
                        }
                        return Single.just(local_file_id);
                    }
                });
            }
        });

        addSingleDisposable(fileIdSingle, new Consumer<String>() {
            @Override
            public void accept(String local_file_id) throws Exception {
                if (consumer != null) {
                    consumer.accept(local_file_id);
                }
                getSecondRefreshLiveData().setValue(false);
            }
        }, new Consumer<Throwable>() {
            @Override
            public void accept(Throwable throwable) throws Exception {
                if (consumer != null) {
                    consumer.accept(null);
                }
                getSecondRefreshLiveData().setValue(false);
            }
        });


    }

    public void loadData() {
        getRefreshLiveData().setValue(true);

        Account account = SupportAccountManager.getInstance().getCurrentAccount();

        Single<List<StarredModel>> listSingle = Objs.getStarredSingleFromServer(account);

        addSingleDisposable(listSingle, new Consumer<List<StarredModel>>() {
            @Override
            public void accept(List<StarredModel> starredModels) throws Exception {
                getRefreshLiveData().setValue(false);
                getListLiveData().setValue(starredModels);
            }
        }, new Consumer<Throwable>() {
            @Override
            public void accept(Throwable throwable) throws Exception {
                getRefreshLiveData().setValue(false);
                SeafException seafException = getExceptionByThrowable(throwable);

                if (seafException == SeafException.REMOTE_WIPED_EXCEPTION) {
                    //post a request
                    completeRemoteWipe();
                }

                getSeafExceptionLiveData().setValue(seafException);
            }
        });
    }

    public void unStarItem(String repoId, String path) {
        Single<ResultModel> flowable = HttpIO.getCurrentInstance().execute(StarredService.class).unStar(repoId, path);
        addSingleDisposable(flowable, new Consumer<ResultModel>() {
            @Override
            public void accept(ResultModel resultModel) throws Exception {
                getUnStarredResultLiveData().setValue(new Pair<>(path, resultModel));
            }
        });
    }

}
