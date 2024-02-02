package com.seafile.seadroid2.ui.base.viewmodel;

import android.text.TextUtils;

import androidx.annotation.NonNull;
import androidx.lifecycle.MutableLiveData;
import androidx.lifecycle.ViewModel;

import com.blankj.utilcode.util.GsonUtils;
import com.blankj.utilcode.util.ToastUtils;
import com.seafile.seadroid2.BuildConfig;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.data.model.ResultModel;
import com.seafile.seadroid2.util.SLogs;

import java.io.Closeable;
import java.io.IOException;
import java.net.SocketTimeoutException;
import java.util.HashMap;
import java.util.Map;

import javax.net.ssl.SSLHandshakeException;

import io.reactivex.Completable;
import io.reactivex.Flowable;
import io.reactivex.Single;
import io.reactivex.android.schedulers.AndroidSchedulers;
import io.reactivex.disposables.CompositeDisposable;
import io.reactivex.disposables.Disposable;
import io.reactivex.functions.Action;
import io.reactivex.functions.Consumer;
import io.reactivex.schedulers.Schedulers;
import kotlin.Pair;
import okhttp3.MediaType;
import okhttp3.RequestBody;
import retrofit2.HttpException;

public class BaseViewModel extends ViewModel {
    private final MutableLiveData<Boolean> RefreshLiveData = new MutableLiveData<>(false);
    private final MutableLiveData<Pair<Integer, SeafException>> ExceptionLiveData = new MutableLiveData<>();

    public MutableLiveData<Boolean> getRefreshLiveData() {
        return RefreshLiveData;
    }

    public MutableLiveData<Pair<Integer, SeafException>> getExceptionLiveData() {
        return ExceptionLiveData;
    }

    public void showRefresh() {
        getRefreshLiveData().setValue(true);
    }

    public void closeRefresh() {
        getRefreshLiveData().setValue(false);
    }

    private final CompositeDisposable compositeDisposable = new CompositeDisposable();

    public void addDisposable(@NonNull Disposable closeable) {
        compositeDisposable.add(closeable);
    }

    private final Consumer<Throwable> throwable = throwable -> {
        SLogs.e(throwable);
        closeRefresh();

        if (BuildConfig.DEBUG) {
            ToastUtils.showLong(throwable.getMessage());
        }

        //check and callback
        checkException(throwable);
    };

    public void disposeAll() {
        compositeDisposable.clear();
        SLogs.d("CompositeDisposable clear all");
    }

    @Override
    protected void onCleared() {
        super.onCleared();
        SLogs.d("onCleared");
        if (!compositeDisposable.isDisposed()) {
            compositeDisposable.dispose();
            SLogs.d("CompositeDisposable dispose");
        }
    }

    public Map<String, RequestBody> generateRequestBody(Map<String, String> requestDataMap) {
        Map<String, RequestBody> requestBodyMap = new HashMap<>();
        if (requestDataMap == null || requestDataMap.isEmpty()) {
            requestBodyMap.put("x-test", RequestBody.create(MediaType.parse("multipart/form-data"), "test"));
            return requestBodyMap;
        }

        for (String key : requestDataMap.keySet()) {
            String s = requestDataMap.get(key);
            if (!TextUtils.isEmpty(s)) {
                RequestBody requestBody = RequestBody.create(MediaType.parse("multipart/form-data"), s);
                requestBodyMap.put(key, requestBody);
            }
        }
        return requestBodyMap;
    }

    public <T> void addSingleDisposable(Single<T> single, Consumer<T> consumer) {
        compositeDisposable.add(single
                .subscribeOn(Schedulers.io())
                .observeOn(AndroidSchedulers.mainThread())
                .subscribe(consumer, throwable));
    }

    public <T> void addSingleDisposable(Single<T> single, Consumer<T> consumer, Consumer<Throwable> throwable) {
        compositeDisposable.add(single
                .subscribeOn(Schedulers.io())
                .observeOn(AndroidSchedulers.mainThread())
                .subscribe(consumer, throwable));
    }

    public <T> void addFlowableDisposable(Flowable<T> flowable, Consumer<T> consumer) {
        compositeDisposable.add(flowable
                .subscribeOn(Schedulers.io())
                .observeOn(AndroidSchedulers.mainThread(), true)
                .subscribe(consumer, throwable));
    }

    public <T> void addCompletableDisposable(Completable completable, Action action) {
        compositeDisposable.add(completable
                .subscribeOn(Schedulers.io())
                .observeOn(AndroidSchedulers.mainThread())
                .subscribe(action, throwable));
    }

    public <T> void addCompletableDisposable(Completable completable, Action action, Consumer<Throwable> throwable1) {
        compositeDisposable.add(completable
                .subscribeOn(Schedulers.io())
                .observeOn(AndroidSchedulers.mainThread())
                .subscribe(action, throwable1));
    }


    private void checkException(Throwable throwable) {
        if (throwable instanceof HttpException) {
            HttpException httpException = (HttpException) throwable;

            if (httpException.getCause() instanceof SSLHandshakeException) {
                getExceptionLiveData().setValue(new kotlin.Pair<>(httpException.code(), SeafException.sslException));
            } else {
                getExceptionLiveData().setValue(new kotlin.Pair<>(httpException.code(), SeafException.networkException));
            }
        }
    }

    //TODO 优化异常返回
    public String getErrorMsgByThrowable(Throwable throwable) throws IOException {
        if (throwable instanceof HttpException) {
            HttpException httpException = (HttpException) throwable;
            if (httpException.response() != null && httpException.response().errorBody() != null) {

                if (504 == httpException.code()){
                    return SeadroidApplication.getAppContext().getString(R.string.network_unavailable);
                }

                String json = httpException.response().errorBody().string();
                if (TextUtils.isEmpty(json)) {
                    return SeadroidApplication.getAppContext().getString(R.string.unknow_error);
                }

                if (json.contains("{\"error_msg\":")) {
                    ResultModel resultModel = GsonUtils.fromJson(json, ResultModel.class);
                    if (TextUtils.equals("Wrong password", resultModel.error_msg)) {
                        return SeadroidApplication.getAppContext().getString(R.string.wrong_password);
                    }

                    return resultModel.error_msg;
                }

                return json;
            }

//            if (httpException.code() == 404) {
//                if (httpException.response() != null && httpException.response().errorBody() != null) {
//
//                }
//            } else if (httpException.code() == 400) {
//
////                {"error_msg":"Wrong password"}
//
//                return SeadroidApplication.getAppContext().getString(R.string.no_permission);
//            }
        } else if (throwable instanceof SSLHandshakeException) {
            SSLHandshakeException sslHandshakeException = (SSLHandshakeException) throwable;
            SLogs.e(sslHandshakeException.getMessage());
            return SeadroidApplication.getAppContext().getString(R.string.network_unavailable);
        } else if (throwable instanceof SocketTimeoutException) {
            SocketTimeoutException socketTimeoutException = (SocketTimeoutException) throwable;
            SLogs.e(socketTimeoutException.getMessage());
            return SeadroidApplication.getAppContext().getString(R.string.network_unavailable);
        }

        return SeadroidApplication.getAppContext().getString(R.string.unknow_error);
    }
}
