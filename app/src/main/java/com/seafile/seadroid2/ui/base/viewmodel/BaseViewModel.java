package com.seafile.seadroid2.ui.base.viewmodel;

import android.text.TextUtils;

import androidx.annotation.NonNull;
import androidx.lifecycle.MutableLiveData;
import androidx.lifecycle.ViewModel;

import com.seafile.seadroid2.BuildConfig;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.annotation.Todo;
import com.seafile.seadroid2.framework.http.HttpIO;
import com.seafile.seadroid2.framework.util.ExceptionUtils;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.util.Toasts;
import com.seafile.seadroid2.ui.account.AccountService;

import java.io.IOException;
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
    private final MutableLiveData<Boolean> _refreshLiveData = new MutableLiveData<>(false);
    private final MutableLiveData<Boolean> _secondRefreshLiveData = new MutableLiveData<>(false);

    private final MutableLiveData<Pair<Integer, SeafException>> _exceptionLiveData = new MutableLiveData<>();
    private final MutableLiveData<SeafException> _seafExceptionLiveData = new MutableLiveData<>();
    private final MutableLiveData<Boolean> _showLoadingDialogLiveData = new MutableLiveData<>(false);

    public MutableLiveData<Boolean> getShowLoadingDialogLiveData() {
        return _showLoadingDialogLiveData;
    }

    public MutableLiveData<Boolean> getRefreshLiveData() {
        return _refreshLiveData;
    }

    public MutableLiveData<Boolean> getSecondRefreshLiveData() {
        return _secondRefreshLiveData;
    }

    public MutableLiveData<Pair<Integer, SeafException>> getExceptionLiveData() {
        return _exceptionLiveData;
    }

    public MutableLiveData<SeafException> getSeafExceptionLiveData() {
        return _seafExceptionLiveData;
    }

    public void showRefresh() {
        getRefreshLiveData().setValue(true);
    }

    public void closeRefresh() {
        getRefreshLiveData().setValue(false);
    }

    private final CompositeDisposable compositeDisposable = new CompositeDisposable();

    public void completeRemoteWipe() {
        Single<Object> single = HttpIO.getCurrentInstance().execute(AccountService.class).deviceWiped();
        addSingleDisposable(single, new Consumer<Object>() {
            @Override
            public void accept(Object o) throws Exception {
                SLogs.d("device wiped");
            }
        });
    }

    private final Consumer<Throwable> defaultThrowable = throwable -> {
        SLogs.e(throwable);
        closeRefresh();

        if (BuildConfig.DEBUG) {
            Toasts.show(throwable.getMessage());
        }

        //check and callback
        checkException(throwable);
    };

    public void disposeAll() {
        compositeDisposable.clear();
        SLogs.d("CompositeDisposable dispose all");
    }

    @Override
    protected void onCleared() {
        super.onCleared();
        SLogs.d("onCleared");
        compositeDisposable.clear();
    }

    @Todo("it need to be optimized")
    public Map<String, RequestBody> genRequestBody(Map<String, String> requestDataMap) {
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

    public void addDisposable(@NonNull Disposable closeable) {
        compositeDisposable.add(closeable);
    }

    /// /////////////// single ///////////////////
    public <T> void addSingleDisposable(Single<T> single, Consumer<T> consumer) {
        compositeDisposable.add(single
                .subscribeOn(Schedulers.io())
                .observeOn(AndroidSchedulers.mainThread())
                .subscribe(consumer, defaultThrowable));
    }

    public <T> void addSingleDisposable(Single<T> single, Consumer<T> consumer, Consumer<Throwable> throwable) {
        compositeDisposable.add(single
                .subscribeOn(Schedulers.io())
                .observeOn(AndroidSchedulers.mainThread())
                .subscribe(consumer, throwable));
    }

    /// /////////////// flowable ///////////////////
    public <T> void addFlowableDisposable(Flowable<T> flowable, Consumer<T> onNext) {
        compositeDisposable.add(flowable
                .subscribeOn(Schedulers.io())
                .observeOn(AndroidSchedulers.mainThread(), true)
                .subscribe(onNext, defaultThrowable));
    }

    public <T> void addFlowableDisposable(Flowable<T> flowable, Consumer<T> onNext, Action onComplete) {
        compositeDisposable.add(flowable
                .subscribeOn(Schedulers.io())
                .observeOn(AndroidSchedulers.mainThread())
                .subscribe(onNext, defaultThrowable, onComplete));
    }

    public <T> void addFlowableDisposable(Flowable<T> flowable, Consumer<T> onNext, Consumer<Throwable> throwable) {
        compositeDisposable.add(flowable
                .subscribeOn(Schedulers.io())
                .observeOn(AndroidSchedulers.mainThread())
                .subscribe(onNext, throwable));
    }

    public <T> void addFlowableDisposable(Flowable<T> flowable, Consumer<T> onNext, Consumer<Throwable> throwable, Action onComplete) {
        compositeDisposable.add(flowable
                .subscribeOn(Schedulers.io())
                .observeOn(AndroidSchedulers.mainThread())
                .subscribe(onNext, throwable, onComplete));
    }

    /// /////////////// completable ///////////////////
    public <T> void addCompletableDisposable(Completable completable, Action action) {
        compositeDisposable.add(completable
                .subscribeOn(Schedulers.io())
                .observeOn(AndroidSchedulers.mainThread())
                .subscribe(action, defaultThrowable));
    }

    public <T> void addCompletableDisposable(Completable completable, Action action, Consumer<Throwable> throwable1) {
        compositeDisposable.add(completable
                .subscribeOn(Schedulers.io())
                .observeOn(AndroidSchedulers.mainThread())
                .subscribe(action, throwable1));
    }

    private void checkException(Throwable throwable) {
        if (throwable instanceof HttpException httpException) {
            if (httpException.getCause() instanceof SSLHandshakeException) {
                getExceptionLiveData().setValue(new kotlin.Pair<>(httpException.code(), SeafException.NETWORK_SSL_EXCEPTION));
            } else {
                getExceptionLiveData().setValue(new kotlin.Pair<>(httpException.code(), SeafException.NETWORK_EXCEPTION));
            }
        }
    }

    public SeafException getSeafExceptionByThrowable(Throwable throwable) throws IOException {
        return ExceptionUtils.parseByThrowable(throwable);
    }

    public String getErrorMsgByThrowable(Throwable throwable) {
        SeafException seafException = ExceptionUtils.parseByThrowable(throwable);
        return seafException.getMessage();
    }
}
