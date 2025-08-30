package com.seafile.seadroid2.baseviewmodel;

import android.text.TextUtils;

import androidx.annotation.NonNull;
import androidx.core.util.Pair;
import androidx.lifecycle.Lifecycle;
import androidx.lifecycle.LifecycleEventObserver;
import androidx.lifecycle.LifecycleOwner;
import androidx.lifecycle.LiveData;
import androidx.lifecycle.MutableLiveData;
import androidx.lifecycle.ViewModel;

import com.seafile.seadroid2.BuildConfig;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.annotation.Todo;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.util.Toasts;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import javax.net.ssl.SSLHandshakeException;

import io.reactivex.Completable;
import io.reactivex.Flowable;
import io.reactivex.Observable;
import io.reactivex.Single;
import io.reactivex.android.schedulers.AndroidSchedulers;
import io.reactivex.disposables.CompositeDisposable;
import io.reactivex.disposables.Disposable;
import io.reactivex.functions.Action;
import io.reactivex.functions.Consumer;
import io.reactivex.schedulers.Schedulers;
import okhttp3.MediaType;
import okhttp3.RequestBody;
import retrofit2.HttpException;

@Todo
public class WrapViewModel extends ViewModel {
    /**
     * 视图状态枚举
     */
    public enum ViewState {
        IDLE,       // 空闲状态
        LOADING,    // 加载中
        SUCCESS,    // 成功
        ERROR       // 错误
    }

    /**
     * 操作类型枚举
     */
    public enum OperationType {
        NORMAL,     // 普通操作
        LOADING,    // 需要显示加载对话框
        REFRESH     // 需要显示刷新动画
    }

    // LiveData 状态管理
    private final MutableLiveData<ViewState> _stateLiveData = new MutableLiveData<>(ViewState.IDLE);
    private final MutableLiveData<Boolean> _refreshLiveData = new MutableLiveData<>(false);
    private final MutableLiveData<Boolean> _secondRefreshLiveData = new MutableLiveData<>(false);
    private final MutableLiveData<Pair<Integer, SeafException>> _exceptionLiveData = new MutableLiveData<>();
    private final MutableLiveData<SeafException> _seafExceptionLiveData = new MutableLiveData<>();
    private final MutableLiveData<Boolean> _showLoadingDialogLiveData = new MutableLiveData<>(false);

    // 订阅管理
    private final CompositeDisposable compositeDisposable = new CompositeDisposable();

    // 异常处理器
    private final ExceptionHandler exceptionHandler = new ExceptionHandler()
            .setDefaultHandler(throwable -> {
                SLogs.e(throwable);
                closeRefresh();
                _stateLiveData.setValue(ViewState.ERROR);

                if (BuildConfig.DEBUG) {
                    Toasts.show(throwable.getMessage());
                }
            })
            .addHandler(HttpException.class, this::handleHttpException)
            .addHandler(IOException.class, this::handleIOException);

    /**
     * 异常处理类，支持针对不同类型异常的处理
     */
    public static class ExceptionHandler {
        private final Map<Class<? extends Throwable>, Consumer<Throwable>> handlers = new HashMap<>();
        private Consumer<Throwable> defaultHandler;

        public ExceptionHandler setDefaultHandler(Consumer<Throwable> handler) {
            this.defaultHandler = handler;
            return this;
        }

        public <T extends Throwable> ExceptionHandler addHandler(Class<T> exceptionClass, Consumer<T> handler) {
            handlers.put(exceptionClass, throwable -> {
                try {
                    handler.accept((T) throwable);
                } catch (Exception e) {
                    SLogs.e("Exception handler error", e);
                }
            });
            return this;
        }

        public void handle(Throwable throwable) {
            Consumer<Throwable> handler = handlers.get(throwable.getClass());
            if (handler != null) {
                try {
                    handler.accept(throwable);
                } catch (Exception e) {
                    SLogs.e("Exception handling error", e);
                }
            } else if (defaultHandler != null) {
                try {
                    defaultHandler.accept(throwable);
                } catch (Exception e) {
                    SLogs.e("Default exception handling error", e);
                }
            }
        }
    }

    /**
     * RxJava任务构建器
     */
    public class RxTaskBuilder<T> {
        private Observable<T> observable;
        private Consumer<T> onNext;
        private Consumer<Throwable> onError;
        private Action onComplete;
        private boolean showLoading = false;
        private boolean showRefresh = false;

        public RxTaskBuilder<T> source(Observable<T> observable) {
            this.observable = observable;
            return this;
        }

        public RxTaskBuilder<T> source(Single<T> single) {
            this.observable = single.toObservable();
            return this;
        }

        public RxTaskBuilder<T> source(Flowable<T> flowable) {
            this.observable = flowable.toObservable();
            return this;
        }

        public RxTaskBuilder<T> onNext(Consumer<T> onNext) {
            this.onNext = onNext;
            return this;
        }

        public RxTaskBuilder<T> onError(Consumer<Throwable> onError) {
            this.onError = onError;
            return this;
        }

        public RxTaskBuilder<T> onComplete(Action onComplete) {
            this.onComplete = onComplete;
            return this;
        }

        public RxTaskBuilder<T> showLoading() {
            this.showLoading = true;
            return this;
        }

        public RxTaskBuilder<T> showRefresh() {
            this.showRefresh = true;
            return this;
        }

        public Disposable execute() {
            if (observable == null) {
                throw new IllegalStateException("Observable source cannot be null");
            }

            if (onNext == null) {
                onNext = data -> { /* default empty */ };
            }

            Consumer<Throwable> errorHandler = onError != null ? onError : exceptionHandler::handle;

            if (showLoading) {
                _showLoadingDialogLiveData.setValue(true);
                _stateLiveData.setValue(ViewState.LOADING);
            }

            if (showRefresh) {
                showRefresh();
            }

            Observable<T> chain = observable
                    .subscribeOn(Schedulers.io())
                    .observeOn(AndroidSchedulers.mainThread())
                    .doOnSubscribe(d -> {
                        if (showLoading) {
                            _showLoadingDialogLiveData.setValue(true);
                            _stateLiveData.setValue(ViewState.LOADING);
                        }
                    })
                    .doFinally(() -> {
                        if (showLoading) {
                            _showLoadingDialogLiveData.setValue(false);
                        }
                        if (showRefresh) {
                            closeRefresh();
                        }
                    });

            Disposable disposable;
            if (onComplete != null) {
                disposable = chain.subscribe(onNext, errorHandler, onComplete);
            } else {
                disposable = chain.subscribe(onNext, errorHandler);
            }

            compositeDisposable.add(disposable);
            return disposable;
        }
    }

    /**
     * 创建一个新的RxJava任务构建器
     */
    public <T> RxTaskBuilder<T> task() {
        return new RxTaskBuilder<>();
    }

    /**
     * 创建一个新的Completable任务构建器
     */
    public RxTaskBuilder<Object> completableTask(Completable completable) {
        return task().source(completable.toObservable());
    }

    /**
     * 添加一个带生命周期感知的订阅
     */
    public <T> void addWithLifecycle(LifecycleOwner owner, Observable<T> observable, Consumer<T> onNext) {
        Disposable disposable = observable
                .subscribeOn(Schedulers.io())
                .observeOn(AndroidSchedulers.mainThread())
                .subscribe(onNext, exceptionHandler::handle);

        owner.getLifecycle().addObserver(new LifecycleEventObserver() {
            @Override
            public void onStateChanged(@NonNull LifecycleOwner source, @NonNull Lifecycle.Event event) {
                if (event == Lifecycle.Event.ON_DESTROY) {
                    if (!disposable.isDisposed()) {
                        disposable.dispose();
                    }
                    owner.getLifecycle().removeObserver(this);
                }
            }
        });

        compositeDisposable.add(disposable);
    }

    /**
     * 添加一个Disposable到CompositeDisposable
     */
    public void addDisposable(@NonNull Disposable disposable) {
        compositeDisposable.add(disposable);
    }

    /**
     * 处理HTTP异常
     */
    private void handleHttpException(HttpException httpException) {
        if (httpException.getCause() instanceof SSLHandshakeException) {
            getExceptionLiveData().setValue(new Pair<>(httpException.code(), SeafException.NETWORK_SSL_EXCEPTION));
        } else {
            getExceptionLiveData().setValue(new Pair<>(httpException.code(), SeafException.NETWORK_EXCEPTION));
        }
    }

    /**
     * 处理IO异常
     */
    private void handleIOException(IOException ioException) {
        getSeafExceptionLiveData().setValue(new SeafException(SeafException.NETWORK_EXCEPTION.getCode(), ioException.getMessage()));
    }

    /**
     * 生成请求体
     */
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

    /**
     * 显示刷新状态
     */
    public void showRefresh() {
        getRefreshLiveData().setValue(true);
    }

    /**
     * 关闭刷新状态
     */
    public void closeRefresh() {
        getRefreshLiveData().setValue(false);
    }

    /**
     * 清除所有订阅
     */
    public void clearAll() {
        SLogs.d("CompositeDisposable current size: " + compositeDisposable.size());
        compositeDisposable.clear();
        SLogs.d("CompositeDisposable clear all: " + compositeDisposable.size());
    }

    @Override
    protected void onCleared() {
        super.onCleared();
        SLogs.d("onCleared");
        clearAll();
    }

    // Getter方法
    public LiveData<ViewState> getStateLiveData() {
        return _stateLiveData;
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

    public MutableLiveData<Boolean> getShowLoadingDialogLiveData() {
        return _showLoadingDialogLiveData;
    }
}
