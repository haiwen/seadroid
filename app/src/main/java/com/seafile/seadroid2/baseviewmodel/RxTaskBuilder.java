package com.seafile.seadroid2.baseviewmodel;

import io.reactivex.Observable;
import io.reactivex.disposables.Disposable;
import io.reactivex.functions.Action;
import io.reactivex.functions.Consumer;

public class RxTaskBuilder<T> {
    private final BaseViewModel viewModel;
    private Observable<T> observable;
    private Consumer<T> onNext;
    private Consumer<Throwable> onError;
    private Action onComplete;
    private boolean showLoading = false;
    private boolean showRefresh = false;

    public RxTaskBuilder(BaseViewModel viewModel) {
        this.viewModel = viewModel;
    }

    public RxTaskBuilder<T> source(Observable<T> observable) {
        this.observable = observable;
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
        return null;
    }
}
