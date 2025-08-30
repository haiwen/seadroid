package com.seafile.seadroid2.baseviewmodel;

import java.util.HashMap;
import java.util.Map;

import io.reactivex.functions.Consumer;

public class ExceptionHandler {
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
                // 处理异常
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
                // 处理异常
            }
        } else if (defaultHandler != null) {
            try {
                defaultHandler.accept(throwable);
            } catch (Exception e) {
                // 处理异常
            }
        }
    }
}
