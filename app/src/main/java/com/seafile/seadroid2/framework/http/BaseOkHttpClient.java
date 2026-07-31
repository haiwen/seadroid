package com.seafile.seadroid2.framework.http;

import android.text.TextUtils;

import androidx.annotation.NonNull;

import com.blankj.utilcode.util.NetworkUtils;
import com.seafile.seadroid2.BuildConfig;
import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.framework.http.interceptor.TokenInterceptor;
import com.seafile.seadroid2.framework.util.SafeLogs;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import okhttp3.Cache;
import okhttp3.CacheControl;
import okhttp3.Interceptor;
import okhttp3.Request;
import okhttp3.Response;
import okhttp3.logging.HttpLoggingInterceptor;

public abstract class BaseOkHttpClient {
    protected final int DEFAULT_TIME_OUT = 60000;
    protected final long MAX_CACHE_SIZE = 20 * 1024 * 1024L;

    protected final Cache cache;
    protected final File cachePath = SeadroidApplication.getAppContext().getCacheDir();

    //cache path
    final File httpCacheDirectory = new File(cachePath, "cache");

    protected Account specialAccount;

    public BaseOkHttpClient(Account specialAccount) {
        this.specialAccount = specialAccount;
        this.cache = new Cache(httpCacheDirectory, MAX_CACHE_SIZE);
    }

    protected List<Interceptor> getInterceptors() {
        List<Interceptor> interceptors = new ArrayList<>();

        if (specialAccount != null && !TextUtils.isEmpty(specialAccount.token)) {
            interceptors.add(new TokenInterceptor(specialAccount.token));
        } else {
            interceptors.add(new TokenInterceptor());
        }

        interceptors.addAll(getDefaultInterceptors());

        return interceptors;
    }

    protected List<Interceptor> getDefaultInterceptors() {
        List<Interceptor> interceptors = new ArrayList<>();

        //print log
        HttpLoggingInterceptor loggingInterceptor = new HttpLoggingInterceptor(new HttpLoggingInterceptor.Logger() {
            @Override
            public void log(@NonNull String s) {
                SafeLogs.i(s);
            }
        });
        loggingInterceptor.setLevel(HttpLoggingInterceptor.Level.HEADERS);
        interceptors.add(loggingInterceptor);
        return interceptors;
    }
}
