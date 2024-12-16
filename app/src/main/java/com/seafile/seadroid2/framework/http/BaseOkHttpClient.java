package com.seafile.seadroid2.framework.http;

import android.text.TextUtils;

import com.blankj.utilcode.util.NetworkUtils;
import com.seafile.seadroid2.BuildConfig;
import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.framework.http.interceptor.HeaderInterceptor;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import okhttp3.Cache;
import okhttp3.CacheControl;
import okhttp3.Interceptor;
import okhttp3.OkHttpClient;
import okhttp3.Request;
import okhttp3.Response;
import okhttp3.logging.HttpLoggingInterceptor;

public abstract class BaseOkHttpClient {
    protected final int DEFAULT_TIME_OUT = 120000;
    protected final int MAX_AGE = 600;
    protected final int MAX_STALE = 60 * 60 * 8;
    protected final long MAX_CACHE_SIZE = 20 * 1024 * 1024L;

    protected final Cache cache;
    protected final File cachePath = SeadroidApplication.getAppContext().getCacheDir();

    //cache path
    final File httpCacheDirectory = new File(cachePath, "cache");

    protected Account account;

    public BaseOkHttpClient(Account account) {
        this.account = account;
        this.cache = new Cache(httpCacheDirectory, MAX_CACHE_SIZE);
    }

    public abstract OkHttpClient getOkClient();

    protected List<Interceptor> getInterceptors() {

        List<Interceptor> interceptors = getInterceptorsWithoutToken();
        if (account != null && !TextUtils.isEmpty(account.token)) {
            interceptors.add(new HeaderInterceptor(account.token));
        }

        return interceptors;
    }

    protected List<Interceptor> getInterceptorsWithoutToken() {

        List<Interceptor> interceptors = new ArrayList<>();

        //print log
        HttpLoggingInterceptor loggingInterceptor = new HttpLoggingInterceptor();
        loggingInterceptor.setLevel(HttpLoggingInterceptor.Level.BASIC);
        loggingInterceptor.setLevel(BuildConfig.DEBUG ? HttpLoggingInterceptor.Level.BODY : HttpLoggingInterceptor.Level.BASIC);
        interceptors.add(loggingInterceptor);

//        interceptors.add(new AddCookiesInterceptor());
//        interceptors.add(new ReceivedCookiesInterceptor());

        return interceptors;
    }


    //cache interceptor
    protected final Interceptor REWRITE_CACHE_CONTROL_INTERCEPTOR = chain -> {
        Request request = chain.request();

        boolean isConnected = NetworkUtils.isConnected();
        if (!isConnected) {
            //no network,use cache data
            request = request.newBuilder().cacheControl(CacheControl.FORCE_CACHE).build();
        } else {
            request = request.newBuilder().cacheControl(CacheControl.FORCE_NETWORK).build();
        }

        Response originalResponse = chain.proceed(request);
        if (isConnected) {
            return originalResponse.newBuilder()
                    .removeHeader("Pragma")
                    .removeHeader("Cache-Control")
                    .header("Cache-Control", "public, max-age=" + MAX_AGE)
                    .build();
        } else {
            return originalResponse.newBuilder()
                    .removeHeader("Pragma")
                    .removeHeader("Cache-Control")
                    .header("Cache-Control", "public, only-if-cached, max-stale=" + MAX_STALE)
                    .build();
        }
    };
}
