package com.seafile.seadroid2.framework.http;

import com.blankj.utilcode.util.NetworkUtils;
import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.ssl.SSLTrustManager;

import java.io.File;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.TimeUnit;

import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLSocketFactory;
import javax.net.ssl.TrustManager;
import javax.net.ssl.X509TrustManager;

import okhttp3.Cache;
import okhttp3.CacheControl;
import okhttp3.ConnectionSpec;
import okhttp3.Interceptor;
import okhttp3.OkHttpClient;
import okhttp3.Request;
import okhttp3.Response;
import retrofit2.Converter;
import retrofit2.Retrofit;
import retrofit2.adapter.rxjava2.RxJava2CallAdapterFactory;

public abstract class BaseIO {

    private final int DEFAULT_TIME_OUT = 120000;
    private final File cachePath = SeadroidApplication.getAppContext().getCacheDir();

    //cache path
    final File httpCacheDirectory = new File(cachePath, "cache");

    //20M
    private static final long MAX_CACHE_SIZE = 20 * 1024 * 1024L;

    private final Cache cache;

    //8h
    private static final int MAX_STALE = 60 * 60 * 8;

    //10min
    private static final int MAX_AGE = 600;

    private OkHttpClient okHttpClient = null;

    protected static final ConcurrentMap<String, Retrofit> CLIENT_MAP = new ConcurrentHashMap<>();

    public BaseIO() {
        this.cache = new Cache(httpCacheDirectory, MAX_CACHE_SIZE);
    }

    public abstract String getServerUrl();

    public abstract Converter.Factory getConverterFactory();

    public abstract List<Interceptor> getInterceptors();

    public abstract Account getAccount();

    public <T> T execute(Class<T> clazz) {
        Retrofit retrofit = getRetrofit(getAccount(), false);
        return retrofit.create(clazz);
    }

    public <T> T execute(Class<T> clazz, boolean isForceCreateClient) {
        Retrofit retrofit = getRetrofit(getAccount(), isForceCreateClient);
        return retrofit.create(clazz);
    }

    //cache interceptor
    private final Interceptor REWRITE_CACHE_CONTROL_INTERCEPTOR = chain -> {
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

    /**
     * @param isForceCreate reset retrofit/okhttp client(current account) if true
     */
    private Retrofit getRetrofit(Account account, boolean isForceCreate) {
        if (account == null) {
            throw new IllegalStateException("account is null.");
        }

        if (!isForceCreate) {
            if (CLIENT_MAP.containsKey(account.getSignature())) {
                return CLIENT_MAP.get(account.getSignature());
            }
        }

        return createRetrofit(account);
    }

    private Retrofit createRetrofit(Account account) {
        Retrofit.Builder rBuilder = new Retrofit.Builder();
        rBuilder.baseUrl(getServerUrl());
        rBuilder.addConverterFactory(getConverterFactory());
        rBuilder.addCallAdapterFactory(RxJava2CallAdapterFactory.create());

        rBuilder.client(getClient());
        Retrofit retrofit = rBuilder.build();

        CLIENT_MAP.put(account.getSignature(), retrofit);

        return retrofit;
    }

    final TrustManager[] trustAllCerts = new TrustManager[]{
            new X509TrustManager() {
                @Override
                public void checkClientTrusted(java.security.cert.X509Certificate[] chain, String authType) throws CertificateException {
                }

                @Override
                public void checkServerTrusted(java.security.cert.X509Certificate[] chain, String authType) throws CertificateException {
                }

                @Override
                public java.security.cert.X509Certificate[] getAcceptedIssuers() {
                    return new X509Certificate[]{};
                }
            }
    };

    public OkHttpClient getClient() {
        if (okHttpClient == null) {
            synchronized (BaseIO.class) {
                if (okHttpClient == null) {
                    OkHttpClient.Builder builder = new OkHttpClient.Builder();

                    try {
                        // Install the all-trusting trust manager
                        final SSLContext sslContext = SSLContext.getInstance("TLS");
                        sslContext.init(null, trustAllCerts, new java.security.SecureRandom());
                        // Create an ssl socket factory with our all-trusting manager
                        final SSLSocketFactory sslSocketFactory = sslContext.getSocketFactory();

                        builder.sslSocketFactory(sslSocketFactory, (X509TrustManager) trustAllCerts[0]);
                    } catch (Exception e) {
                        e.printStackTrace();
                    }

                    builder.connectionSpecs(Arrays.asList(
                            ConnectionSpec.MODERN_TLS,
                            ConnectionSpec.COMPATIBLE_TLS,
                            ConnectionSpec.CLEARTEXT));
                    builder.cache(cache);

                    //cache control
                    builder.interceptors().add(REWRITE_CACHE_CONTROL_INTERCEPTOR);
                    builder.networkInterceptors().add(REWRITE_CACHE_CONTROL_INTERCEPTOR);

                    //add interceptors
                    List<Interceptor> interceptors = getInterceptors();
                    if (interceptors != null && !interceptors.isEmpty()) {
                        for (Interceptor i : interceptors) {
                            builder.interceptors().add(i);
                        }
                    }

                    //timeout
                    builder.writeTimeout(DEFAULT_TIME_OUT, TimeUnit.MILLISECONDS);
                    builder.readTimeout(DEFAULT_TIME_OUT, TimeUnit.MILLISECONDS);
                    builder.connectTimeout(DEFAULT_TIME_OUT, TimeUnit.MILLISECONDS);

                    okHttpClient = builder.build();
                }
            }
        }
        return okHttpClient;
    }

}
