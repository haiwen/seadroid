package com.seafile.seadroid2.framework.http;

import com.blankj.utilcode.util.CollectionUtils;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.ssl.SSLTrustManager;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.TimeUnit;

import javax.net.ssl.SSLSocketFactory;
import javax.net.ssl.TrustManager;
import javax.net.ssl.X509TrustManager;

import okhttp3.ConnectionSpec;
import okhttp3.Interceptor;
import okhttp3.OkHttpClient;
import okhttp3.Protocol;

public class SafeOkHttpClient extends BaseOkHttpClient {
    private final List<Interceptor> _interceptors = new ArrayList<>();

    public SafeOkHttpClient(Account account) {
        super(account);

        _interceptors.addAll(getInterceptors());
    }


    public OkHttpClient getOkClient() {
        return getOkClient(false);
    }

    public OkHttpClient getOkClient(boolean isForceUseHttp_1_1) {
        OkHttpClient.Builder builder = getOkClientBuilder();
        if (isForceUseHttp_1_1) {
            ArrayList<Protocol> protocols = new ArrayList<>();
            protocols.add(Protocol.HTTP_1_1);
            builder.protocols(protocols);
        }
        return builder.build();
    }

    public OkHttpClient getOkClient(List<Protocol> protocols) {
        OkHttpClient.Builder builder = getOkClientBuilder();
        if (!CollectionUtils.isEmpty(protocols)) {
            builder.protocols(protocols);
        }
        return builder.build();
    }

    private OkHttpClient.Builder getOkClientBuilder() {
        OkHttpClient.Builder builder = new OkHttpClient.Builder();

        //https
        if (specialAccount.getServer().startsWith("https://")) {
            //ssl
            SSLSocketFactory factory = SSLTrustManager.instance().getSSLSocketFactory(specialAccount);
            TrustManager[] trustManagers = SSLTrustManager.instance().getTrustManagers(specialAccount);
            X509TrustManager trustManager = (X509TrustManager) trustManagers[0];

            builder.sslSocketFactory(factory, trustManager);
            // Use OkHttp's default OkHostnameVerifier for proper hostname validation.
            // SSLTrustManager.SecureX509TrustManager handles certificate trust (including
            // user-approved self-signed certs via customCheck()), and OkHttp's built-in
            // verifier ensures the hostname matches the certificate's CN/SAN.

            // HTTPS: only enable MODERN_TLS (TLS 1.2+).
            // TLS 1.0/1.1 have been removed since Android 14 (API 34).
            builder.connectionSpecs(List.of(ConnectionSpec.MODERN_TLS));
        } else {
            // HTTP: allow cleartext
            builder.connectionSpecs(List.of(ConnectionSpec.CLEARTEXT));
        }

        builder.cache(cache);

        //add interceptors
        if (!CollectionUtils.isEmpty(_interceptors)) {
            for (Interceptor i : _interceptors) {
                builder.interceptors().add(i);
            }
        }

        //timeout
        builder.writeTimeout(DEFAULT_TIME_OUT, TimeUnit.MILLISECONDS);
        builder.readTimeout(DEFAULT_TIME_OUT, TimeUnit.MILLISECONDS);
        builder.connectTimeout(DEFAULT_TIME_OUT, TimeUnit.MILLISECONDS);
        return builder;
    }


}
