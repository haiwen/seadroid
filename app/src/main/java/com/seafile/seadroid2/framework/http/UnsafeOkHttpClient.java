
package com.seafile.seadroid2.framework.http;

import com.blankj.utilcode.util.CollectionUtils;
import com.seafile.seadroid2.account.Account;

import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.TimeUnit;

import javax.net.ssl.HostnameVerifier;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLSession;
import javax.net.ssl.SSLSocketFactory;
import javax.net.ssl.TrustManager;
import javax.net.ssl.X509TrustManager;

import okhttp3.ConnectionSpec;
import okhttp3.Interceptor;
import okhttp3.OkHttpClient;

public class UnsafeOkHttpClient extends BaseOkHttpClient {
    public UnsafeOkHttpClient() {
        super(null);
    }

    public UnsafeOkHttpClient(Account account) {
        super(account);
    }

    private final TrustManager[] trustAllCerts = new TrustManager[]{new X509TrustManager() {
        @Override
        public void checkClientTrusted(X509Certificate[] chain, String authType) throws CertificateException {
        }

        @Override
        public void checkServerTrusted(X509Certificate[] chain, String authType) throws CertificateException {
        }

        @Override
        public X509Certificate[] getAcceptedIssuers() {
            return new X509Certificate[]{};
        }
    }
    };

    public OkHttpClient.Builder getBuilder() {
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
        builder.hostnameVerifier(new HostnameVerifier() {
            @Override
            public boolean verify(String hostname, SSLSession session) {
                return true;
            }
        });
        //cache control
//        builder.interceptors().add(REWRITE_CACHE_CONTROL_INTERCEPTOR);
//        builder.networkInterceptors().add(REWRITE_CACHE_CONTROL_INTERCEPTOR);

        //add interceptors
        List<Interceptor> interceptors = getInterceptors();
        if (!CollectionUtils.isEmpty(interceptors)) {
            for (Interceptor i : interceptors) {
                builder.interceptors().add(i);
            }
        }

        //timeout
        builder.writeTimeout(DEFAULT_TIME_OUT, TimeUnit.MILLISECONDS);
        builder.readTimeout(DEFAULT_TIME_OUT, TimeUnit.MILLISECONDS);
        builder.connectTimeout(DEFAULT_TIME_OUT, TimeUnit.MILLISECONDS);

        return builder;
    }

    public OkHttpClient getOkClient() {
        OkHttpClient.Builder builder = getBuilder();
        return builder.build();
    }


}