package com.seafile.seadroid2.io.http.interceptor;

import android.text.TextUtils;

import java.io.IOException;

import okhttp3.Interceptor;
import okhttp3.Request;
import okhttp3.Response;

public class HeaderInterceptor implements Interceptor {
    private String authToken = null;

    public HeaderInterceptor(String authToken) {
        this.authToken = authToken;
    }

    @Override
    public Response intercept(Chain chain) throws IOException {
        return chain.proceed(initBuilder(chain.request().newBuilder()).build());
    }

    private Request.Builder initBuilder(Request.Builder builder) {
        builder.addHeader("Content-Type", "application/json");
        builder.addHeader("Accept", "application/json");
        builder.addHeader("charset", "utf-8");
        builder.addHeader("timestamp", String.valueOf(System.currentTimeMillis()));

        if (!TextUtils.isEmpty(authToken)) {
            builder.addHeader("Authorization",  "Token " + authToken);
        }

        return builder;
    }
}
