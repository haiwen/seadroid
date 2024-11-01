package com.seafile.seadroid2.framework.http.interceptor;

import android.text.TextUtils;

import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.util.TokenManager;

import java.io.IOException;

import okhttp3.Interceptor;
import okhttp3.Request;
import okhttp3.Response;

public class CurrentTokenInterceptor implements Interceptor {
    @Override
    public Response intercept(Chain chain) throws IOException {
        return chain.proceed(initBuilder(chain.request().newBuilder()).build());
    }

    private Request.Builder initBuilder(Request.Builder builder) {
        builder.addHeader("Content-Type", "application/json");
        builder.addHeader("Accept", "application/json");
        builder.addHeader("charset", "utf-8");
        builder.addHeader("timestamp", String.valueOf(System.currentTimeMillis()));

        String authToken = TokenManager.getInstance().getToken();
        if (!TextUtils.isEmpty(authToken)) {
            builder.addHeader("Authorization", "Token " + authToken);
        }
        return builder;
    }
}
