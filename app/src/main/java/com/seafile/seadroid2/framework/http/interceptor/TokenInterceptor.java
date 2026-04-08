package com.seafile.seadroid2.framework.http.interceptor;

import android.text.TextUtils;

import androidx.annotation.NonNull;

import com.seafile.seadroid2.framework.http.HttpIO;
import com.seafile.seadroid2.framework.http.HttpManager;

import java.io.IOException;

import okhttp3.Interceptor;
import okhttp3.Request;
import okhttp3.Response;

public class TokenInterceptor implements Interceptor {
    private String authToken = null;

    /**
     * user default account token
     * */
    public TokenInterceptor() {

    }

    /**
     * user custom account token
     * */
    public TokenInterceptor(String authToken) {
        this.authToken = authToken;
    }

    @NonNull
    @Override
    public Response intercept(Chain chain) throws IOException {
        return chain.proceed(initBuilder(chain.request().newBuilder()).build());
    }

    private Request.Builder initBuilder(Request.Builder builder) {
//        builder.addHeader("Content-Type", "application/json");
        builder.addHeader("Accept", "application/json");
        builder.addHeader("charset", "utf-8");
        builder.addHeader("timestamp", String.valueOf(System.currentTimeMillis()));

        //
        if (TextUtils.isEmpty(authToken)) {
            authToken = HttpManager.getCurrentHttp().getCurrentToken();
        }

        if (!TextUtils.isEmpty(authToken)) {
            builder.addHeader("Authorization", "Token " + authToken);
        }

        return builder;
    }
}
