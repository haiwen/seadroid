package com.seafile.seadroid2.io.http.interceptor;

import java.io.IOException;

import okhttp3.Interceptor;
import okhttp3.Request;
import okhttp3.Response;

public class AddCookiesInterceptor implements Interceptor {
    @Override
    public Response intercept(Chain chain) throws IOException {
        Request.Builder builder = chain.request().newBuilder();

//        HashSet<String> preferences = SPUtils.get(SPContants.KEY_COOKIES);
//        if (preferences != null) {
//            for (String cookie : preferences) {
//                builder.addHeader("Cookie", cookie);
//            }
//            SLogs.d("AddCookiesInterceptor Cookie: " + Joiner.on(",").join(preferences));
//        }
        return chain.proceed(builder.build());
    }
}
