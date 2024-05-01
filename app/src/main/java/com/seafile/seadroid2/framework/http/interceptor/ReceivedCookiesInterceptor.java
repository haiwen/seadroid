package com.seafile.seadroid2.framework.http.interceptor;

import java.io.IOException;
import java.util.HashSet;

import okhttp3.Interceptor;
import okhttp3.Response;

public class ReceivedCookiesInterceptor implements Interceptor {
    @Override
    public Response intercept(Chain chain) throws IOException {
        Response response = chain.proceed(chain.request());

//        if (!response.headers("Set-Cookie").isEmpty()) {
//            HashSet<String> cookies = new HashSet<>(response.headers("Set-Cookie"));
//            SPUtils.put(SPContants.KEY_COOKIES, cookies);
//        }
        return response;
    }

}
