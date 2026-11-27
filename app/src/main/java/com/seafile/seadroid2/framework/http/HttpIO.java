package com.seafile.seadroid2.framework.http;

import android.content.ContentResolver;
import android.content.Context;
import android.net.Uri;
import android.text.TextUtils;

import com.blankj.utilcode.util.CloneUtils;
import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.framework.http.converter.ConverterFactory;
import com.seafile.seadroid2.framework.util.ContentResolvers;

import java.io.IOException;
import java.io.InputStream;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

import io.reactivex.Single;
import okhttp3.MediaType;
import okhttp3.MultipartBody;
import okhttp3.Request;
import okhttp3.RequestBody;
import okhttp3.Response;
import okhttp3.ResponseBody;
import retrofit2.Retrofit;
import retrofit2.adapter.rxjava2.RxJava2CallAdapterFactory;

public class HttpIO {
    private final Account account;
    private final String token;
    private final String server;
    private final String email;

    private SafeOkHttpClient safeOkHttpClient;

    protected HttpIO(Account account) {
        if (account == null) {
            throw new IllegalArgumentException("IO constructor(): account is null.");
        }

        if (TextUtils.isEmpty(account.server)) {
            throw new IllegalArgumentException("IO constructor(): account.server is null.");
        }

        if (TextUtils.isEmpty(account.email)) {
            throw new IllegalArgumentException("IO constructor(): account.email is null.");
        }

        this.account = account;
        this.token = account.token;
        this.server = account.server;
        this.email = account.email;
    }


    public Account getAccount() {
        return account;
    }

    public String getCurrentToken() {
        return token;
    }

    public String getCurrentEmail() {
        return email;
    }

    public String getCurrentServer() {
        return server;
    }


    /**
     * Not logged in/Log in to another server
     */
    public static HttpIO getInstanceByAccount(Account account) {
        return HttpManager.getHttpWithAccount(account);
    }

    /**
     * get client
     */
    public SafeOkHttpClient getSafeClient() {
        if (safeOkHttpClient == null) {
            safeOkHttpClient = new SafeOkHttpClient(account);
        }
        return safeOkHttpClient;
    }


    /**
     * When log in again or switch account, you should reset this IO Singleton.
     * <p><b>because it's a SINGLETON, unless kill the APP!<b/></p>
     */
    public static void resetLoggedInInstance() {
        HttpManager.reset();
    }


    public <T> T execute(Class<T> clazz) {
        Retrofit retrofit = createRetrofit();
        return retrofit.create(clazz);
    }

    private Retrofit retrofit;

    private Retrofit createRetrofit() {
        if (retrofit != null) {
            return retrofit;
        }

        Retrofit.Builder rBuilder = new Retrofit.Builder();

        String rawServer = getCurrentServer();
        if (rawServer == null) {
            throw new IllegalArgumentException("Server URL is null");
        }
        String serverUrl = rawServer.trim().replace('：', ':').replaceAll("(?i)^(https?://)\\s+", "$1");
        if (!serverUrl.endsWith("/")) {
            serverUrl = serverUrl + "/";
        }

        rBuilder.baseUrl(serverUrl);
        rBuilder.addConverterFactory(ConverterFactory.create());
        rBuilder.addCallAdapterFactory(RxJava2CallAdapterFactory.create());

//        rBuilder.client(new UnsafeOkHttpClient(account).getOkClient());
        rBuilder.client(getSafeClient().getOkClient());

        retrofit = rBuilder.build();
        return retrofit;
    }
}
