package com.seafile.seadroid2.ui.account;

import com.seafile.seadroid2.account.AccountInfo;
import com.seafile.seadroid2.framework.model.SSOLinkModel;
import com.seafile.seadroid2.framework.model.SSOStatusModel;
import com.seafile.seadroid2.framework.model.TokenModel;

import java.util.Map;

import io.reactivex.Single;

import okhttp3.RequestBody;
import retrofit2.Call;
import retrofit2.http.GET;
import retrofit2.http.HeaderMap;
import retrofit2.http.Multipart;
import retrofit2.http.POST;
import retrofit2.http.PartMap;
import retrofit2.http.Path;

public interface AccountService {


    @POST("api2/device-wiped//")
    @Multipart
    Single<Object> deviceWiped();

    @POST("api2/auth-token/")
    @Multipart
    Call<TokenModel> login(@HeaderMap Map<String, String> headers, @PartMap Map<String, RequestBody> map);

    @GET("api2/account/info/")
    Single<AccountInfo> getAccountInfo();

    @GET("api2/account/info/")
    Call<AccountInfo> getAccountInfoCall();

    @POST("api2/client-sso-link/")
    Single<SSOLinkModel> getSsoLink();

    @GET("api2/client-sso-link/{token}/")
    Single<SSOStatusModel> getSsoStatus(@Path("token") String token);
}
