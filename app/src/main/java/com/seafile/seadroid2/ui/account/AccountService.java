package com.seafile.seadroid2.ui.account;

import com.seafile.seadroid2.account.AccountInfo;

import io.reactivex.Single;
import retrofit2.http.GET;

public interface AccountService {
    @GET("api2/account/info/")
    Single<AccountInfo> getAccountInfo();
}
