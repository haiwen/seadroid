package com.seafile.seadroid2.ui.main;

import com.seafile.seadroid2.framework.data.model.server.ServerInfoModel;

import io.reactivex.Single;
import retrofit2.Call;
import retrofit2.http.GET;

public interface MainService {

    @GET("api2/server-info/")
    Single<ServerInfoModel> getServerInfo();

    @GET("api2/server-info/")
    Call<ServerInfoModel> getServerInfoSync();
}
