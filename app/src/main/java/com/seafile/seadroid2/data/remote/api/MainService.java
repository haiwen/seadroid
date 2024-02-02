package com.seafile.seadroid2.data.remote.api;

import com.seafile.seadroid2.data.model.server.ServerInfoModel;

import io.reactivex.Single;
import retrofit2.http.GET;

public interface MainService {

    @GET("api2/server-info/")
    Single<ServerInfoModel> getServerInfo();

}
