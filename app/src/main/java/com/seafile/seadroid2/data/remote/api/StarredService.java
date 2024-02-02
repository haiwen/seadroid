package com.seafile.seadroid2.data.remote.api;

import com.seafile.seadroid2.data.model.ResultModel;
import com.seafile.seadroid2.data.model.star.StarredWrapperModel;

import io.reactivex.Flowable;
import io.reactivex.Single;
import retrofit2.http.DELETE;
import retrofit2.http.GET;
import retrofit2.http.Query;

public interface StarredService {
    @GET("api/v2.1/starred-items/")
    Single<StarredWrapperModel> getStarItems();

    @DELETE("api/v2.1/starred-items/")
    Single<ResultModel> unStarItem(@Query("repo_id") String repoId, @Query("path") String path);

}
