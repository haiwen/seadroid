package com.seafile.seadroid2.ui.star;

import com.seafile.seadroid2.framework.data.model.ResultModel;
import com.seafile.seadroid2.framework.data.model.repo.Dirent2Model;
import com.seafile.seadroid2.framework.data.model.star.StarredWrapperModel;

import java.util.Map;

import io.reactivex.Single;
import okhttp3.RequestBody;
import retrofit2.http.Body;
import retrofit2.http.DELETE;
import retrofit2.http.GET;
import retrofit2.http.Multipart;
import retrofit2.http.POST;
import retrofit2.http.PartMap;
import retrofit2.http.Query;

public interface StarredService {
    @GET("api/v2.1/starred-items/")
    Single<StarredWrapperModel> getStarItems();

    @Multipart
    @POST("api/v2.1/starred-items/")
    Single<Dirent2Model> star(@PartMap Map<String, RequestBody> map);

    @DELETE("api/v2.1/starred-items/")
    Single<ResultModel> unStar(@Query("repo_id") String repoId, @Query("path") String path);

}
