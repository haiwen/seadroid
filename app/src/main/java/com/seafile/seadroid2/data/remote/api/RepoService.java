package com.seafile.seadroid2.data.remote.api;

import com.seafile.seadroid2.data.model.ResultModel;
import com.seafile.seadroid2.data.model.repo.DirentMiniModel;
import com.seafile.seadroid2.data.model.repo.DirentWrapperModel;
import com.seafile.seadroid2.data.model.repo.RepoWrapperModel;

import java.util.Map;

import io.reactivex.Flowable;
import io.reactivex.Single;
import okhttp3.RequestBody;
import retrofit2.http.DELETE;
import retrofit2.http.GET;
import retrofit2.http.Multipart;
import retrofit2.http.POST;
import retrofit2.http.PartMap;
import retrofit2.http.Path;
import retrofit2.http.Query;

public interface RepoService {
    @GET("api/v2.1/repos/")
    Single<RepoWrapperModel> getRepos();

    @GET("api/v2.1/repos/{repo_id}/dir/?with_thumbnail=true")
    Single<DirentWrapperModel> getDirents(@Path("repo_id") String repoId, @Query("p") String path);

    @Multipart
    @POST("api/v2.1/starred-items/")
    Single<DirentMiniModel> star(@PartMap Map<String, RequestBody> map);

    //https://dev.seafile.com/seahub/api/v2.1/starred-items/?repo_id=a5354c34-118d-4d60-9ed6-a246c6d8148d&path=%2F%E8%BF%99%E6%98%AF%E4%B8%80%E4%B8%AA%E8%B6%85%E7%BA%A7%E8%B6%85%E7%BA%A7%E8%B6%85%E7%BA%A7%E8%B6%85%E7%BA%A7%E8%B6%85%E7%BA%A7%E8%B6%85%E7%BA%A7%E8%B6%85%E7%BA%A7%E8%B6%85%E7%BA%A7%E8%B6%85%E7%BA%A7%E8%B6%85%E7%BA%A7%E9%95%BF%E7%9A%84%E5%90%8D%E5%AD%97
    @DELETE("api/v2.1/starred-items/")
    Single<ResultModel> unStar(@Query("repo_id") String repoId, @Query("path") String path);

}
