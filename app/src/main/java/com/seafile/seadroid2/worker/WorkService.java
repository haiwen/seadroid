package com.seafile.seadroid2.worker;

import com.seafile.seadroid2.data.model.transfer.UploadBlksLinkModel;

import java.util.Map;

import io.reactivex.Single;
import okhttp3.RequestBody;
import retrofit2.http.GET;
import retrofit2.http.Multipart;
import retrofit2.http.POST;
import retrofit2.http.PartMap;
import retrofit2.http.Path;
import retrofit2.http.Query;

public interface WorkService {
    @GET("api2/repos/{repo_id}/update-link/")
    Single<String> getUpdateLink(@Path("repo_id") String repoId);

    @GET("api2/repos/{repo_id}/update-link/")
    Single<String> getUploadLink(@Path("repo_id") String repoId, @Query("p") String dir);

    @Multipart
    @POST("api2/repos/{repo_id}/upload-blks-link/")
    Single<UploadBlksLinkModel> getBlockUploadLink(@Path("repo_id") String repoId, @PartMap Map<String, RequestBody> map);
}
