package com.seafile.seadroid2.ui.media.player;

import io.reactivex.Single;
import retrofit2.http.GET;
import retrofit2.http.Path;
import retrofit2.http.Query;

public interface PlayerApiService {
    @GET("api2/repos/{repo_id}/file/")
    Single<String> getFileLink(@Path("repo_id") String repoId, @Query("p") String p, @Query("op") String op, @Query("reuse") int reuse);

}
