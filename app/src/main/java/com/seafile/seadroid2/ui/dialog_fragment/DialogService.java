package com.seafile.seadroid2.ui.dialog_fragment;

import com.seafile.seadroid2.data.model.ResultModel;
import com.seafile.seadroid2.data.model.dirents.DeleteDirentModel;
import com.seafile.seadroid2.data.model.dirents.FileCreateModel;
import com.seafile.seadroid2.data.db.entities.RepoModel;

import java.util.Map;

import io.reactivex.Observable;
import io.reactivex.Single;
import okhttp3.RequestBody;
import retrofit2.http.Body;
import retrofit2.http.DELETE;
import retrofit2.http.Multipart;
import retrofit2.http.POST;
import retrofit2.http.PartMap;
import retrofit2.http.Path;
import retrofit2.http.Query;

public interface DialogService {

    @Multipart
    @POST("api2/repos/")
    Single<RepoModel> createRepo(@PartMap Map<String, RequestBody> map);


    @DELETE("api/v2.1/repos/{repo_id}/")
    Single<String> deleteRepo(@Path("repo_id") String repoId);

    @Multipart
    @POST("api2/repos/{repo_id}/dir/")
    Single<String> createDir(@Path("repo_id") String repoId, @Query("p") String path, @PartMap Map<String, RequestBody> map);

    @Multipart
    @POST("api/v2.1/repos/{repo_id}/file/")
    Single<FileCreateModel> createFile(@Path("repo_id") String repoId, @Query("p") String fileName, @PartMap Map<String, RequestBody> map);

    @Multipart
    @POST("api2/repos/{repo_id}/?op=rename")
    Single<String> renameRepo(@Path("repo_id") String repoId, @PartMap Map<String, RequestBody> map);

    @Multipart
    @POST("api2/repos/{repo_id}/dir/")
    Single<String> renameDir(@Path("repo_id") String repoId, @Query("p") String path, @PartMap Map<String, RequestBody> map);

    @Multipart
    @POST("api/v2.1/repos/{repo_id}/file/")
    Single<FileCreateModel> renameFile(@Path("repo_id") String repoId, @Query("p") String path, @PartMap Map<String, RequestBody> map);

    @Multipart
    @POST("api/v2.1/repos/{repo_id}/set-password/")
    Single<ResultModel> setPassword(@Path("repo_id") String repoId, @PartMap Map<String, RequestBody> map);

    @DELETE("api/v2.1/repos/{repo_id}/dir/")
    Observable<DeleteDirentModel> deleteDir(@Path("repo_id") String repoId, @Query("p") String path);

    @DELETE("api/v2.1/repos/{repo_id}/file/")
    Observable<DeleteDirentModel> deleteFile(@Path("repo_id") String repoId, @Query("p") String path);

    @POST("api/v2.1/repos/sync-batch-move-item/")
    Single<ResultModel> moveDirents(@Body Map<String, Object> map);

    @POST("api/v2.1/repos/sync-batch-copy-item/")
    Single<ResultModel> copyDirents(@Body Map<String, Object> map);
}
