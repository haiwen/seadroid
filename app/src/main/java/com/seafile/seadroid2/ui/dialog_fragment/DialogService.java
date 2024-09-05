package com.seafile.seadroid2.ui.dialog_fragment;

import com.seafile.seadroid2.framework.data.model.ResultModel;
import com.seafile.seadroid2.framework.data.model.dirents.DeleteDirentModel;
import com.seafile.seadroid2.framework.data.model.dirents.FileCreateModel;
import com.seafile.seadroid2.framework.data.db.entities.RepoModel;
import com.seafile.seadroid2.framework.data.model.objs.DirentShareLinkModel;

import java.util.List;
import java.util.Map;

import io.reactivex.Single;
import okhttp3.RequestBody;
import retrofit2.http.Body;
import retrofit2.http.DELETE;
import retrofit2.http.GET;
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

    @DELETE("api/v2.1/repos/{repo_id}/{obj}/")
    Single<DeleteDirentModel> deleteDirent(@Path("repo_id") String repoId, @Path("obj") String obj, @Query("p") String path);

    @POST("api/v2.1/repos/sync-batch-move-item/")
    Single<ResultModel> moveDirents(@Body Map<String, Object> map);

    @POST("api/v2.1/repos/sync-batch-copy-item/")
    Single<ResultModel> copyDirents(@Body Map<String, Object> map);

    @POST("api/v2.1/multi-share-links/")
    Single<DirentShareLinkModel> createMultiShareLink(@Body Map<String, Object> map);

    @POST("api/v2.1/share-links/")
    Single<DirentShareLinkModel> createShareLink(@Body Map<String, Object> map);

    @GET("api/v2.1/share-links/")
    Single<List<DirentShareLinkModel>> listAllShareLink(@Query("repo_id") String repoId, @Query("path") String path);

    @POST("api/v2.1/upload-links/")
    Single<DirentShareLinkModel> uploadLinks(@Body Map<String, Object> map);

    @GET("api/v2.1/upload-links/")
    Single<List<DirentShareLinkModel>> listUploadLinks(@Query("repo_id") String repo_id, @Query("path") String path);

    @DELETE("api/v2.1/upload-links/{link_id}/")
    Single<Boolean> deleteUploadLink(@Path("link_id") String linkId);
}
