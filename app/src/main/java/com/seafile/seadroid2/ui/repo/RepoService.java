package com.seafile.seadroid2.ui.repo;

import com.seafile.seadroid2.data.BlockInfoBean;
import com.seafile.seadroid2.data.model.dirents.DirentDirModel;
import com.seafile.seadroid2.data.model.dirents.DirentFileModel;
import com.seafile.seadroid2.data.db.entities.RepoModel;
import com.seafile.seadroid2.data.model.ResultModel;
import com.seafile.seadroid2.data.model.dirents.DirentRecursiveFileModel;
import com.seafile.seadroid2.data.model.repo.DirentMiniModel;
import com.seafile.seadroid2.data.model.repo.DirentWrapperModel;
import com.seafile.seadroid2.data.model.repo.RepoWrapperModel;

import java.util.List;
import java.util.Map;

import io.reactivex.Single;
import okhttp3.RequestBody;
import retrofit2.Call;
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

    @GET("api/v2.1/repos/")
    Call<RepoWrapperModel> getReposCall();


    @GET("api2/repos/{repo_id}/")
    Single<RepoModel> getRepoInfo(@Path("repo_id") String repoId);

    @GET("api/v2.1/repos/{repo_id}/dir/?with_thumbnail=true")
    Single<DirentWrapperModel> getDirents(@Path("repo_id") String repoId, @Query("p") String path);

    @GET("api/v2.1/repos/{repo_id}/dir/?with_thumbnail=true")
    Call<DirentWrapperModel> getDirentsCall(@Path("repo_id") String repoId, @Query("p") String path);

    @Multipart
    @POST("api/v2.1/starred-items/")
    Single<DirentMiniModel> star(@PartMap Map<String, RequestBody> map);

    @DELETE("api/v2.1/starred-items/")
    Single<ResultModel> unStar(@Query("repo_id") String repoId, @Query("path") String path);

    @POST("api2/repos/{repo_id}/dir/")
    @Multipart
    Call<String> mkDirCall(@Path("repo_id") String repoId, @Query("p") String path, @PartMap Map<String, RequestBody> map);

    @POST("api2/repos/{repo_id}/dir/")
    @Multipart
    Call<String> renameDirCall(@Path("repo_id") String repoId, @Query("p") String path, @PartMap Map<String, RequestBody> map);

    @GET("api/v2.1/repos/{repo_id}/dir/detail/")
    Call<DirentDirModel> getDirDetailCall(@Path("repo_id") String repoId, @Query("path") String path);

    @POST("api2/repos/{repo_id}/file/")
    @Multipart
    Call<String> renameFileCall(@Path("repo_id") String repoId, @Query("p") String path, @PartMap Map<String, RequestBody> map);

    @GET("api2/repos/{repo_id}/file/detail/")
    Call<DirentFileModel> getFileDetailCall(@Path("repo_id") String repoId, @Query("p") String path);

    @GET("api2/repos/{repo_id}/dir/?t=f&recursive=1")
    Call<List<DirentRecursiveFileModel>> getDirRecursiveFileCall(@Path("repo_id") String repoId, @Query("p") String path);

    @GET("api2/repos/{repo_id}/file/?op=download")
    Call<String> getFileDownloadLink(@Path("repo_id") String repoId, @Query("p") String path);

    @GET("api2/repos/{repo_id}/upload-link/")
    Call<String> getFileUploadLink(@Path("repo_id") String repoId, @Query("p") String path);

    @GET("api2/repos/{repo_id}/update-link/")
    Call<String> getFileUpdateLink(@Path("repo_id") String repoId);

    @Multipart
    @POST("api2/repos/{repo_id}/upload-blks-link/")
    Call<BlockInfoBean> getFileBlockUploadLink(@Path("repo_id") String repoId, @PartMap Map<String, RequestBody> map);

}
