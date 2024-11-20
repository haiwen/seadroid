package com.seafile.seadroid2.ui.repo;

import com.seafile.seadroid2.framework.data.db.entities.PermissionEntity;
import com.seafile.seadroid2.framework.data.db.entities.RepoModel;
import com.seafile.seadroid2.framework.data.model.ResultModel;
import com.seafile.seadroid2.framework.data.model.permission.PermissionListWrapperModel;
import com.seafile.seadroid2.framework.data.model.permission.PermissionWrapperModel;
import com.seafile.seadroid2.framework.data.model.repo.Dirent2Model;
import com.seafile.seadroid2.framework.data.model.repo.DirentWrapperModel;
import com.seafile.seadroid2.framework.data.model.repo.RepoWrapperModel;

import java.security.Permission;
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


    @GET("api/v2.1/repos/{repo_id}/dir/?with_thumbnail=true")
    Call<DirentWrapperModel> getDirentsSync(@Path("repo_id") String repoId, @Query("p") String path);

    @GET("api/v2.1/repos/{repo_id}/custom-share-permissions/{permission_id}/")
    Single<PermissionWrapperModel> getCustomSharePermissionById(@Path("repo_id") String repoId, @Path("permission_id") int id);

    @GET("api/v2.1/repos/{repo_id}/custom-share-permissions/")
    Single<PermissionListWrapperModel> getCustomSharePermissions(@Path("repo_id") String repoId);
}
