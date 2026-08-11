package com.seafile.seadroid2.ui.repo;

import com.seafile.seadroid2.framework.db.entities.GroupEntity;
import com.seafile.seadroid2.framework.model.ResultModel;
import com.seafile.seadroid2.framework.model.dirents.DirentRecursiveModel;
import com.seafile.seadroid2.framework.model.permission.PermissionListWrapperModel;
import com.seafile.seadroid2.framework.model.permission.PermissionWrapperModel;
import com.seafile.seadroid2.framework.model.repo.DirentWrapperModel;
import com.seafile.seadroid2.framework.model.repo.RepoInfoModel;
import com.seafile.seadroid2.framework.model.repo.RepoWrapperModel;

import java.util.List;

import io.reactivex.Single;
import retrofit2.Call;
import retrofit2.http.DELETE;
import retrofit2.http.GET;
import retrofit2.http.Path;
import retrofit2.http.Query;

public interface RepoService {
    @GET("api/v2.1/groups/?with_repos=0")
    Single<List<GroupEntity>> getGroupsAsync();

    @GET("api/v2.1/repos/")
    Single<RepoWrapperModel> getReposAsync();

    @GET("api/v2.1/repos/")
    Call<RepoWrapperModel> getReposSync();

    @GET("api2/repos/{repo_id}/")
    Single<RepoInfoModel> getRepoInfo(@Path("repo_id") String repoId);

    @GET("api/v2.1/repos/{repo_id}/dir/?with_thumbnail=true")
    Single<DirentWrapperModel> getDirentsAsync(@Path("repo_id") String repoId, @Query("p") String path);

    @GET("api/v2.1/repos/{repo_id}/dir/?with_thumbnail=true")
    Call<DirentWrapperModel> getDirentsSync(@Path("repo_id") String repoId, @Query("p") String path);

    @GET("api2/repos/{repo_id}/dir/?t=f&recursive=1")
    Call<List<DirentRecursiveModel>> getDirRecursiveFileCall(@Path("repo_id") String repoId, @Query("p") String path);

    /**
     * @param t type, 'f' for file, 'd' for dir
     *
     */
    @GET("api2/repos/{repo_id}/dir/?recursive=1")
    Single<List<DirentRecursiveModel>> getRecursiveDirOrFileSingle(@Path("repo_id") String repoId, @Query("p") String path, @Query("t") String t);

    @GET("api/v2.1/repos/{repo_id}/custom-share-permissions/{permission_id}/")
    Single<PermissionWrapperModel> getCustomSharePermissionById(@Path("repo_id") String repoId, @Path("permission_id") int id);

    @GET("api/v2.1/repos/{repo_id}/custom-share-permissions/")
    Single<PermissionListWrapperModel> getCustomSharePermissions(@Path("repo_id") String repoId);

    //    https://dev.seafile.com/seahub/api2/beshared-repos/c897035a-a521-4b4e-be89-c9437a7ac968/?share_type=personal&from=ed812a0629bf40bc80526a4ef3216dae%40auth.local
    @DELETE("api2/beshared-repos/{repo_id}/")
    Single<ResultModel> deleteRepoShare(@Path("repo_id") String repoId, @Query("from") String from,@Query("share_type") String shareType);
}
