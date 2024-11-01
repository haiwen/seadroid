package com.seafile.seadroid2.ui.sdoc;

import com.seafile.seadroid2.framework.data.model.sdoc.MetadataConfigModel;
import com.seafile.seadroid2.framework.data.model.sdoc.SDocCommentWrapperModel;
import com.seafile.seadroid2.framework.data.model.sdoc.SDocDetailModel;
import com.seafile.seadroid2.framework.data.model.sdoc.SDocRecordWrapperModel;
import com.seafile.seadroid2.framework.data.model.sdoc.SDocWrapperModel;
import com.seafile.seadroid2.framework.data.model.user.UserWrapperModel;

import io.reactivex.Single;
import retrofit2.http.GET;
import retrofit2.http.Path;
import retrofit2.http.Query;

public interface SDocService {
    @GET("api2/repos/{repo_id}/file/detail/")
    Single<SDocDetailModel> getFileDetail(@Path("repo_id") String repoId, @Query("p") String path);

    @GET("api/v2.1/repos/{repo_id}/related-users/")
    Single<UserWrapperModel> getRelatedUsers(@Path("repo_id") String repoId);

    @GET("api/v2.1/repos/{repo_id}/metadata/")
    Single<MetadataConfigModel> getMetadata(@Path("repo_id") String repoId);

    @GET("api/v2.1/repos/{repo_id}/metadata/record/")
    Single<SDocRecordWrapperModel> getRecords(@Path("repo_id") String repoId, @Query("parent_dir") String parentDir, @Query("name") String name);

    //
    @GET("api/v1/docs/{uuid}/comment/")
    Single<SDocCommentWrapperModel> getComments(@Path("uuid") String uuid);

    @GET("api/v1/docs/{uuid}/")
    Single<SDocWrapperModel> getElements(@Path("uuid") String uuid);
}
