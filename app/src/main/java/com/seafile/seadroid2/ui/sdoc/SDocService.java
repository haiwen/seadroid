package com.seafile.seadroid2.ui.sdoc;

import com.seafile.seadroid2.framework.model.sdoc.FileDetailModel;
import com.seafile.seadroid2.framework.model.sdoc.FileRecordWrapperModel;
import com.seafile.seadroid2.framework.model.sdoc.FileTagWrapperModel;
import com.seafile.seadroid2.framework.model.sdoc.MetadataConfigModel;
import com.seafile.seadroid2.framework.model.user.ParticipantsWrapperModel;
import com.seafile.seadroid2.framework.model.user.UserWrapperModel;

import io.reactivex.Single;
import retrofit2.http.GET;
import retrofit2.http.Path;
import retrofit2.http.Query;

public interface SDocService {
    @GET("api2/repos/{repo_id}/file/detail/")
    Single<FileDetailModel> getFileDetail(@Path("repo_id") String repoId, @Query("p") String path);

    @GET("api/v2.1/repos/{repo_id}/related-users/")
    Single<UserWrapperModel> getRelatedUsers(@Path("repo_id") String repoId);

    @GET("api/v2.1/seadoc/participants/{doc_uuid}/")
    Single<ParticipantsWrapperModel> getParticipants(@Path("doc_uuid") String doc_uuid);

    @GET("api/v2.1/repos/{repo_id}/metadata/")
    Single<MetadataConfigModel> getMetadata(@Path("repo_id") String repoId);

    @GET("api/v2.1/repos/{repo_id}/metadata/record/")
    Single<FileRecordWrapperModel> getRecords(@Path("repo_id") String repoId, @Query("parent_dir") String parentDir, @Query("name") String name, @Query("file_name") String fileName);

    @GET("api/v2.1/repos/{repo_id}/metadata/tags/?start=0&limit=1000")
    Single<FileTagWrapperModel> getTags(@Path("repo_id") String repoId);

}
