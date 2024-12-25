package com.seafile.seadroid2.ui.sdoc;

import com.seafile.seadroid2.framework.data.model.ResultModel;
import com.seafile.seadroid2.framework.data.model.docs_comment.DocsCommentWrapperModel;
import com.seafile.seadroid2.framework.data.model.docs_comment.DocsCommentsWrapperModel;
import com.seafile.seadroid2.framework.data.model.docs_comment.DocsUploadResultModel;
import com.seafile.seadroid2.framework.data.model.sdoc.FileDetailModel;
import com.seafile.seadroid2.framework.data.model.sdoc.FileRecordWrapperModel;
import com.seafile.seadroid2.framework.data.model.sdoc.MetadataConfigModel;
import com.seafile.seadroid2.framework.data.model.sdoc.SDocOutlineWrapperModel;
import com.seafile.seadroid2.framework.data.model.user.UserWrapperModel;

import java.util.Map;

import io.reactivex.Flowable;
import io.reactivex.Single;
import okhttp3.MultipartBody;
import okhttp3.RequestBody;
import retrofit2.http.Body;
import retrofit2.http.DELETE;
import retrofit2.http.GET;
import retrofit2.http.Multipart;
import retrofit2.http.POST;
import retrofit2.http.PUT;
import retrofit2.http.Part;
import retrofit2.http.PartMap;
import retrofit2.http.Path;
import retrofit2.http.Query;

public interface DocsCommentService {
    @GET("api2/repos/{repo_id}/file/detail/")
    Single<FileDetailModel> getFileDetail(@Path("repo_id") String repoId, @Query("p") String path);

    @GET("api/v2.1/repos/{repo_id}/related-users/")
    Single<UserWrapperModel> getRelatedUsers(@Path("repo_id") String repoId);

    @GET("api/v2.1/repos/{repo_id}/metadata/")
    Single<MetadataConfigModel> getMetadata(@Path("repo_id") String repoId);

    @GET("api/v2.1/repos/{repo_id}/metadata/record/")
    Single<FileRecordWrapperModel> getRecords(@Path("repo_id") String repoId, @Query("parent_dir") String parentDir, @Query("name") String name, @Query("file_name") String fileName);

    //
    @GET("api/v1/docs/{uuid}/comment/")
    Single<DocsCommentsWrapperModel> getComments(@Path("uuid") String uuid);

    @GET("api/v1/docs/{uuid}/")
    Single<SDocOutlineWrapperModel> getElements(@Path("uuid") String uuid);

    @Multipart
    @POST("api/v2.1/seadoc/upload-image/{sdoc_uuid}/")
    Flowable<DocsUploadResultModel> upload(@Path("sdoc_uuid") String docUid, @Part() MultipartBody.Part file, @PartMap Map<String, RequestBody> map);

    @POST("api/v1/docs/{sdoc_uuid}/comment/")
    Single<DocsCommentWrapperModel> postComment(@Path("sdoc_uuid") String uuid, @Body Map<String, Object> map);

    @PUT("api/v1/docs/{sdoc_uuid}/comment/{comment_id}/")
    Single<ResultModel> markResolved(@Path("sdoc_uuid") String docUid, @Path("comment_id") long commentId, @Body Map<String, Object> map);

    @DELETE("api/v1/docs/{sdoc_uuid}/comment/{comment_id}/")
    Single<ResultModel> delete(@Path("sdoc_uuid") String docUid, @Path("comment_id") long commentId);

}
