package com.seafile.seadroid2.ui.file;

import com.seafile.seadroid2.framework.data.BlockInfoBean;
import com.seafile.seadroid2.framework.data.FileBlocks;
import com.seafile.seadroid2.framework.data.model.dirents.DirentDirModel;
import com.seafile.seadroid2.framework.data.model.dirents.DirentFileModel;
import com.seafile.seadroid2.framework.data.model.dirents.DirentRecursiveFileModel;

import java.util.List;
import java.util.Map;

import io.reactivex.Single;
import okhttp3.RequestBody;
import retrofit2.Call;
import retrofit2.http.GET;
import retrofit2.http.Multipart;
import retrofit2.http.POST;
import retrofit2.http.PartMap;
import retrofit2.http.Path;
import retrofit2.http.Query;

public interface FileService {

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

    @GET("api2/repos/{repo_id}/file/detail/")
    Single<DirentFileModel> getFileDetail(@Path("repo_id") String repoId, @Query("p") String path);

    @GET("api2/repos/{repo_id}/dir/?t=f&recursive=1")
    Call<List<DirentRecursiveFileModel>> getDirRecursiveFileCall(@Path("repo_id") String repoId, @Query("p") String path);

    @GET("api2/repos/{repo_id}/file/?op=download")
    Call<String> getFileDownloadLink(@Path("repo_id") String repoId, @Query("p") String path);

    @GET("api2/repos/{repo_id}/file/?op=download")
    Single<String> getFileDownloadLinkAsync(@Path("repo_id") String repoId, @Query("p") String path);

    @GET("api2/repos/{repo_id}/file/?op=download")
    Call<String> getFileDownloadLinkSync(@Path("repo_id") String repoId, @Query("p") String path);

    @GET("api2/repos/{repo_id}/file/?op=downloadblks")
    Call<FileBlocks> getFileBlockDownloadLink(@Path("repo_id") String repoId, @Query("p") String path);

    @GET("api2/repos/{repo_id}/files/{file_id}/blks/{block_id}/download-link/")
    Call<String> getBlockDownloadLink(@Path("repo_id") String repoId, @Path("file_id") String fileId, @Path("block_id") String blockId);

    @GET("api2/repos/{repo_id}/upload-link/")
    Call<String> getFileUploadLink(@Path("repo_id") String repoId, @Query("p") String path);

    @GET("api2/repos/{repo_id}/update-link/")
    Call<String> getFileUpdateLink(@Path("repo_id") String repoId);

    @GET("api2/repos/{repo_id}/update-link/")
    Single<String> getFileUpdateLinkSync(@Path("repo_id") String repoId);


    @Multipart
    @POST("api2/repos/{repo_id}/upload-blks-link/")
    Call<BlockInfoBean> getFileBlockUploadLink(@Path("repo_id") String repoId, @PartMap Map<String, RequestBody> map);

}
