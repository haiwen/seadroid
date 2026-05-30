package com.seafile.seadroid2.framework.util;

import android.text.TextUtils;

import androidx.annotation.VisibleForTesting;

import com.blankj.utilcode.util.CollectionUtils;
import com.seafile.seadroid2.framework.db.AppDatabase;
import com.seafile.seadroid2.framework.db.dao.FileCacheStatusDAO;
import com.seafile.seadroid2.framework.db.entities.FileCacheStatusEntity;
import com.seafile.seadroid2.framework.http.HttpManager;
import com.seafile.seadroid2.framework.model.dirents.DirentFileModel;
import com.seafile.seadroid2.ui.file.FileService;

import java.util.List;

import io.reactivex.Single;
import io.reactivex.functions.Function;

public class FileCacheHelper {

    /**
     * Fetch the server's current file detail and compare its id against the
     * locally cached file id. Returns the cached file id if they match (cache is
     * fresh), or empty string if the cache is stale or absent.
     */
    public static Single<String> checkRemoteAndOpen(String repoId, String path) {
        FileService fileService = HttpManager
                .getCurrentHttp()
                .execute(FileService.class);
        FileCacheStatusDAO dao = AppDatabase
                .getInstance()
                .fileCacheStatusDAO();
        return checkRemoteAndOpen(fileService, dao, repoId, path);
    }

    @VisibleForTesting
    public static Single<String> checkRemoteAndOpen(FileService fileService, FileCacheStatusDAO dao, String repoId, String path) {
        Single<DirentFileModel> detailSingle = fileService.getFileDetail(repoId, path);

        Single<List<FileCacheStatusEntity>> cacheDbSingle = dao.getByFullPath(repoId, path);

        return cacheDbSingle.flatMap(new Function<List<FileCacheStatusEntity>, Single<String>>() {
            @Override
            public Single<String> apply(List<FileCacheStatusEntity> cacheStatusEntities) {
                if (CollectionUtils.isEmpty(cacheStatusEntities)) {
                    return Single.just("");
                }

                String localFileId = cacheStatusEntities.get(0).file_id;
                if (TextUtils.isEmpty(localFileId)) {
                    return Single.just("");
                }

                return detailSingle.flatMap(new Function<DirentFileModel, Single<String>>() {
                    @Override
                    public Single<String> apply(DirentFileModel direntFileModel) {
                        if (direntFileModel == null) {
                            return Single.just("");
                        }
                        if (!TextUtils.equals(direntFileModel.id, localFileId)) {
                            return Single.just("");
                        }
                        return Single.just(localFileId);
                    }
                });
            }
        });
    }
}
