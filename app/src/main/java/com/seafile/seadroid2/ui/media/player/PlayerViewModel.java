package com.seafile.seadroid2.ui.media.player;

import android.text.TextUtils;

import androidx.lifecycle.MutableLiveData;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.NetworkUtils;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.framework.db.AppDatabase;
import com.seafile.seadroid2.framework.db.entities.FileCacheStatusEntity;
import com.seafile.seadroid2.framework.http.HttpIO;
import com.seafile.seadroid2.ui.base.viewmodel.BaseViewModel;
import com.seafile.seadroid2.ui.file.FileService;

import java.io.File;
import java.util.List;

import io.reactivex.Single;
import io.reactivex.functions.Consumer;

public class PlayerViewModel extends BaseViewModel {

    private final MutableLiveData<String> _urlLiveData = new MutableLiveData<>();

    public MutableLiveData<String> getUrlLiveData() {
        return _urlLiveData;
    }

    public void checkLocalAndOpen(String repo_id, String path, String newFileId, boolean isReused) {
        getSecondRefreshLiveData().setValue(true);

        Single<List<FileCacheStatusEntity>> dbSingle = AppDatabase.getInstance().fileCacheStatusDAO().getByFullPath(repo_id, path);
        addSingleDisposable(dbSingle, new Consumer<List<FileCacheStatusEntity>>() {
            @Override
            public void accept(List<FileCacheStatusEntity> fileCacheStatusEntities) throws Exception {
                if (CollectionUtils.isEmpty(fileCacheStatusEntities)) {
                    getFileLink(repo_id, path, isReused);
                } else {
                    FileCacheStatusEntity fileCacheStatusEntity = fileCacheStatusEntities.get(0);
                    if (TextUtils.equals(newFileId, fileCacheStatusEntity.file_id)) {
                        File file = new File(fileCacheStatusEntity.target_path);
                        if (file.exists()) {
                            getUrlLiveData().setValue(fileCacheStatusEntity.target_path);
                        } else {
                            getFileLink(repo_id, path, isReused);
                        }
                    } else {
                        getFileLink(repo_id, path, isReused);
                    }
                }
            }
        }, new Consumer<Throwable>() {
            @Override
            public void accept(Throwable throwable) throws Exception {
                SeafException seafException = getSeafExceptionByThrowable(throwable);
                getSeafExceptionLiveData().setValue(seafException);
            }
        });
    }

    private void getFileLink(String repoId, String p, boolean isReUsed) {
        if (!NetworkUtils.isConnected()) {
            getSeafExceptionLiveData().setValue(SeafException.NETWORK_UNAVAILABLE);
            return;
        }

        Single<String> urlSingle = HttpIO.getCurrentInstance().execute(FileService.class).getFileDownloadLinkAsync(repoId, p, isReUsed ? 1 : 0);
        addSingleDisposable(urlSingle, new Consumer<String>() {
            @Override
            public void accept(String dlink) {
                if (TextUtils.isEmpty(dlink) || "\"\"".equals(dlink)) {
                    getSeafExceptionLiveData().setValue(SeafException.REQUEST_URL_EXCEPTION);
                    return;
                }

                getUrlLiveData().setValue(dlink);
            }
        }, new Consumer<Throwable>() {
            @Override
            public void accept(Throwable throwable) throws Exception {
                SeafException seafException = getSeafExceptionByThrowable(throwable);
                getSeafExceptionLiveData().setValue(seafException);
            }
        });
    }
}
