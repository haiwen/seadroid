package com.seafile.seadroid2.ui.file;

import android.webkit.MimeTypeMap;

import androidx.lifecycle.MutableLiveData;

import com.blankj.utilcode.util.FileUtils;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.framework.db.AppDatabase;
import com.seafile.seadroid2.framework.db.entities.DirentModel;
import com.seafile.seadroid2.framework.db.entities.FileCacheStatusEntity;
import com.seafile.seadroid2.framework.model.dirents.DirentFileModel;
import com.seafile.seadroid2.framework.datastore.DataManager;
import com.seafile.seadroid2.framework.http.HttpIO;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.ui.base.viewmodel.BaseViewModel;

import org.reactivestreams.Publisher;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;

import io.reactivex.Flowable;
import io.reactivex.Single;
import io.reactivex.SingleEmitter;
import io.reactivex.SingleOnSubscribe;
import io.reactivex.functions.Action;
import io.reactivex.functions.Consumer;
import io.reactivex.functions.Function;

public class FileViewModel extends BaseViewModel {
    private final MutableLiveData<Long[]> progressLiveData = new MutableLiveData<>();
    private final MutableLiveData<File> outFileLiveData = new MutableLiveData<>();

    public MutableLiveData<Long[]> getProgressLiveData() {
        return progressLiveData;
    }

    public MutableLiveData<File> getOutFileLiveData() {
        return outFileLiveData;
    }

    public void loadFileDetail(String repoId, String path, Consumer<DirentFileModel> consumer) {

        // get file detail
        Single<DirentFileModel> detailSingle = HttpIO.getCurrentInstance().execute(FileService.class).getFileDetail(repoId, path);
        addSingleDisposable(detailSingle, consumer, new Consumer<Throwable>() {
            @Override
            public void accept(Throwable throwable) throws Exception {
                SeafException seafException = getExceptionByThrowable(throwable);
                getSeafExceptionLiveData().setValue(seafException);
            }
        });
    }

    /**
     * Overlay downloads
     */
    public void download(Account account, DirentModel direntModel, File destinationFile) {
        try {
            File tempFile = DataManager.createTempFile();
            Single<String> urlSingle = HttpIO.getCurrentInstance().execute(FileService.class).getFileDownloadLinkAsync(direntModel.repo_id, direntModel.full_path, 1);
            Flowable<String> urlFlowable = urlSingle.toFlowable();
            Flowable<Long[]> flowable = urlFlowable.flatMap(new Function<String, Publisher<Long[]>>() {
                @Override
                public Publisher<Long[]> apply(String url) throws Exception {
                    return HttpIO.getCurrentInstance().downloadBinary(url, tempFile);
                }
            });

            addFlowableDisposable(flowable, new Consumer<Long[]>() {
                @Override
                public void accept(Long[] longs) throws Exception {
                    getProgressLiveData().setValue(longs);
                }
            }, new Consumer<Throwable>() {
                @Override
                public void accept(Throwable throwable) throws Exception {

                    java.nio.file.Files.deleteIfExists(tempFile.toPath());

                    SeafException seafException = getExceptionByThrowable(throwable);
                    getSeafExceptionLiveData().setValue(seafException);
                }
            }, new Action() {
                @Override
                public void run() throws Exception {
                    //important
                    if (java.nio.file.Files.exists(tempFile.toPath())) {
                        Path path = java.nio.file.Files.move(tempFile.toPath(), destinationFile.toPath(), java.nio.file.StandardCopyOption.REPLACE_EXISTING);
                        boolean isSuccess = path.toFile().exists();
                        SLogs.d("download file: " + isSuccess);
                    }

                    getOutFileLiveData().setValue(destinationFile);
                }
            });
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public void saveToDb(Account account, DirentModel direntModel, File destinationFile, Consumer<Boolean> consumer) {
        Single<Boolean> single = Single.create(new SingleOnSubscribe<Boolean>() {
            @Override
            public void subscribe(SingleEmitter<Boolean> emitter) throws Exception {
                FileCacheStatusEntity entity = new FileCacheStatusEntity();
                entity.v = 2;//new version
                entity.repo_id = direntModel.repo_id;
                entity.repo_name = direntModel.repo_name;
                entity.related_account = direntModel.related_account;
                entity.file_id = direntModel.id;
                entity.full_path = direntModel.full_path;
                entity.target_path = destinationFile.getAbsolutePath();
                entity.setParent_path(Utils.getParentPath(entity.full_path));

                entity.file_name = direntModel.name;
                entity.file_size = destinationFile.length();
                entity.file_format = FileUtils.getFileExtension(entity.full_path);
                entity.file_md5 = FileUtils.getFileMD5ToString(destinationFile).toLowerCase();
                entity.mime_type = MimeTypeMap.getSingleton().getMimeTypeFromExtension(entity.file_format);
                entity.created_at = System.currentTimeMillis();
                entity.modified_at = entity.created_at;

                entity.uid = entity.genUID();
                AppDatabase.getInstance().fileCacheStatusDAO().insert(entity);

                emitter.onSuccess(true);
            }
        });

        addSingleDisposable(single, consumer);
    }
}
