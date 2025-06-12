package com.seafile.seadroid2.ui.media.image;

import android.text.TextUtils;
import android.webkit.MimeTypeMap;

import androidx.lifecycle.MutableLiveData;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.FileUtils;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.framework.datastore.DataManager;
import com.seafile.seadroid2.framework.db.AppDatabase;
import com.seafile.seadroid2.framework.db.entities.DirentModel;
import com.seafile.seadroid2.framework.db.entities.FileCacheStatusEntity;
import com.seafile.seadroid2.framework.http.HttpIO;
import com.seafile.seadroid2.framework.model.sdoc.FileDetailModel;
import com.seafile.seadroid2.framework.model.sdoc.FileProfileConfigModel;
import com.seafile.seadroid2.framework.model.sdoc.FileRecordWrapperModel;
import com.seafile.seadroid2.framework.model.sdoc.FileTagWrapperModel;
import com.seafile.seadroid2.framework.model.sdoc.MetadataConfigModel;
import com.seafile.seadroid2.framework.model.user.UserWrapperModel;
import com.seafile.seadroid2.framework.util.ExceptionUtils;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.listener.FileTransferProgressListener;
import com.seafile.seadroid2.ui.base.viewmodel.BaseViewModel;
import com.seafile.seadroid2.ui.file.FileService;
import com.seafile.seadroid2.ui.sdoc.SDocService;

import org.apache.commons.lang3.StringUtils;

import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.net.URLEncoder;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

import io.reactivex.Single;
import io.reactivex.SingleEmitter;
import io.reactivex.SingleOnSubscribe;
import io.reactivex.SingleSource;
import io.reactivex.functions.BiFunction;
import io.reactivex.functions.Consumer;
import io.reactivex.functions.Function;
import okhttp3.Call;
import okhttp3.Request;
import okhttp3.Response;
import okhttp3.ResponseBody;

public class PhotoViewModel extends BaseViewModel {

    private final MutableLiveData<String> _downloadedUrlLiveData = new MutableLiveData<>();
    private final MutableLiveData<String> _originalUrlLiveData = new MutableLiveData<>();
    private final MutableLiveData<SeafException> _fileDetailExceptionLiveData = new MutableLiveData<>();
    private final MutableLiveData<DirentModel> _checkLocalLiveData = new MutableLiveData<>();

    public MutableLiveData<String> getDownloadedPathLiveData() {
        return _downloadedUrlLiveData;
    }

    public MutableLiveData<SeafException> getFileDetailExceptionLiveData() {
        return _fileDetailExceptionLiveData;
    }

    public MutableLiveData<String> getOriginalUrlLiveData() {
        return _originalUrlLiveData;
    }

    public MutableLiveData<DirentModel> getCheckLocalLiveData() {
        return _checkLocalLiveData;
    }

    private final MutableLiveData<FileProfileConfigModel> _fileProfileConfigLiveData = new MutableLiveData<>();

    public MutableLiveData<FileProfileConfigModel> getFileDetailLiveData() {
        return _fileProfileConfigLiveData;
    }

    public void getFileDetail(String repoId, String path) {
        SLogs.d(PhotoFragment.TAG, "getFileDetail()", path);

        Single<FileDetailModel> detailSingle = HttpIO.getCurrentInstance()
                .execute(SDocService.class)
                .getFileDetail(repoId, path);

        Single<MetadataConfigModel> metadataSingle = HttpIO
                .getCurrentInstance()
                .execute(SDocService.class)
                .getMetadata(repoId)
                .onErrorReturnItem(new MetadataConfigModel());

        Single<FileProfileConfigModel> s = Single.zip(metadataSingle, detailSingle, new BiFunction<MetadataConfigModel, FileDetailModel, FileProfileConfigModel>() {
            @Override
            public FileProfileConfigModel apply(MetadataConfigModel metadataConfigModel, FileDetailModel fileDetailModel) {
                FileProfileConfigModel configModel = new FileProfileConfigModel();
                configModel.setMetadataConfigModel(metadataConfigModel);
                configModel.setDetail(fileDetailModel);
                return configModel;
            }
        }).flatMap(new Function<FileProfileConfigModel, SingleSource<FileProfileConfigModel>>() {
            @Override
            public SingleSource<FileProfileConfigModel> apply(FileProfileConfigModel configModel) throws Exception {
                List<Single<?>> singles = new ArrayList<>();

                if (configModel.getMetaEnabled()) {

                    String parent_dir;
                    String name;

                    // 1. /a/b/c/t.txt
                    // 2. /a/t.txt
                    // 3. /t.txt
                    // 4. t.txt
                    // 5. /
                    if (path.contains("/")) {
                        parent_dir = path.substring(0, path.lastIndexOf("/"));
                        name = path.substring(path.lastIndexOf("/") + 1);
                    } else {
                        parent_dir = null;
                        name = path;
                    }

                    if (TextUtils.isEmpty(parent_dir)) {
                        parent_dir = "/";
                    }

                    SLogs.d(PhotoFragment.TAG, "metadata enabled", "path = " + path);
                    //user
                    Single<UserWrapperModel> userSingle = HttpIO.getCurrentInstance()
                            .execute(SDocService.class)
                            .getRelatedUsers(repoId);
                    singles.add(userSingle);

                    //record
                    SLogs.d(PhotoFragment.TAG, "parent_dir: " + parent_dir + ", name: " + name);
                    Single<FileRecordWrapperModel> recordSingle = HttpIO.getCurrentInstance()
                            .execute(SDocService.class)
                            .getRecords(repoId, parent_dir, name, name)
                            .onErrorReturnItem(new FileRecordWrapperModel());
                    singles.add(recordSingle);
                }

                if (configModel.getTagsEnabled()) {
                    SLogs.d(PhotoFragment.TAG, "tag enabled", "path = " + path);
                    //tag
                    Single<FileTagWrapperModel> tagSingle = HttpIO.getCurrentInstance()
                            .execute(SDocService.class)
                            .getTags(repoId)
                            .onErrorReturnItem(new FileTagWrapperModel());
                    singles.add(tagSingle);
                }

                if (singles.isEmpty()) {
                    configModel.initDefaultIfMetaNotEnable();
                    return Single.just(configModel);
                }

                return Single.zip(singles, new Function<Object[], FileProfileConfigModel>() {
                    @Override
                    public FileProfileConfigModel apply(Object[] results) throws Exception {

                        if (configModel.getMetaEnabled()) {
                            UserWrapperModel u = (UserWrapperModel) results[0];
                            configModel.setRelatedUserWrapperModel(u);

                            FileRecordWrapperModel r = (FileRecordWrapperModel) results[1];
                            if (r.results.isEmpty()) {
                                configModel.initDefaultIfMetaNotEnable();
                            } else {
                                configModel.addRecordWrapperModel(r);
                            }
                        } else {
                            configModel.initDefaultIfMetaNotEnable();
                        }

                        if (configModel.getMetaEnabled() && configModel.getTagsEnabled()) {
                            FileTagWrapperModel t = (FileTagWrapperModel) results[2];
                            configModel.setTagWrapperModel(t);
                        } else if (configModel.getTagsEnabled()) {
                            FileTagWrapperModel t = (FileTagWrapperModel) results[0];
                            configModel.setTagWrapperModel(t);
                        }

                        return configModel;
                    }
                });
            }
        });


        addSingleDisposable(s, new Consumer<FileProfileConfigModel>() {
            @Override
            public void accept(FileProfileConfigModel fileProfileConfigModel) throws Exception {
                getFileDetailLiveData().setValue(fileProfileConfigModel);
            }
        }, new Consumer<Throwable>() {
            @Override
            public void accept(Throwable throwable) {
                SeafException seafException = ExceptionUtils.parseByThrowable(throwable);
                getFileDetailExceptionLiveData().setValue(seafException);
            }
        });
    }

    public void checkLocal(String repoId, String fullPath) {
        Single<List<DirentModel>> direntSingle = AppDatabase.getInstance().direntDao().getListByFullPathAsync(repoId, fullPath);
        Single<List<FileCacheStatusEntity>> cacheSingle = AppDatabase.getInstance().fileCacheStatusDAO().getByFullPath(repoId, fullPath);
        Single<DirentModel> d = Single.zip(direntSingle, cacheSingle, new BiFunction<List<DirentModel>, List<FileCacheStatusEntity>, DirentModel>() {
            @Override
            public DirentModel apply(List<DirentModel> direntModels, List<FileCacheStatusEntity> fileCacheStatusEntities) throws Exception {
                if (CollectionUtils.isEmpty(direntModels)) {
                    return null;
                }

                DirentModel direntModel = direntModels.get(0);
                if (CollectionUtils.isEmpty(fileCacheStatusEntities)) {
                    //set null
                    direntModel.local_file_id = null;
                    return direntModel;
                }

                FileCacheStatusEntity entity = fileCacheStatusEntities.get(0);
                if (TextUtils.equals(entity.file_id, direntModel.id)) {
                    //set null
                    direntModel.local_file_id = entity.file_id;
                }
                return direntModel;
            }
        });
        addSingleDisposable(d, new Consumer<DirentModel>() {
            @Override
            public void accept(DirentModel direntModel) throws Exception {
                getCheckLocalLiveData().setValue(direntModel == null ? new DirentModel() : direntModel);
            }
        });
    }

    private final int SEGMENT_SIZE = 8192;
    private final FileTransferProgressListener fileTransferProgressListener = new FileTransferProgressListener();

    public void requestOriginalUrl(DirentModel direntModel) {
        Single<String> downloadUrlSingle = HttpIO.getCurrentInstance()
                .execute(FileService.class)
                .getFileDownloadLinkAsync(direntModel.repo_id, direntModel.full_path, 1);

        addSingleDisposable(downloadUrlSingle, new Consumer<String>() {
            @Override
            public void accept(String dlink) throws Exception {
                //
                dlink = StringUtils.replace(dlink, "\"", "");
                int i = dlink.lastIndexOf('/');
                if (i == -1) {
                    return;
                }

                dlink = dlink.substring(0, i) + "/" + URLEncoder.encode(dlink.substring(i + 1), "UTF-8");

                getOriginalUrlLiveData().setValue(dlink);
            }
        });
    }

    public void download(DirentModel direntModel) {
        Single<String> downloadUrlSingle = HttpIO.getCurrentInstance()
                .execute(FileService.class)
                .getFileDownloadLinkAsync(direntModel.repo_id, direntModel.full_path, 1);

        Single<String> downloadedFilePathSingle = downloadUrlSingle.flatMap(new Function<String, SingleSource<String>>() {
            @Override
            public SingleSource<String> apply(String dlink) throws Exception {
                //
                dlink = StringUtils.replace(dlink, "\"", "");
                int i = dlink.lastIndexOf('/');
                if (i == -1) {
                    return null;
                }

                dlink = dlink.substring(0, i) + "/" + URLEncoder.encode(dlink.substring(i + 1), "UTF-8");
                return Single.just(dlink);
            }
        }).flatMap(new Function<String, SingleSource<File>>() {
            @Override
            public SingleSource<File> apply(String s) throws Exception {
                if (TextUtils.isEmpty(s)) {
                    //download url is null
                    throw SeafException.NETWORK_EXCEPTION;
                }

                return getDownloadSingle(direntModel, s);
            }
        }).flatMap(new Function<File, SingleSource<String>>() {
            @Override
            public SingleSource<String> apply(File file) throws Exception {
                return Single.just(file.getAbsolutePath());
            }
        });

        addSingleDisposable(downloadedFilePathSingle, new Consumer<String>() {
            @Override
            public void accept(String s) {
                getDownloadedPathLiveData().setValue(s);
            }
        }, new Consumer<Throwable>() {
            @Override
            public void accept(Throwable throwable) throws Exception {
                SeafException seafException = getSeafExceptionByThrowable(throwable);
                getSeafExceptionLiveData().setValue(seafException);
            }
        });
    }

    private Single<File> getDownloadSingle(DirentModel direntModel, String dlink) {
        return Single.create(new SingleOnSubscribe<File>() {
            @Override
            public void subscribe(SingleEmitter<File> emitter) throws Exception {
                if (emitter.isDisposed()) {
                    return;
                }

                Account currentAccount = SupportAccountManager.getInstance().getCurrentAccount();
                File destinationFile = DataManager.getLocalFileCachePath(currentAccount, direntModel.repo_id, direntModel.repo_name, direntModel.full_path);

                Request request = new Request.Builder()
                        .url(dlink)
                        .get()
                        .build();

                Call newCall = HttpIO.getCurrentInstance().getOkHttpClient().getOkClient().newCall(request);

                try (Response response = newCall.execute()) {
                    if (!response.isSuccessful()) {
                        if (!emitter.isDisposed()) {
                            emitter.onError(SeafException.NETWORK_EXCEPTION);
                        }
                        return;
                    }

                    ResponseBody responseBody = response.body();
                    if (responseBody == null) {
                        if (!emitter.isDisposed()) {
                            emitter.onError(SeafException.NETWORK_EXCEPTION);
                        }
                        return;
                    }

                    long fileSize = responseBody.contentLength();
                    if (fileSize == -1) {
                        SLogs.d(PhotoFragment.TAG, "getDownloadSingle()", "download file error -> contentLength is -1");
                        SLogs.d(PhotoFragment.TAG, destinationFile.getAbsolutePath());

                        fileSize = direntModel.size;
                    }

                    File tempFile = DataManager.createTempFile();
                    try (InputStream inputStream = responseBody.byteStream();
                         FileOutputStream fileOutputStream = new FileOutputStream(tempFile)) {

                        int bytesRead;
                        byte[] buffer = new byte[SEGMENT_SIZE];
                        while ((bytesRead = inputStream.read(buffer, 0, buffer.length)) != -1) {
                            fileOutputStream.write(buffer, 0, bytesRead);
                        }
                    }

                    if (!java.nio.file.Files.exists(tempFile.toPath())) {
                        if (!emitter.isDisposed()) {
                            emitter.onError(SeafException.TRANSFER_FILE_EXCEPTION);
                        }
                        return;
                    }

                    Path path = java.nio.file.Files.move(tempFile.toPath(), destinationFile.toPath(), java.nio.file.StandardCopyOption.REPLACE_EXISTING);
                    boolean isSuccess = path.toFile().exists();

                    java.nio.file.Files.deleteIfExists(tempFile.toPath());

                    if (isSuccess) {
                        FileCacheStatusEntity entity = getSaveEntity(direntModel, destinationFile);
                        AppDatabase.getInstance().fileCacheStatusDAO().insert(entity);

                        SLogs.d(PhotoFragment.TAG, "getDownloadSingle()", "move file success: " + path);
                        if (!emitter.isDisposed()) {
                            emitter.onSuccess(destinationFile);
                        }
                    } else {
                        SLogs.d(PhotoFragment.TAG, "getDownloadSingle()", "move file failed: " + path);
                        if (!emitter.isDisposed()) {
                            emitter.onError(SeafException.TRANSFER_FILE_EXCEPTION);
                        }
                    }
                } catch (Exception e) {
                    if (!emitter.isDisposed()) {
                        emitter.onError(e);
                    }
                }
            }
        });
    }


    public FileCacheStatusEntity getSaveEntity(DirentModel direntModel, File destinationFile) {
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
        return entity;
    }

}
