package com.seafile.seadroid2.ui.media;

import android.text.TextUtils;

import androidx.lifecycle.MutableLiveData;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.FileUtils;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.enums.TransferAction;
import com.seafile.seadroid2.enums.TransferResult;
import com.seafile.seadroid2.enums.TransferStatus;
import com.seafile.seadroid2.framework.data.db.AppDatabase;
import com.seafile.seadroid2.framework.data.db.entities.DirentModel;
import com.seafile.seadroid2.framework.data.db.entities.FileTransferEntity;
import com.seafile.seadroid2.framework.datastore.DataManager;
import com.seafile.seadroid2.framework.http.HttpIO;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.worker.ExistingFileStrategy;
import com.seafile.seadroid2.listener.FileTransferProgressListener;
import com.seafile.seadroid2.ui.base.viewmodel.BaseViewModel;
import com.seafile.seadroid2.ui.file.FileService;

import org.apache.commons.lang3.StringUtils;

import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.net.URLEncoder;
import java.nio.file.Path;
import java.util.List;

import io.reactivex.Single;
import io.reactivex.SingleEmitter;
import io.reactivex.SingleOnSubscribe;
import io.reactivex.SingleSource;
import io.reactivex.functions.BiFunction;
import io.reactivex.functions.Consumer;
import io.reactivex.functions.Function;
import kotlin.Pair;
import okhttp3.Call;
import okhttp3.Request;
import okhttp3.Response;
import okhttp3.ResponseBody;

public class PhotoViewModel extends BaseViewModel {
    private final MutableLiveData<String> _downloadedUrlLiveData = new MutableLiveData<>();
    private final MutableLiveData<String> _originalUrlLiveData = new MutableLiveData<>();

    private final MutableLiveData<Pair<DirentModel, FileTransferEntity>> _checkLocalLiveData = new MutableLiveData<>();

    public MutableLiveData<String> getDownloadedPathLiveData() {
        return _downloadedUrlLiveData;
    }

    public MutableLiveData<String> getOriginalUrlLiveData() {
        return _originalUrlLiveData;
    }

    public MutableLiveData<Pair<DirentModel, FileTransferEntity>> getCheckLocalLiveData() {
        return _checkLocalLiveData;
    }

    public void checkLocal(String repoId, String fullPath) {
        Single<List<DirentModel>> direntSingle = AppDatabase.getInstance().direntDao().getListByFullPathAsync(repoId, fullPath);
        Single<List<FileTransferEntity>> transferSingle = AppDatabase.getInstance().fileTransferDAO().getListByFullPathAsync(repoId, TransferAction.DOWNLOAD, fullPath);
        Single<Pair<DirentModel, FileTransferEntity>> rSingle = Single.zip(direntSingle, transferSingle, new BiFunction<List<DirentModel>, List<FileTransferEntity>, Pair<DirentModel, FileTransferEntity>>() {
            @Override
            public Pair<DirentModel, FileTransferEntity> apply(List<DirentModel> direntModels, List<FileTransferEntity> fileTransferEntities) throws Exception {
                if (CollectionUtils.isEmpty(direntModels)) {
                    return null;
                }

                if (CollectionUtils.isEmpty(fileTransferEntities)) {
                    return new Pair<>(direntModels.get(0), null);
                }

                return new Pair<>(direntModels.get(0), fileTransferEntities.get(0));
            }
        });

        addSingleDisposable(rSingle, new Consumer<Pair<DirentModel, FileTransferEntity>>() {
            @Override
            public void accept(Pair<DirentModel, FileTransferEntity> pair) throws Exception {
                getCheckLocalLiveData().setValue(pair);
            }
        });

    }

    private final int SEGMENT_SIZE = 8192;
    private final FileTransferProgressListener fileTransferProgressListener = new FileTransferProgressListener();

    public void requestOriginalUrl(DirentModel direntModel) {
        Single<String> downloadUrlSingle = HttpIO.getCurrentInstance()
                .execute(FileService.class)
                .getFileDownloadLinkAsync(direntModel.repo_id, direntModel.full_path);

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
                .getFileDownloadLinkAsync(direntModel.repo_id, direntModel.full_path);

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
        }).flatMap(new Function<String, SingleSource<FileTransferEntity>>() {
            @Override
            public SingleSource<FileTransferEntity> apply(String s) throws Exception {
                if (TextUtils.isEmpty(s)) {
                    //download url is null
                    throw SeafException.networkException;
                }

                FileTransferEntity transferEntity = FileTransferEntity.convertDirentModel2This(false, direntModel);

                return getDownloadSingle(transferEntity, s);
            }
        }).flatMap(new Function<FileTransferEntity, SingleSource<String>>() {
            @Override
            public SingleSource<String> apply(FileTransferEntity transferEntity) throws Exception {
                return Single.just(transferEntity.target_path);
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
                SeafException seafException = getExceptionByThrowable(throwable);
                getSeafExceptionLiveData().setValue(seafException);
            }
        });
    }

    private Single<FileTransferEntity> getDownloadSingle(FileTransferEntity transferEntity, String dlink) {
        return Single.create(new SingleOnSubscribe<FileTransferEntity>() {
            @Override
            public void subscribe(SingleEmitter<FileTransferEntity> emitter) throws Exception {

                Account currentAccount = SupportAccountManager.getInstance().getCurrentAccount();
                File localFile = DataManager.getLocalRepoFile(currentAccount, transferEntity);

                transferEntity.target_path = localFile.getAbsolutePath();
                AppDatabase.getInstance().fileTransferDAO().insert(transferEntity);

                Request request = new Request.Builder()
                        .url(dlink)
                        .get()
                        .build();

                Call newCall = HttpIO.getCurrentInstance().getOkHttpClient().getOkClient().newCall(request);

                try (Response response = newCall.execute()) {
                    if (!response.isSuccessful()) {
                        emitter.onError(SeafException.networkException);
                        return;
                    }

                    ResponseBody responseBody = response.body();
                    if (responseBody == null) {
                        emitter.onError(SeafException.networkException);
                        return;
                    }

                    long fileSize = responseBody.contentLength();
                    if (fileSize == -1) {
                        SLogs.d("download file error -> contentLength is -1");
                        SLogs.d(localFile.getAbsolutePath());

                        fileSize = transferEntity.file_size;
                    }

                    File tempFile = DataManager.createTempFile();
                    try (InputStream inputStream = responseBody.byteStream();
                         FileOutputStream fileOutputStream = new FileOutputStream(tempFile)) {

                        long totalBytesRead = 0;

                        int bytesRead;
                        byte[] buffer = new byte[SEGMENT_SIZE];
                        while ((bytesRead = inputStream.read(buffer, 0, buffer.length)) != -1) {
                            fileOutputStream.write(buffer, 0, bytesRead);
                            totalBytesRead += bytesRead;

                            int p = fileTransferProgressListener.onProgress(totalBytesRead, fileSize);
                            SLogs.e(transferEntity.file_name + ", progress: " + p);
                        }
                        int p = fileTransferProgressListener.onProgress(fileSize, fileSize);
                        SLogs.e(transferEntity.file_name + ", progress: " + p);
                    }

                    //important
                    Path path = java.nio.file.Files.move(tempFile.toPath(), localFile.toPath(), java.nio.file.StandardCopyOption.REPLACE_EXISTING);
                    boolean isSuccess = path.toFile().exists();
                    if (isSuccess) {
                        SLogs.e("move file success: " + path);
                        transferEntity.transferred_size = fileSize;
                        transferEntity.action_end_at = System.currentTimeMillis();
                        transferEntity.file_original_modified_at = transferEntity.action_end_at;
                        transferEntity.transfer_result = TransferResult.TRANSMITTED;
                        transferEntity.transfer_status = TransferStatus.SUCCEEDED;

                        transferEntity.file_md5 = FileUtils.getFileMD5ToString(transferEntity.target_path).toLowerCase();

                        AppDatabase.getInstance().fileTransferDAO().update(transferEntity);

                        emitter.onSuccess(transferEntity);
                    } else {
                        SLogs.e("move file failed: " + path);

                        transferEntity.transfer_result = TransferResult.FILE_ERROR;
                        transferEntity.transfer_status = TransferStatus.FAILED;
                        AppDatabase.getInstance().fileTransferDAO().update(transferEntity);

                        emitter.onError(SeafException.transferFileException);
                    }
                } catch (Exception e) {
                    emitter.onError(e);
                }
            }
        });
    }

}
