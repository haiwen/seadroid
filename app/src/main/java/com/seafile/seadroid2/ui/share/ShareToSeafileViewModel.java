package com.seafile.seadroid2.ui.share;

import android.content.Context;
import android.net.Uri;
import android.text.TextUtils;
import android.util.Pair;

import androidx.lifecycle.MutableLiveData;

import com.blankj.utilcode.util.CollectionUtils;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.framework.data.db.AppDatabase;
import com.seafile.seadroid2.framework.data.db.entities.DirentModel;
import com.seafile.seadroid2.framework.data.db.entities.FileTransferEntity;
import com.seafile.seadroid2.framework.data.model.repo.DirentWrapperModel;
import com.seafile.seadroid2.framework.datastore.DataManager;
import com.seafile.seadroid2.framework.http.HttpIO;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.framework.worker.BackgroundJobManagerImpl;
import com.seafile.seadroid2.ui.base.viewmodel.BaseViewModel;
import com.seafile.seadroid2.ui.repo.RepoService;

import org.apache.commons.io.IOUtils;

import java.io.File;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.List;

import io.reactivex.Single;
import io.reactivex.SingleEmitter;
import io.reactivex.SingleOnSubscribe;
import io.reactivex.SingleSource;
import io.reactivex.functions.Consumer;
import io.reactivex.functions.Function;
import retrofit2.Call;
import retrofit2.Response;

public class ShareToSeafileViewModel extends BaseViewModel {

    private MutableLiveData<Boolean> mActionLiveData = new MutableLiveData<>();

    public MutableLiveData<Boolean> getActionLiveData() {
        return mActionLiveData;
    }

    public void checkRemoteExists(Context context, Account account, String repoId, String repoName, String targetDir, List<Uri> uris, Consumer<Pair<List<DirentModel>, List<DirentModel>>> consumer) {
        Single<Pair<List<DirentModel>, List<DirentModel>>> single = Single.create(new SingleOnSubscribe<Pair<List<DirentModel>, List<DirentModel>>>() {
            @Override
            public void subscribe(SingleEmitter<Pair<List<DirentModel>, List<DirentModel>>> emitter) throws Exception {
                Call<DirentWrapperModel> call = HttpIO.getInstanceByAccount(account).execute(RepoService.class).getDirentsSync(repoId, targetDir);
                Response<DirentWrapperModel> res = call.execute();
                if (!res.isSuccessful()) {
                    emitter.onError(SeafException.networkException);
                    return;
                }


                DirentWrapperModel wrapperModel = res.body();
                if (wrapperModel == null || CollectionUtils.isEmpty(wrapperModel.dirent_list)) {
                    emitter.onSuccess(new Pair<>(null, null));
                    return;
                }

                List<DirentModel> list = new ArrayList<>();
                for (Uri uri : uris) {
                    String fileName = Utils.getFilenameFromUri(context, uri);
                    for (DirentModel direntModel : wrapperModel.dirent_list) {
                        if (TextUtils.equals(direntModel.name, fileName)) {
                            list.add(direntModel);
                            break;
                        }
                    }
                }

                emitter.onSuccess(new Pair<>(wrapperModel.dirent_list, list));
            }
        });

        addSingleDisposable(single, new Consumer<Pair<List<DirentModel>, List<DirentModel>>>() {
            @Override
            public void accept(Pair<List<DirentModel>, List<DirentModel>> listListPair) throws Exception {
                consumer.accept(listListPair);
            }
        });
    }

    public void upload(Context context, Account account, String repoId, String
            repoName, String targetDir, List<Uri> uris, List<DirentModel> direntModels, boolean isUpdate) {
        Single<List<File>> localFileSingle = Single.create(new SingleOnSubscribe<List<File>>() {
            @Override
            public void subscribe(SingleEmitter<List<File>> emitter) throws Exception {
                //Copy to the app's internal cache directory

                List<File> fileList = new ArrayList<>();
                for (Uri uri : uris) {
                    File tempDir = DataManager.createTempDir();
                    File tempFile = new File(tempDir, Utils.getFilenameFromUri(context, uri));

                    if (!tempFile.createNewFile()) {
                        throw new RuntimeException("could not create temporary file");
                    }

                    try (
                            InputStream in = context.getContentResolver().openInputStream(uri);
                            OutputStream out = Files.newOutputStream(tempFile.toPath())
                    ) {
                        IOUtils.copy(in, out);
                    }

                    fileList.add(tempFile);
                }

                emitter.onSuccess(fileList);
            }
        });

        Single<Boolean> booleanSingle = localFileSingle.flatMap(new Function<List<File>, SingleSource<List<FileTransferEntity>>>() {
            @Override
            public SingleSource<List<FileTransferEntity>> apply(List<File> files) throws Exception {
                //Calculate the MD5 of the file
                return Single.create(new SingleOnSubscribe<List<FileTransferEntity>>() {
                    @Override
                    public void subscribe(SingleEmitter<List<FileTransferEntity>> emitter) throws Exception {
                        List<FileTransferEntity> sList = new ArrayList<>();
                        if (CollectionUtils.isEmpty(direntModels)) {
                            for (File file : files) {
                                FileTransferEntity fileTransferEntity = FileTransferEntity
                                        .convert2ThisForFileBackup(account, repoId, repoName, file, targetDir, file.lastModified(), isUpdate, false);

                                sList.add(fileTransferEntity);
                            }
                        } else {
                            for (File file : files) {
                                boolean isEx = direntModels.stream().anyMatch(a -> TextUtils.equals(a.name, file.getName()));

                                FileTransferEntity fileTransferEntity = FileTransferEntity
                                        .convert2ThisForFileBackup(account, repoId, repoName, file, targetDir, file.lastModified(), isUpdate, isEx);

                                sList.add(fileTransferEntity);
                            }
                        }

                        emitter.onSuccess(sList);
                    }
                });
            }
        }).flatMap(new Function<List<FileTransferEntity>, SingleSource<Boolean>>() {
            @Override
            public SingleSource<Boolean> apply(List<FileTransferEntity> fileTransferEntities) throws Exception {
                return Single.create(new SingleOnSubscribe<Boolean>() {
                    @Override
                    public void subscribe(SingleEmitter<Boolean> emitter) throws Exception {
                        AppDatabase.getInstance().fileTransferDAO().insertAll(fileTransferEntities);
                        emitter.onSuccess(true);
                    }
                });
            }
        });

        addSingleDisposable(booleanSingle, new Consumer<Boolean>() {
            @Override
            public void accept(Boolean aBoolean) throws Exception {
                BackgroundJobManagerImpl.getInstance().startFileUploadWorker();

                getActionLiveData().setValue(true);
            }
        });

    }
}
