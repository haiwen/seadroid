package com.seafile.seadroid2.ui.editor;

import androidx.lifecycle.MutableLiveData;

import com.blankj.utilcode.util.FileIOUtils;
import com.seafile.seadroid2.framework.worker.FileSyncService;
import com.seafile.seadroid2.ui.base.viewmodel.BaseViewModel;

import io.reactivex.Single;
import io.reactivex.SingleEmitter;
import io.reactivex.SingleOnSubscribe;
import io.reactivex.functions.Consumer;

public class EditorViewModel extends BaseViewModel {
    public MutableLiveData<String> fileIdLiveData = new MutableLiveData<>();

    public MutableLiveData<String> getFileIdLiveData() {
        return fileIdLiveData;
    }

    public void read(String path, Consumer<String> consumer) {
        Single<String> single = Single.create(new SingleOnSubscribe<String>() {
            @Override
            public void subscribe(SingleEmitter<String> emitter) throws Exception {
                String content = FileIOUtils.readFile2String(path);
                emitter.onSuccess(content);
            }
        });

        addSingleDisposable(single, consumer);
    }

    /**
     * @see FileSyncService#startFolderMonitor() for more details
     */
    public void save(String repoId, String localPath, String fullPath, String content, String target_file) {
        getRefreshLiveData().setValue(true);
        Single<Boolean> saveSingle = getSaveSingle(localPath, content);
        addSingleDisposable(saveSingle, new Consumer<Boolean>() {
            @Override
            public void accept(Boolean aBoolean) throws Exception {
                // The upload feature is triggered in the FileSyncService

                getRefreshLiveData().setValue(false);
                getFileIdLiveData().setValue("");
            }
        });

//        Single<Boolean> saveSingle = getSaveSingle(localPath, content);
//        Single<String> linkSingle = IO.getInstanceWithLoggedIn().execute(FileService.class).getFileUpdateLinkSync(repoId);
//
//        Single<String> single = Single.zip(saveSingle, linkSingle, new BiFunction<Boolean, String, String>() {
//            @Override
//            public String apply(Boolean aBoolean, String url) throws Exception {
//                return url;
//            }
//        }).flatMap((Function<String, SingleSource<String>>) url -> {
//            File file = new File(localPath);
//
//            String fileId = updateFile(url, file, target_file);
//            return Single.just(fileId);
//        }).flatMap(new Function<String, SingleSource<String>>() {
//            @Override
//            public SingleSource<String> apply(String fileId) throws Exception {
//                return updateModifiedAt(repoId, fullPath, fileId).flatMap(aBoolean -> Single.just(fileId));
//            }
//        });
//
//        addSingleDisposable(single, new Consumer<String>() {
//            @Override
//            public void accept(String fileId) throws Exception {
//                getRefreshLiveData().setValue(false);
//                getFileIdLiveData().setValue(fileId);
//            }
//        }, new Consumer<Throwable>() {
//            @Override
//            public void accept(Throwable throwable) throws Exception {
//                getRefreshLiveData().setValue(false);
//                SeafException seafException = getExceptionByThrowable(throwable);
//                getSeafExceptionLiveData().setValue(seafException);
//            }
//        });

    }

//    private Single<Boolean> updateModifiedAt(String repoId, String fullPath, String fileId) {
//        Single<List<FileTransferEntity>> transferSingle = AppDatabase.getInstance().fileTransferDAO().getListByFullPathsAsync(repoId, CollectionUtils.newArrayList(fullPath), TransferAction.DOWNLOAD);
//        Single<List<DirentModel>> dirSingle = AppDatabase.getInstance().direntDao().getAllByFullPath(repoId, fullPath);
//
//        return Single.zip(transferSingle, dirSingle, new BiFunction<List<FileTransferEntity>, List<DirentModel>, Boolean>() {
//            @Override
//            public Boolean apply(List<FileTransferEntity> fileTransferEntities, List<DirentModel> direntModels) throws Exception {
//
//                long now = System.currentTimeMillis();
//                if (CollectionUtils.isEmpty(fileTransferEntities)) {
//                    fileTransferEntities.get(0).modified_at = now;
//                    fileTransferEntities.get(0).file_id = fileId;
//                    AppDatabase.getInstance().fileTransferDAO().update(fileTransferEntities.get(0));
//                }
//
//                if (CollectionUtils.isEmpty(direntModels)) {
//                    direntModels.get(0).last_modified_at = now;
//                    AppDatabase.getInstance().direntDao().update(direntModels.get(0));
//                }
//
//                return true;
//            }
//        });
//    }

    private Single<Boolean> getSaveSingle(String path, String content) {
        return Single.create(new SingleOnSubscribe<Boolean>() {
            @Override
            public void subscribe(SingleEmitter<Boolean> emitter) throws Exception {
                FileIOUtils.writeFileFromString(path, content, false);
                emitter.onSuccess(true);
            }
        });
    }

//    private String updateFile(String url, File localFile, String target_file) throws IOException, SeafException {
//        //net
//        MultipartBody.Builder builder = new MultipartBody.Builder();
//        builder.setType(MultipartBody.FORM);
//
//        builder.addFormDataPart("target_file", target_file);
//
//        ProgressRequestBody progressRequestBody = new ProgressRequestBody(localFile, null);
//        builder.addFormDataPart("file", localFile.getName(), progressRequestBody);
//
//        RequestBody requestBody = builder.build();
//
//        Request request = new Request.Builder()
//                .url(url)
//                .post(requestBody)
//                .build();
//
//        Call newCall = IO.getInstanceWithLoggedIn().getClient().newCall(request);
//
//        Response response = newCall.execute();
//        if (!response.isSuccessful()) {
//            String b = response.body() != null ? response.body().string() : null;
//            SLogs.e("上传结果，失败：" + b);
//
//            //[text={"error": "Out of quota.\n"}]
//            if (b != null && b.toLowerCase().contains("out of quota")) {
//                throw SeafException.OUT_OF_QUOTA;
//            }
//
//            throw SeafException.networkException;
//        }
//
//        String str = response.body().string();
//        String fileId = str.replace("\"", "");
//        SLogs.e("上传结果，文件 ID：" + str);
//
//        return fileId;
//
//    }
}
