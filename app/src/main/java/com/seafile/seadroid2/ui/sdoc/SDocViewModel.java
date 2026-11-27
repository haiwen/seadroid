package com.seafile.seadroid2.ui.sdoc;

import android.content.ContentResolver;
import android.content.Context;
import android.net.Uri;
import android.text.TextUtils;
import android.util.Pair;

import androidx.lifecycle.MutableLiveData;

import com.blankj.utilcode.util.CloneUtils;
import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.GsonUtils;
import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.baseviewmodel.BaseViewModel;
import com.seafile.seadroid2.framework.db.AppDatabase;
import com.seafile.seadroid2.framework.db.entities.PermissionEntity;
import com.seafile.seadroid2.framework.db.entities.RepoModel;
import com.seafile.seadroid2.framework.http.HttpIO;
import com.seafile.seadroid2.framework.http.HttpManager;
import com.seafile.seadroid2.framework.model.ResultModel;
import com.seafile.seadroid2.framework.model.sdoc.FileProfileConfigModel;
import com.seafile.seadroid2.framework.model.sdoc.OptionTagModel;
import com.seafile.seadroid2.framework.model.sdoc.OutlineItemModel;
import com.seafile.seadroid2.framework.model.sdoc.SDocOutlineWrapperModel;
import com.seafile.seadroid2.framework.model.sdoc.SDocPageOptionsModel;
import com.seafile.seadroid2.framework.model.sdoc.UploadSdocImageResultModel;
import com.seafile.seadroid2.framework.model.user.UserModel;
import com.seafile.seadroid2.framework.util.ContentResolvers;
import com.seafile.seadroid2.framework.util.ExceptionUtils;
import com.seafile.seadroid2.framework.util.Objs;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.util.StringUtils;
import com.seafile.seadroid2.framework.util.Toasts;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.ui.docs_comment.DocsCommentService;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import io.reactivex.Single;
import io.reactivex.SingleSource;
import io.reactivex.functions.Consumer;
import okhttp3.MediaType;
import okhttp3.MultipartBody;
import okhttp3.OkHttpClient;
import okhttp3.Request;
import okhttp3.RequestBody;
import okhttp3.Response;
import okhttp3.ResponseBody;

public class SDocViewModel extends BaseViewModel {
    private final MutableLiveData<Pair<String, List<UserModel>>> _onUserSelectedLiveData = new MutableLiveData<>();
    private final MutableLiveData<Pair<String, List<OptionTagModel>>> _onTagSelectedLiveData = new MutableLiveData<>();
    private final MutableLiveData<Boolean> _onSaveLiveData = new MutableLiveData<>();
    private final MutableLiveData<List<UploadSdocImageResultModel>> _uploadImageResultIntoSdocLiveData = new MutableLiveData<>();

    public MutableLiveData<List<UploadSdocImageResultModel>> getUploadImageResultIntoSdocLiveData() {
        return _uploadImageResultIntoSdocLiveData;
    }

    public MutableLiveData<Boolean> getOnSaveLiveData() {
        return _onSaveLiveData;
    }

    public MutableLiveData<Pair<String, List<UserModel>>> getOnUserSelectedLiveData() {
        return _onUserSelectedLiveData;
    }

    public MutableLiveData<Pair<String, List<OptionTagModel>>> getOnTagSelectedLiveData() {
        return _onTagSelectedLiveData;
    }

    private final MutableLiveData<FileProfileConfigModel> _fileProfileConfigLiveData = new MutableLiveData<>();
    private final MutableLiveData<List<OutlineItemModel>> _sdocElementListLiveData = new MutableLiveData<>();
    private final MutableLiveData<String> _outlineValueLiveData = new MutableLiveData<>();

    public void setOutlineValue(String data) {
        _outlineValueLiveData.setValue(data);
    }

    public MutableLiveData<String> getOutlineValueLiveData() {
        return _outlineValueLiveData;
    }

    public MutableLiveData<FileProfileConfigModel> getFileDetailLiveData() {
        return _fileProfileConfigLiveData;
    }


    public MutableLiveData<List<OutlineItemModel>> getSdocElementLiveData() {
        return _sdocElementListLiveData;
    }


    public void getRepoModelAndPermissionEntity(String repoId, Consumer<Pair<RepoModel, PermissionEntity>> consumer) {
        Single<Pair<RepoModel, PermissionEntity>> r = getSingleForLoadRepoModelAndAllPermission(repoId);
        addSingleDisposable(r, new Consumer<Pair<RepoModel, PermissionEntity>>() {
            @Override
            public void accept(Pair<RepoModel, PermissionEntity> pair) throws Exception {
                if (consumer != null) {
                    consumer.accept(pair);
                }
            }
        });
    }

    /**
     * get the repoModel and repoMode's PermissionEntity from local, if not exist, get from remote.
     */
    private Single<Pair<RepoModel, PermissionEntity>> getSingleForLoadRepoModelAndAllPermission(String repoId) {
        Single<List<RepoModel>> repoSingle = AppDatabase.getInstance().repoDao().getRepoById(repoId);
        return repoSingle.flatMap(new io.reactivex.functions.Function<List<RepoModel>, SingleSource<Pair<RepoModel, PermissionEntity>>>() {
            @Override
            public SingleSource<Pair<RepoModel, PermissionEntity>> apply(List<RepoModel> repoModels) throws Exception {
                if (CollectionUtils.isEmpty(repoModels)) {
                    return Single.just(new Pair<>(null, null));
                }

                RepoModel repoModel = repoModels.get(0);
                if (!repoModel.isCustomPermission()) {
                    return Single.just(new Pair<>(repoModel, new PermissionEntity(repoId, repoModel.permission)));
                }

                Single<List<PermissionEntity>> pSingle = AppDatabase.getInstance().permissionDAO().getByRepoAndIdAsync(repoId, repoModel.getCustomPermissionNum());
                return pSingle.flatMap((io.reactivex.functions.Function<List<PermissionEntity>, SingleSource<Pair<RepoModel, PermissionEntity>>>) pList -> {
                    //no data in local db
                    if (CollectionUtils.isEmpty(pList)) {
                        return Single.just(new Pair<>(repoModel, new PermissionEntity(repoModel.repo_id, "r")));
                    }

                    //get first permission
                    return Single.just(new Pair<>(repoModel, pList.get(0)));
                });
            }
        });
    }


    public void loadFileDetail(String repoId, String path) {
        getSecondRefreshLiveData().setValue(true);

        Single<FileProfileConfigModel> s = Objs.getLoadFileDetailSingle(repoId, path);

        addSingleDisposable(s, new Consumer<FileProfileConfigModel>() {
            @Override
            public void accept(FileProfileConfigModel fileProfileConfigModel) throws Exception {
                getSecondRefreshLiveData().setValue(false);
                getFileDetailLiveData().setValue(fileProfileConfigModel);
            }
        }, new Consumer<Throwable>() {
            @Override
            public void accept(Throwable throwable) {
                getSecondRefreshLiveData().setValue(false);
                SeafException seafException = ExceptionUtils.parseByThrowable(throwable);
                Toasts.show(seafException.getMessage());
            }
        });
    }

    public static final List<String> _AllowedElementTypes = List.of("header1", "header2", "header3");

    public void loadSdocElements(SDocPageOptionsModel pageOptionsModel) {
        if (TextUtils.isEmpty(pageOptionsModel.seadocServerUrl)) {
            return;
        }
        getRefreshLiveData().setValue(true);

        String sdocServerUrl = pageOptionsModel.seadocServerUrl;
        if (!sdocServerUrl.endsWith("/")) {
            sdocServerUrl = sdocServerUrl + "/";
        }

        Account curAccount = SupportAccountManager.getInstance().getCurrentAccount();
        Account partialAccount = CloneUtils.deepClone(curAccount, Account.class);
        partialAccount.setToken(pageOptionsModel.seadocAccessToken);
        partialAccount.setServer(sdocServerUrl);

        Single<SDocOutlineWrapperModel> single = HttpManager.getHttpWithAccount(partialAccount).execute(DocsCommentService.class).getElements(pageOptionsModel.docUuid);
        addSingleDisposable(single, new Consumer<SDocOutlineWrapperModel>() {
            @Override
            public void accept(SDocOutlineWrapperModel wrapperModel) throws Exception {

                if (wrapperModel == null || wrapperModel.elements == null) {
                    getSdocElementLiveData().setValue(null);
                    return;
                }

                List<OutlineItemModel> newList = wrapperModel.elements.stream().filter(new Predicate<OutlineItemModel>() {
                    @Override
                    public boolean test(OutlineItemModel sDocModel) {
                        if (!_AllowedElementTypes.contains(sDocModel.type)) {
                            return false;
                        }

                        if (TextUtils.isEmpty(sDocModel.text) && CollectionUtils.isEmpty(sDocModel.children)) {
                            return false;
                        }

                        return true;
                    }
                }).map(new Function<OutlineItemModel, OutlineItemModel>() {
                    @Override
                    public OutlineItemModel apply(OutlineItemModel sDocModel) {
                        if (!TextUtils.isEmpty(sDocModel.text)) {
                            return sDocModel;
                        }

                        if (CollectionUtils.isEmpty(sDocModel.children)) {
                            return sDocModel;
                        }

                        String text = "";
                        for (OutlineItemModel child : sDocModel.children) {
                            if (!TextUtils.isEmpty(child.text)) {
                                String nt = StringUtils.trim(child.text, "\n").trim();
                                text = text.concat(nt);
                            }
                        }
                        sDocModel.text = text;
                        return sDocModel;
                    }
                }).collect(Collectors.toList());

                getSdocElementLiveData().setValue(newList);
                getRefreshLiveData().setValue(false);
            }
        }, new Consumer<Throwable>() {
            @Override
            public void accept(Throwable throwable) throws Exception {
                SLogs.e(throwable);
                getRefreshLiveData().setValue(false);
            }
        });
    }

    public void putRecord(String repoId, String recordId, Map<String, Object> data, List<String> tagIds) {
        List<Single<ResultModel>> singleList = new ArrayList<>();
        if (CollectionUtils.isNotEmpty(tagIds)) {
            List<Map<String, Object>> fileTags = new ArrayList<>();
            Map<String, Object> tagParams = new HashMap<>();
            tagParams.put("record_id", recordId);
            tagParams.put("tags", tagIds);
            fileTags.add(tagParams);

            Map<String, Object> t = new HashMap<>();
            t.put("file_tags_data", fileTags);
            SLogs.d("Tag request params: ");
            SLogs.d(GsonUtils.toJson(t));

            Single<ResultModel> tagSingle = HttpManager.getCurrentHttp().execute(SDocService.class).putRecordTag(repoId, t);
            singleList.add(tagSingle);
        }

        if (data != null && !data.isEmpty()) {
            Map<String, Object> params = new HashMap<>();
            params.put("data", data);
            params.put("record_id", recordId);
            SLogs.d("Data request params: ");
            SLogs.d(GsonUtils.toJson(params));
            Single<ResultModel> recordSingle = HttpManager.getCurrentHttp().execute(SDocService.class).putRecord(repoId, params);
            singleList.add(recordSingle);
        }

        if (singleList.isEmpty()) {
            return;
        }

        getSecondRefreshLiveData().setValue(true);

        Single<List<ResultModel>> zipSingle = Single.zip(singleList, new io.reactivex.functions.Function<Object[], List<ResultModel>>() {
            @Override
            public List<ResultModel> apply(Object[] objects) throws Exception {
                List<ResultModel> results = new ArrayList<>();
                for (Object obj : objects) {
                    results.add((ResultModel) obj);
                }
                return results;
            }
        });

        addSingleDisposable(zipSingle, new Consumer<List<ResultModel>>() {
            @Override
            public void accept(List<ResultModel> resultModels) throws Exception {
                getSecondRefreshLiveData().setValue(false);
                getOnSaveLiveData().setValue(true);
            }
        }, new Consumer<Throwable>() {
            @Override
            public void accept(Throwable throwable) throws Exception {
                SeafException seafException = ExceptionUtils.parseByThrowable(throwable);
                getSeafExceptionLiveData().setValue(seafException);
                getSecondRefreshLiveData().setValue(false);
            }
        });
    }

    public void uploadImageToSDoc(String sdocUUid, String seadocAccessToken, List<Uri> uris) {
        getSecondRefreshLiveData().setValue(true);

        String server = HttpManager.getCurrentHttp().getCurrentServer();
        String uploadUrl = Utils.pathJoin(server, "/api/v2.1/seadoc/upload-image/", sdocUUid, "/");


        List<Single<UploadSdocImageResultModel>> uploadSingles = new ArrayList<>();
        for (Uri uri : uris) {
            Single<UploadSdocImageResultModel> uploadSingle = uploadImage(uploadUrl, seadocAccessToken, uri);
            uploadSingles.add(uploadSingle);
        }

        Single<List<UploadSdocImageResultModel>> zipSingle = Single.zip(uploadSingles, objects -> {
            List<UploadSdocImageResultModel> results = new ArrayList<>();
            for (Object obj : objects) {
                results.add((UploadSdocImageResultModel) obj);
            }
            return results;
        });

        addSingleDisposable(zipSingle, results -> {
            getSecondRefreshLiveData().setValue(false);
            getUploadImageResultIntoSdocLiveData().setValue(results);
        }, throwable -> {
            SeafException seafException = ExceptionUtils.parseByThrowable(throwable);
            getSeafExceptionLiveData().setValue(seafException);
            SLogs.e(seafException);
            getSecondRefreshLiveData().setValue(false);
            Toasts.show(seafException.getMessage());
        });
    }

    public Single<UploadSdocImageResultModel> uploadImage(String uploadUrl, String seadocAccessToken, Uri uri) {
        return Single.fromCallable(() -> {
            if (TextUtils.isEmpty(uploadUrl)) {
                throw new IllegalArgumentException("uploadUrl is empty");
            }
            if (uri == null) {
                throw new IllegalArgumentException("uri is null");
            }


            Context context = SeadroidApplication.getAppContext();
            ContentResolver contentResolver = context.getContentResolver();
            String fileName = ContentResolvers.getFileNameFromUri(contentResolver, uri);
            if (TextUtils.isEmpty(fileName)) {
                fileName = "upload.bin";
            }

            String mime = contentResolver.getType(uri);
            if (TextUtils.isEmpty(mime)) {
                mime = "application/octet-stream";
            }
            MediaType mediaType = MediaType.parse(mime);
            if (mediaType == null) {
                mediaType = MediaType.parse("application/octet-stream");
            }

            final MediaType finalMediaType = mediaType;

            RequestBody fileBody = new RequestBody() {
                @Override
                public MediaType contentType() {
                    return finalMediaType;
                }

                @Override
                public void writeTo(okio.BufferedSink sink) throws IOException {
                    try (InputStream inputStream = contentResolver.openInputStream(uri)) {
                        if (inputStream == null) {
                            throw new IOException("Failed to open input stream for uri: " + uri);
                        }
                        sink.writeAll(okio.Okio.source(inputStream));
                    }
                }
            };

            MultipartBody requestBody = new MultipartBody.Builder()
                    .setType(MultipartBody.FORM)
                    .addFormDataPart("file", fileName, fileBody)
                    .build();

            Request.Builder requestBuilder = new Request.Builder()
                    .url(uploadUrl)
                    .post(requestBody);

            if (!TextUtils.isEmpty(seadocAccessToken)) {
                requestBuilder.addHeader("Authorization", "Token " + seadocAccessToken);
            }
            requestBuilder.addHeader("Accept", "application/json");
            requestBuilder.addHeader("charset", "utf-8");
            requestBuilder.addHeader("timestamp", String.valueOf(System.currentTimeMillis()));

            Request request = requestBuilder.build();

            OkHttpClient.Builder builder = new OkHttpClient.Builder();
            OkHttpClient okHttpClient = builder.build();

            try (Response response = okHttpClient.newCall(request).execute()) {
                int code = response.code();
                try (ResponseBody responseBody = response.body()) {
                    String body = responseBody != null ? responseBody.string() : "";
                    if (!response.isSuccessful()) {
                        throw new IOException("upload failed, code=" + code + ", body=" + body);
                    }

                    return GsonUtils.fromJson(body, UploadSdocImageResultModel.class);
                }
            }
        });
    }
}
