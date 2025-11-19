package com.seafile.seadroid2.ui.docs_comment;

import android.content.ContentResolver;
import android.net.Uri;
import android.text.TextUtils;

import androidx.lifecycle.MutableLiveData;

import com.blankj.utilcode.util.CloneUtils;
import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.TimeUtils;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.framework.model.ResultModel;
import com.seafile.seadroid2.framework.model.docs_comment.DocsCommentWrapperModel;
import com.seafile.seadroid2.framework.model.docs_comment.DocsUploadResultModel;
import com.seafile.seadroid2.framework.model.docs_comment.DocsCommentModel;
import com.seafile.seadroid2.framework.model.docs_comment.DocsCommentsWrapperModel;
import com.seafile.seadroid2.framework.model.sdoc.SDocPageOptionsModel;
import com.seafile.seadroid2.framework.http.HttpIO;
import com.seafile.seadroid2.framework.model.user.UserModel;
import com.seafile.seadroid2.framework.model.user.UserWrapperModel;
import com.seafile.seadroid2.framework.util.ContentResolvers;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.util.StringUtils;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.baseviewmodel.BaseViewModel;
import com.seafile.seadroid2.ui.sdoc.DocsCommentService;
import com.seafile.seadroid2.ui.sdoc.SDocService;
import com.seafile.seadroid2.view.rich_edittext.RichEditText;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

import io.reactivex.Flowable;
import io.reactivex.Single;
import io.reactivex.functions.Consumer;
import kotlin.Pair;
import okhttp3.MediaType;
import okhttp3.MultipartBody;
import okhttp3.RequestBody;

public class DocsCommentViewModel extends BaseViewModel {

    private final MutableLiveData<DocsCommentsWrapperModel> _fileCommentLiveData = new MutableLiveData<>();
    private final MutableLiveData<Boolean> _postCommentLiveData = new MutableLiveData<>();
    private final MutableLiveData<List<UserModel>> _relatedUsersLiveData = new MutableLiveData<>();

    public void setRelatedUsers(List<UserModel> users) {
        _relatedUsersLiveData.setValue(users);
    }

    public MutableLiveData<List<UserModel>> getRelatedUsersLiveData() {
        return _relatedUsersLiveData;
    }

    public MutableLiveData<Boolean> getPostCommentLiveData() {
        return _postCommentLiveData;
    }

    public MutableLiveData<DocsCommentsWrapperModel> getSdocCommentLiveData() {
        return _fileCommentLiveData;
    }

    public void getRelatedUsers(String repoId) {
        Single<UserWrapperModel> userSingle = HttpIO.getCurrentInstance().execute(SDocService.class).getRelatedUsers(repoId);
        addSingleDisposable(userSingle, new Consumer<UserWrapperModel>() {
            @Override
            public void accept(UserWrapperModel userWrapperModel) throws Exception {
                setRelatedUsers(userWrapperModel.user_list);
            }
        });
    }

    public void loadDocComments(SDocPageOptionsModel pageOptionsModel) {
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

        Single<DocsCommentsWrapperModel> commentSingle = HttpIO.getInstanceByAccount(partialAccount).execute(DocsCommentService.class).getComments(pageOptionsModel.docUuid);
        addSingleDisposable(commentSingle, new Consumer<DocsCommentsWrapperModel>() {
            @Override
            public void accept(DocsCommentsWrapperModel docsCommentsWrapperModel) throws Exception {
                docsCommentsWrapperModel.comments = docsCommentsWrapperModel.comments.stream().sorted(new Comparator<DocsCommentModel>() {
                    @Override
                    public int compare(DocsCommentModel o1, DocsCommentModel o2) {
                        return o1.created_at.compareTo(o2.created_at);
                    }
                }).map(new Function<DocsCommentModel, DocsCommentModel>() {
                    @Override
                    public DocsCommentModel apply(DocsCommentModel docsCommentModel) {

                        Pair<Boolean, List<RichEditText.RichContentModel>> pair = formatContent(docsCommentModel.comment);
                        if (pair != null) {
                            docsCommentModel.commentList = pair.getSecond();
                            docsCommentModel.isContainImage = pair.getFirst();
                        }

                        return docsCommentModel;
                    }
                }).collect(Collectors.toList());

                getSdocCommentLiveData().setValue(docsCommentsWrapperModel);
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


    private Pair<Boolean, List<RichEditText.RichContentModel>> formatContent(String comment) {
        if (TextUtils.isEmpty(comment)) {
            return null;
        }

        List<RichEditText.RichContentModel> models = CollectionUtils.newArrayList();

        String[] lines = org.apache.commons.lang3.StringUtils.split(comment, "\n\n");
        for (String line : lines) {
            if (TextUtils.isEmpty(line)) {
                continue;
            }

            int imgLabelCount = StringUtils.countMatches(line, imgPrefix);
            int imgMdCount = StringUtils.countMatches(line, imgMdPrefix);
            if (imgMdCount == 0 && imgLabelCount == 0) {
                RichEditText.RichContentModel m = new RichEditText.RichContentModel();
                m.type = 0;
                m.content = line;
                models.add(m);

                continue;
            }

            if (imgLabelCount > 0) {
                models.addAll(getImageLabelResult(line));
            }

            if (imgMdCount > 0) {
                models.addAll(getImageMdResult(line));
            }
        }
        return new Pair<>(true, models);
    }

    private final String imgPrefix = "<img";
    private final String imgSuffix = ">";

    private final String imgMdPrefix = "![](";
    private final String imgMdSuffix = ")";

    private List<RichEditText.RichContentModel> getImageLabelResult(String s) {
        List<RichEditText.RichContentModel> models = CollectionUtils.newArrayList();

        int start = org.apache.commons.lang3.StringUtils.indexOf(s, imgPrefix);
        int end = org.apache.commons.lang3.StringUtils.indexOf(s, imgSuffix);
        if (start > 0) {
            String startStr = org.apache.commons.lang3.StringUtils.substring(s, 0, start);
            RichEditText.RichContentModel m = new RichEditText.RichContentModel();
            m.type = 0;
            m.content = startStr;
            models.add(m);
        }

        String sss = org.apache.commons.lang3.StringUtils.substring(s, start, end + 1);
        String a = sss.replaceAll("<img src=\"", " ").replace("\" height=\"60\">", " ");
        String content = a.trim();// URLEncoder.encode(,"utf-8");
        if (!TextUtils.isEmpty(content) && !TextUtils.equals("null", content.toLowerCase(Locale.getDefault()))) {
            RichEditText.RichContentModel m = new RichEditText.RichContentModel();
            m.type = 1;
            m.content = content;
            models.add(m);
        }

        s = s.substring(end + 1);
        if (!org.apache.commons.lang3.StringUtils.isEmpty(s)) {
            RichEditText.RichContentModel m = new RichEditText.RichContentModel();
            m.type = 0;
            m.content = s;
            models.add(m);
        }

        return models;
    }

    private List<RichEditText.RichContentModel> getImageMdResult(String s) {
        List<RichEditText.RichContentModel> models = CollectionUtils.newArrayList();

        int start = org.apache.commons.lang3.StringUtils.indexOf(s, imgMdPrefix);
        int end = org.apache.commons.lang3.StringUtils.indexOf(s, imgMdSuffix);
        if (start > 0) {
            //check text before image content
            String startStr = org.apache.commons.lang3.StringUtils.substring(s, 0, start);
            RichEditText.RichContentModel m = new RichEditText.RichContentModel();
            m.type = 0;
            m.content = startStr;
            models.add(m);
        }

        String sss = org.apache.commons.lang3.StringUtils.substring(s, start + 4, end);
        String content = sss.trim();// URLEncoder.encode(,"utf-8");
        if (!TextUtils.isEmpty(content) && !TextUtils.equals("null", content.toLowerCase(Locale.getDefault()))) {
            RichEditText.RichContentModel m = new RichEditText.RichContentModel();
            m.type = 1;
            m.content = content;
            models.add(m);
        }

        s = s.substring(end + 1);
        if (!org.apache.commons.lang3.StringUtils.isEmpty(s)) {
            RichEditText.RichContentModel m = new RichEditText.RichContentModel();
            m.type = 0;
            m.content = s;
            models.add(m);
        }

        return models;
    }

    public void markResolve(String sdocServerUrl, String token, String sdocUid, long commentId, Consumer<Long> consumer) {
        getRefreshLiveData().setValue(true);

        if (!sdocServerUrl.endsWith("/")) {
            sdocServerUrl = sdocServerUrl + "/";
        }

        Account curAccount = SupportAccountManager.getInstance().getCurrentAccount();
        Account partialAccount = CloneUtils.deepClone(curAccount, Account.class);
        partialAccount.setServer(sdocServerUrl);
        partialAccount.setToken(token);

        Map<String, Object> params = new HashMap<>();
        params.put("resolved", true);

        Single<ResultModel> resolvedSingle = HttpIO.getInstanceByAccount(partialAccount).execute(DocsCommentService.class).markResolved(sdocUid, commentId, params);
        addSingleDisposable(resolvedSingle, new Consumer<ResultModel>() {
            @Override
            public void accept(ResultModel resultModel) throws Exception {
                getRefreshLiveData().setValue(false);
                if (consumer != null) {
                    consumer.accept(commentId);
                }
            }
        }, new Consumer<Throwable>() {
            @Override
            public void accept(Throwable throwable) {
                getRefreshLiveData().setValue(false);
            }
        });
    }

    public void delete(String sdocServerUrl, String token, String sdocUid, long commentId, Consumer<Long> consumer) {
        getRefreshLiveData().setValue(true);

        if (!sdocServerUrl.endsWith("/")) {
            sdocServerUrl = sdocServerUrl + "/";
        }

        Account curAccount = SupportAccountManager.getInstance().getCurrentAccount();
        Account partialAccount = CloneUtils.deepClone(curAccount, Account.class);
        partialAccount.setServer(sdocServerUrl);
        partialAccount.setToken(token);

        Single<ResultModel> resolvedSingle = HttpIO.getInstanceByAccount(partialAccount)
                .execute(DocsCommentService.class)
                .delete(sdocUid, commentId);
        addSingleDisposable(resolvedSingle, new Consumer<ResultModel>() {
            @Override
            public void accept(ResultModel resultModel) throws Exception {
                getRefreshLiveData().setValue(false);
                if (consumer != null) {
                    consumer.accept(commentId);
                }
            }
        }, new Consumer<Throwable>() {
            @Override
            public void accept(Throwable throwable) {
                getRefreshLiveData().setValue(false);
            }
        });
    }


    public void uploadFile(ContentResolver contentResolver, Uri uri, String docUid, String token, Consumer<String> consumer, Consumer<String> errorCallBack) {
        String fileName = ContentResolvers.getFileNameFromUri(contentResolver, uri);

        byte[] fileContent = ContentResolvers.getFileContentFromUri(contentResolver, uri);
        RequestBody body = RequestBody.create(MediaType.parse("application/octet-stream"), fileContent);
        MultipartBody.Part filePart = MultipartBody.Part.createFormData("file", fileName, body);

        Map<String, RequestBody> partMap = new HashMap<>();
        partMap.put("authorization", RequestBody.create(MediaType.parse("text"), "Token " + token));

        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        account.token = token;
        Flowable<DocsUploadResultModel> uploadFile = HttpIO.getInstanceByAccount(account).execute(DocsCommentService.class).upload(docUid, filePart, partMap);

        addFlowableDisposable(uploadFile, new Consumer<DocsUploadResultModel>() {
            @Override
            public void accept(DocsUploadResultModel resultModel) throws Exception {
                if (consumer != null) {

                    String sName = resultModel.relative_path.get(0);
                    String sUrl = HttpIO.getCurrentInstance().getServerUrl();
                    String absUrl = Utils.pathJoin(sUrl, "api", "v2.1", "seadoc", "download-image", docUid, sName);

                    consumer.accept(absUrl);
                }
            }
        }, new Consumer<Throwable>() {
            @Override
            public void accept(Throwable throwable) throws Exception {
                if (errorCallBack != null) {
                    errorCallBack.accept(uri.toString());
                }
            }
        });

    }


    public void postComment(SDocPageOptionsModel pageOptionsModel, String comment) {
        getRefreshLiveData().setValue(true);

        String sdocServerUrl = pageOptionsModel.seadocServerUrl;
        if (TextUtils.isEmpty(sdocServerUrl)) {
            getRefreshLiveData().setValue(false);
            getSeafExceptionLiveData().setValue(SeafException.REQUEST_URL_EXCEPTION);
            return;
        }

        if (!sdocServerUrl.endsWith("/")) {
            sdocServerUrl = sdocServerUrl + "/";
        }

        Account curAccount = SupportAccountManager.getInstance().getCurrentAccount();
        Account partialAccount = CloneUtils.deepClone(curAccount, Account.class);
        partialAccount.setToken(pageOptionsModel.seadocAccessToken);
        partialAccount.setServer(sdocServerUrl);

        Map<String, Object> detail = new HashMap<>();
        detail.put("element_id", "0");
        detail.put("comment", comment);

        Map<String, Object> params = new HashMap<>();
        params.put("comment", comment);
        params.put("detail", detail);
        params.put("author", partialAccount.email);
        params.put("updated_at", TimeUtils.getNowString());


        Single<DocsCommentWrapperModel> single = HttpIO.getInstanceByAccount(partialAccount).execute(DocsCommentService.class).postComment(pageOptionsModel.docUuid, params);
        addSingleDisposable(single, new Consumer<DocsCommentWrapperModel>() {
            @Override
            public void accept(DocsCommentWrapperModel docsCommentWrapperModel) throws Exception {
                getRefreshLiveData().setValue(false);
                getPostCommentLiveData().setValue(true);
            }
        }, new Consumer<Throwable>() {
            @Override
            public void accept(Throwable throwable) throws Exception {
                getRefreshLiveData().setValue(false);
                getSeafExceptionLiveData().setValue(getSeafExceptionByThrowable(throwable));
            }
        });
    }
}
