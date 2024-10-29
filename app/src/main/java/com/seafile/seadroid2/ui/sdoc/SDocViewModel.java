package com.seafile.seadroid2.ui.sdoc;

import android.text.TextUtils;

import androidx.lifecycle.MutableLiveData;

import com.blankj.utilcode.util.CloneUtils;
import com.blankj.utilcode.util.CollectionUtils;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.framework.data.model.sdoc.MetadataConfigModel;
import com.seafile.seadroid2.framework.data.model.sdoc.SDocCommentModel;
import com.seafile.seadroid2.framework.data.model.sdoc.SDocCommentWrapperModel;
import com.seafile.seadroid2.framework.data.model.sdoc.SDocDetailModel;
import com.seafile.seadroid2.framework.data.model.sdoc.SDocModel;
import com.seafile.seadroid2.framework.data.model.sdoc.SDocPageOptionsModel;
import com.seafile.seadroid2.framework.data.model.sdoc.SDocProfileConfigModel;
import com.seafile.seadroid2.framework.data.model.sdoc.SDocRecordWrapperModel;
import com.seafile.seadroid2.framework.data.model.sdoc.SDocWrapperModel;
import com.seafile.seadroid2.framework.data.model.user.UserWrapperModel;
import com.seafile.seadroid2.framework.http.HttpIO;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.util.StringUtils;
import com.seafile.seadroid2.ui.base.viewmodel.BaseViewModel;

import java.util.Comparator;
import java.util.List;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import io.reactivex.Single;
import io.reactivex.functions.Consumer;
import io.reactivex.functions.Function3;

public class SDocViewModel extends BaseViewModel {

    private final MutableLiveData<SDocProfileConfigModel> _fileProfileConfigLiveData = new MutableLiveData<>();
    private final MutableLiveData<SDocRecordWrapperModel> _sdocRecordLiveData = new MutableLiveData<>();
    private final MutableLiveData<SDocCommentWrapperModel> _sdocCommentLiveData = new MutableLiveData<>();
    private final MutableLiveData<List<SDocModel>> _sdocElementListLiveData = new MutableLiveData<>();

    public MutableLiveData<SDocProfileConfigModel> getFileDetailLiveData() {
        return _fileProfileConfigLiveData;
    }

    public MutableLiveData<SDocRecordWrapperModel> getSdocRecordLiveData() {
        return _sdocRecordLiveData;
    }

    public MutableLiveData<SDocCommentWrapperModel> getSdocCommentLiveData() {
        return _sdocCommentLiveData;
    }

    public MutableLiveData<List<SDocModel>> getSdocElementLiveData() {
        return _sdocElementListLiveData;
    }

    public void initSDocConfig(String repoId, String path) {
        Single<UserWrapperModel> userSingle = HttpIO.getCurrentInstance().execute(SDocService.class).getRelatedUsers(repoId);
        Single<MetadataConfigModel> metadataSingle = HttpIO.getCurrentInstance().execute(SDocService.class).getMetadata(repoId);
        Single<SDocDetailModel> detailSingle = HttpIO.getCurrentInstance().execute(SDocService.class).getFileDetail(repoId, path);

        Single<SDocProfileConfigModel> s = Single.zip(detailSingle, userSingle, metadataSingle, new Function3<SDocDetailModel, UserWrapperModel, MetadataConfigModel, SDocProfileConfigModel>() {
            @Override
            public SDocProfileConfigModel apply(SDocDetailModel docDetailModel, UserWrapperModel userWrapperModel, MetadataConfigModel metadataConfigModel) throws Exception {
                SDocProfileConfigModel configModel = new SDocProfileConfigModel();
                configModel.setDetail(docDetailModel);
                configModel.setUsers(userWrapperModel);
                configModel.setMetadata(metadataConfigModel);
                return configModel;
            }
        });

        addSingleDisposable(s, new Consumer<SDocProfileConfigModel>() {
            @Override
            public void accept(SDocProfileConfigModel sDocProfileConfigModel) throws Exception {
                getFileDetailLiveData().setValue(sDocProfileConfigModel);
            }
        });
    }

    public void getRecords(String repoId, String path) {
        if (TextUtils.isEmpty(path) || TextUtils.equals("/", path)) {
            return;
        }

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

        Single<SDocRecordWrapperModel> single = HttpIO.getCurrentInstance().execute(SDocService.class).getRecords(repoId, parent_dir, name);
        addSingleDisposable(single, new Consumer<SDocRecordWrapperModel>() {
            @Override
            public void accept(SDocRecordWrapperModel sDocRecordWrapperModel) throws Exception {
                getSdocRecordLiveData().setValue(sDocRecordWrapperModel);
            }
        });
    }

    public static final List<String> _AllowedElementTypes = List.of("header1", "header2", "header3");

    public void getSDocElements(SDocPageOptionsModel pageOptionsModel) {
        if (TextUtils.isEmpty(pageOptionsModel.seadocServerUrl)) {
            return;
        }

        String sdocServerUrl = pageOptionsModel.seadocServerUrl;
        if (!sdocServerUrl.endsWith("/")) {
            sdocServerUrl = sdocServerUrl + "/";
        }

        Account curAccount = SupportAccountManager.getInstance().getCurrentAccount();
        Account partialAccount = CloneUtils.deepClone(curAccount, Account.class);
        partialAccount.setToken(pageOptionsModel.seadocAccessToken);
        partialAccount.setServer(sdocServerUrl);

        Single<SDocWrapperModel> single = HttpIO.getInstanceByAccount(partialAccount).execute(SDocService.class).getElements(pageOptionsModel.docUuid);
        addSingleDisposable(single, new Consumer<SDocWrapperModel>() {
            @Override
            public void accept(SDocWrapperModel wrapperModel) throws Exception {

                if (wrapperModel == null || wrapperModel.elements == null) {
                    getSdocElementLiveData().setValue(null);
                    return;
                }

                List<SDocModel> newList = wrapperModel.elements.stream().filter(new Predicate<SDocModel>() {
                    @Override
                    public boolean test(SDocModel sDocModel) {
                        if (!_AllowedElementTypes.contains(sDocModel.type)) {
                            return false;
                        }

                        if (TextUtils.isEmpty(sDocModel.text) && CollectionUtils.isEmpty(sDocModel.children)) {
                            return false;
                        }

                        return true;
                    }
                }).map(new Function<SDocModel, SDocModel>() {
                    @Override
                    public SDocModel apply(SDocModel sDocModel) {
                        if (!TextUtils.isEmpty(sDocModel.text)) {
                            return sDocModel;
                        }

                        if (CollectionUtils.isEmpty(sDocModel.children)) {
                            return sDocModel;
                        }

                        String text = "";
                        for (SDocModel child : sDocModel.children) {
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
            }
        }, new Consumer<Throwable>() {
            @Override
            public void accept(Throwable throwable) throws Exception {
                SLogs.e(throwable);
            }
        });

    }

    public void getSDocComments(SDocPageOptionsModel pageOptionsModel) {
        if (TextUtils.isEmpty(pageOptionsModel.seadocServerUrl)) {
            return;
        }

        String sdocServerUrl = pageOptionsModel.seadocServerUrl;
        if (!sdocServerUrl.endsWith("/")) {
            sdocServerUrl = sdocServerUrl + "/";
        }

        Account curAccount = SupportAccountManager.getInstance().getCurrentAccount();
        Account partialAccount = CloneUtils.deepClone(curAccount, Account.class);
        partialAccount.setToken(pageOptionsModel.seadocAccessToken);
        partialAccount.setServer(sdocServerUrl);

        Single<SDocCommentWrapperModel> commentSingle = HttpIO.getInstanceByAccount(partialAccount).execute(SDocService.class).getComments(pageOptionsModel.docUuid);
        addSingleDisposable(commentSingle, new Consumer<SDocCommentWrapperModel>() {
            @Override
            public void accept(SDocCommentWrapperModel sDocCommentWrapperModel) throws Exception {
                sDocCommentWrapperModel.comments = sDocCommentWrapperModel.comments.stream().sorted(new Comparator<SDocCommentModel>() {
                    @Override
                    public int compare(SDocCommentModel o1, SDocCommentModel o2) {
                        return -o1.created_at.compareTo(o2.created_at);
                    }
                }).collect(Collectors.toList());

                getSdocCommentLiveData().setValue(sDocCommentWrapperModel);
            }
        }, new Consumer<Throwable>() {
            @Override
            public void accept(Throwable throwable) throws Exception {
                SLogs.e(throwable);
            }
        });

    }
}
