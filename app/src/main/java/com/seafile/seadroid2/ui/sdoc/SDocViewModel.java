package com.seafile.seadroid2.ui.sdoc;

import android.text.TextUtils;

import androidx.lifecycle.MutableLiveData;

import com.blankj.utilcode.util.CloneUtils;
import com.blankj.utilcode.util.CollectionUtils;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.framework.data.model.sdoc.FileDetailModel;
import com.seafile.seadroid2.framework.data.model.sdoc.FileProfileConfigModel;
import com.seafile.seadroid2.framework.data.model.sdoc.FileRecordWrapperModel;
import com.seafile.seadroid2.framework.data.model.sdoc.MetadataConfigModel;
import com.seafile.seadroid2.framework.data.model.sdoc.OutlineItemModel;
import com.seafile.seadroid2.framework.data.model.sdoc.SDocOutlineWrapperModel;
import com.seafile.seadroid2.framework.data.model.sdoc.SDocPageOptionsModel;
import com.seafile.seadroid2.framework.data.model.user.UserWrapperModel;
import com.seafile.seadroid2.framework.http.HttpIO;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.util.StringUtils;
import com.seafile.seadroid2.ui.base.viewmodel.BaseViewModel;

import java.util.List;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import io.reactivex.Single;
import io.reactivex.functions.BiFunction;
import io.reactivex.functions.Consumer;
import io.reactivex.functions.Function3;

public class SDocViewModel extends BaseViewModel {

    private final MutableLiveData<FileProfileConfigModel> _fileProfileConfigLiveData = new MutableLiveData<>();
    private final MutableLiveData<FileRecordWrapperModel> _fileRecordLiveData = new MutableLiveData<>();
    private final MutableLiveData<List<OutlineItemModel>> _sdocElementListLiveData = new MutableLiveData<>();

    public MutableLiveData<FileProfileConfigModel> getFileDetailLiveData() {
        return _fileProfileConfigLiveData;
    }

    public MutableLiveData<FileRecordWrapperModel> getSdocRecordLiveData() {
        return _fileRecordLiveData;
    }

    public MutableLiveData<List<OutlineItemModel>> getSdocElementLiveData() {
        return _sdocElementListLiveData;
    }

    public void loadFileDetail(String repoId, String path, boolean isMetadataEnable) {

        Single<UserWrapperModel> userSingle = HttpIO.getCurrentInstance().execute(DocsCommentService.class).getRelatedUsers(repoId);

        //Even if isMetadataEnable is enabled, you still need to check whether the enable field of MetadataConfigModel is available
        Single<MetadataConfigModel> metadataSingle = HttpIO.getCurrentInstance().execute(DocsCommentService.class).getMetadata(repoId);
        Single<FileDetailModel> detailSingle = HttpIO.getCurrentInstance().execute(DocsCommentService.class).getFileDetail(repoId, path);

        Single<FileProfileConfigModel> s;
        if (isMetadataEnable) {
            s = Single.zip(detailSingle, userSingle, metadataSingle, new Function3<FileDetailModel, UserWrapperModel, MetadataConfigModel, FileProfileConfigModel>() {
                @Override
                public FileProfileConfigModel apply(FileDetailModel fileDetailModel, UserWrapperModel userWrapperModel, MetadataConfigModel metadataConfigModel) throws Exception {
                    FileProfileConfigModel configModel = new FileProfileConfigModel();
                    configModel.setDetail(fileDetailModel);
                    configModel.setUsers(userWrapperModel);
                    configModel.setMetadataConfigModel(metadataConfigModel);
                    return configModel;
                }
            });
        } else {
            s = Single.zip(detailSingle, userSingle, new BiFunction<FileDetailModel, UserWrapperModel, FileProfileConfigModel>() {
                @Override
                public FileProfileConfigModel apply(FileDetailModel fileDetailModel, UserWrapperModel userWrapperModel) throws Exception {
                    FileProfileConfigModel configModel = new FileProfileConfigModel();
                    configModel.setDetail(fileDetailModel);
                    configModel.setUsers(userWrapperModel);
                    return configModel;
                }
            });
        }

        addSingleDisposable(s, new Consumer<FileProfileConfigModel>() {
            @Override
            public void accept(FileProfileConfigModel fileProfileConfigModel) throws Exception {
                getFileDetailLiveData().setValue(fileProfileConfigModel);
            }
        });
    }

    public void loadRecords(String repoId, String path) {
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

        if (TextUtils.isEmpty(parent_dir)) {
            parent_dir = "/";
        }

        Single<FileRecordWrapperModel> single = HttpIO.getCurrentInstance().execute(DocsCommentService.class).getRecords(repoId, parent_dir, name, name);
        addSingleDisposable(single, new Consumer<FileRecordWrapperModel>() {
            @Override
            public void accept(FileRecordWrapperModel fileRecordWrapperModel) throws Exception {
                getSdocRecordLiveData().setValue(fileRecordWrapperModel);
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

        Single<SDocOutlineWrapperModel> single = HttpIO.getInstanceByAccount(partialAccount).execute(DocsCommentService.class).getElements(pageOptionsModel.docUuid);
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
}
