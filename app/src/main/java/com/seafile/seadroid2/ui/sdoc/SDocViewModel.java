package com.seafile.seadroid2.ui.sdoc;

import android.text.TextUtils;
import android.util.Pair;

import androidx.lifecycle.MutableLiveData;

import com.blankj.utilcode.util.CloneUtils;
import com.blankj.utilcode.util.CollectionUtils;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.framework.db.AppDatabase;
import com.seafile.seadroid2.framework.db.entities.PermissionEntity;
import com.seafile.seadroid2.framework.db.entities.RepoModel;
import com.seafile.seadroid2.framework.http.HttpIO;
import com.seafile.seadroid2.framework.model.sdoc.FileDetailModel;
import com.seafile.seadroid2.framework.model.sdoc.FileProfileConfigModel;
import com.seafile.seadroid2.framework.model.sdoc.FileRecordWrapperModel;
import com.seafile.seadroid2.framework.model.sdoc.FileTagWrapperModel;
import com.seafile.seadroid2.framework.model.sdoc.MetadataConfigModel;
import com.seafile.seadroid2.framework.model.sdoc.OutlineItemModel;
import com.seafile.seadroid2.framework.model.sdoc.SDocOutlineWrapperModel;
import com.seafile.seadroid2.framework.model.sdoc.SDocPageOptionsModel;
import com.seafile.seadroid2.framework.model.user.UserWrapperModel;
import com.seafile.seadroid2.framework.util.ExceptionUtils;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.util.StringUtils;
import com.seafile.seadroid2.framework.util.Toasts;
import com.seafile.seadroid2.baseviewmodel.BaseViewModel;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import io.reactivex.Single;
import io.reactivex.SingleSource;
import io.reactivex.functions.BiFunction;
import io.reactivex.functions.Consumer;

public class SDocViewModel extends BaseViewModel {

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
     * get the repoModel and repoMode‘s PermissionEntity from local, if not exist, get from remote.
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

        //Even if isMetadataEnable is enabled, you still need to check whether the enable field of MetadataConfigModel is available
        Single<MetadataConfigModel> metadataSingle = HttpIO.getCurrentInstance().execute(SDocService.class).getMetadata(repoId).onErrorReturnItem(new MetadataConfigModel());
        Single<FileDetailModel> detailSingle = HttpIO.getCurrentInstance().execute(SDocService.class).getFileDetail(repoId, path);

        Single<FileProfileConfigModel> s = Single.zip(metadataSingle, detailSingle, new BiFunction<MetadataConfigModel, FileDetailModel, FileProfileConfigModel>() {
            @Override
            public FileProfileConfigModel apply(MetadataConfigModel metadataConfigModel, FileDetailModel fileDetailModel) {

                FileProfileConfigModel configModel = new FileProfileConfigModel();
                configModel.setMetadataConfigModel(metadataConfigModel);
                configModel.setDetail(fileDetailModel);
                return configModel;
            }
        }).flatMap(new io.reactivex.functions.Function<FileProfileConfigModel, SingleSource<FileProfileConfigModel>>() {
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
                    Single<UserWrapperModel> userSingle = HttpIO.getCurrentInstance().execute(SDocService.class).getRelatedUsers(repoId);
                    singles.add(userSingle);

                    Single<FileRecordWrapperModel> recordSingle = HttpIO.getCurrentInstance().execute(SDocService.class).getRecords(repoId, parent_dir, name, name);
                    singles.add(recordSingle);
                }

                if (configModel.getTagsEnabled()) {
                    Single<FileTagWrapperModel> tagSingle = HttpIO.getCurrentInstance().execute(SDocService.class).getTags(repoId);
                    singles.add(tagSingle);
                }

                if (singles.isEmpty()) {
                    configModel.initDefaultIfMetaNotEnable();
                    return Single.just(configModel);
                }

                return Single.zip(singles, new io.reactivex.functions.Function<Object[], FileProfileConfigModel>() {
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
        }).delay(200, TimeUnit.MILLISECONDS);


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
