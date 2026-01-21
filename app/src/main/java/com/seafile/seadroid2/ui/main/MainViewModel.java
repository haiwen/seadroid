package com.seafile.seadroid2.ui.main;

import android.content.Context;
import android.net.Uri;
import android.text.TextUtils;

import androidx.fragment.app.Fragment;
import androidx.lifecycle.MutableLiveData;

import com.blankj.utilcode.util.CollectionUtils;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.enums.FeatureDataSource;
import com.seafile.seadroid2.enums.SaveTo;
import com.seafile.seadroid2.enums.TransferStatus;
import com.seafile.seadroid2.framework.db.AppDatabase;
import com.seafile.seadroid2.framework.db.entities.PermissionEntity;
import com.seafile.seadroid2.framework.db.entities.RepoModel;
import com.seafile.seadroid2.framework.http.HttpIO;
import com.seafile.seadroid2.framework.model.ServerInfo;
import com.seafile.seadroid2.framework.model.dirents.DirentFileModel;
import com.seafile.seadroid2.framework.model.repo.DirentWrapperModel;
import com.seafile.seadroid2.framework.model.server.ServerInfoModel;
import com.seafile.seadroid2.framework.util.FileUtils;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.framework.worker.ExistingFileStrategy;
import com.seafile.seadroid2.framework.worker.GlobalTransferCacheList;
import com.seafile.seadroid2.framework.worker.queue.TransferModel;
import com.seafile.seadroid2.ui.activities.AllActivitiesFragment;
import com.seafile.seadroid2.baseviewmodel.BaseViewModel;
import com.seafile.seadroid2.ui.file.FileService;
import com.seafile.seadroid2.ui.repo.RepoQuickFragment;
import com.seafile.seadroid2.ui.repo.RepoService;
import com.seafile.seadroid2.ui.settings.TabSettings2Fragment;
import com.seafile.seadroid2.ui.star.StarredQuickFragment;

import java.io.File;
import java.util.List;
import java.util.stream.Collectors;

import io.reactivex.Single;
import io.reactivex.functions.Consumer;

public class MainViewModel extends BaseViewModel {
    private final String TAG = "MainViewModel";
    //force refresh repo/dirents
    private final MutableLiveData<Boolean> _on_force_refresh_repo_list_live_data = new MutableLiveData<>();

    private final MutableLiveData<ServerInfo> _server_info_live_data = new MutableLiveData<>();


    public MutableLiveData<Boolean> getOnForceRefreshRepoListLiveData() {
        return _on_force_refresh_repo_list_live_data;
    }

    public MutableLiveData<ServerInfo> getServerInfoLiveData() {
        return _server_info_live_data;
    }

    private final List<Fragment> fragments = CollectionUtils.newUnmodifiableListNotNull(
            RepoQuickFragment.newInstance(),
            StarredQuickFragment.newInstance(),
            AllActivitiesFragment.newInstance(),
            TabSettings2Fragment.newInstance()
    );

    public List<Fragment> getFragments() {
        return fragments;
    }


    public void getServerInfo() {
        Single<ServerInfoModel> serverInfoSingle = HttpIO.getCurrentInstance().execute(MainService.class).getServerInfo();
        addSingleDisposable(serverInfoSingle, new Consumer<ServerInfoModel>() {
            @Override
            public void accept(ServerInfoModel serverInfo) throws Exception {
                Account account = SupportAccountManager.getInstance().getCurrentAccount();
                if (account == null) {
                    return;
                }

                ServerInfo serverInfo1 = new ServerInfo(account.server, serverInfo.version, serverInfo.getFeaturesString(), serverInfo.encrypted_library_version);
                SupportAccountManager.getInstance().setServerInfo(account, serverInfo1);

                getServerInfoLiveData().setValue(serverInfo1);
            }
        });
    }

    public void getPermissionFromLocal(String repoId, int pNum, Consumer<PermissionEntity> consumer) {
        Single<List<PermissionEntity>> pSingle = AppDatabase.getInstance().permissionDAO().getByRepoAndIdAsync(repoId, pNum);
        addSingleDisposable(pSingle, new Consumer<List<PermissionEntity>>() {
            @Override
            public void accept(List<PermissionEntity> permissionEntities) throws Exception {

                if (consumer != null) {
                    if (CollectionUtils.isEmpty(permissionEntities)) {
                        consumer.accept(null);
                    } else {
                        consumer.accept(permissionEntities.get(0));
                    }

                }
            }
        });
    }


    public void multipleCheckRemoteDirent(Context context, Account account, String repoId, String repoName, String parentDir, List<Uri> uriList, java.util.function.Consumer<Boolean> consumer) {
        if (CollectionUtils.isEmpty(uriList)) {
            if (consumer != null) {
                consumer.accept(false);
            }
            return;
        }

        String appCacheUriPrefix = "content://" + context.getPackageName() + ".documents";
        List<Uri> uris = uriList.stream().filter(f -> f != null && !f.toString().startsWith(appCacheUriPrefix)).collect(Collectors.toList());
        if (CollectionUtils.isEmpty(uris)) {
            SLogs.d(TAG, "multipleCheckRemoteDirent()", "uris is in the app cache directory, can't upload");
            if (consumer != null) {
                consumer.accept(false);
            }
            return;
        }

        Single<DirentWrapperModel> detailSingle = HttpIO.getCurrentInstance()
                .execute(RepoService.class)
                .getDirentsAsync(repoId, parentDir);
        addSingleDisposable(detailSingle, new Consumer<DirentWrapperModel>() {
            @Override
            public void accept(DirentWrapperModel wrapperModel) throws Exception {
                if (wrapperModel == null || CollectionUtils.isEmpty(wrapperModel.dirent_list)) {
                    SLogs.d(TAG, "multipleCheckRemoteDirent()", "request " + parentDir + " children result is null or empty.");

                    for (Uri uri : uris) {
                        String fileName = Utils.getFilenameFromUri(context, uri);
                        TransferModel transferModel = gen(context, account, repoId, repoName, uri, fileName, parentDir, false);
                        GlobalTransferCacheList.FILE_UPLOAD_QUEUE.put(transferModel);
                    }

                    if (consumer != null) {
                        consumer.accept(true);
                    }
                    return;
                }

                for (Uri uri : uris) {
                    String fileName = Utils.getFilenameFromUri(context, uri);
                    boolean isExists = wrapperModel.dirent_list.stream().anyMatch(f -> TextUtils.equals(f.name, fileName));
                    TransferModel transferModel = gen(context, account, repoId, repoName, uri, fileName, parentDir, isExists);
                    GlobalTransferCacheList.FILE_UPLOAD_QUEUE.put(transferModel);
                }

                SLogs.d(TAG, "multipleCheckRemoteDirent()", "can upload " + uris.size() + " files");

                if (consumer != null) {
                    consumer.accept(true);
                }
            }
        }, new Consumer<Throwable>() {
            @Override
            public void accept(Throwable throwable) throws Exception {
                if (consumer != null) {
                    consumer.accept(null);
                }
            }
        });
    }

    public void checkRemoteDirent(String repoId, String fullPath, java.util.function.Consumer<DirentFileModel> consumer) {
        Single<DirentFileModel> detailSingle = HttpIO.getCurrentInstance()
                .execute(FileService.class)
                .getFileDetail(repoId, fullPath);
        addSingleDisposable(detailSingle, new Consumer<DirentFileModel>() {
            @Override
            public void accept(DirentFileModel direntFileModel) throws Exception {
                if (consumer != null) {
                    consumer.accept(direntFileModel);
                }
            }
        }, new Consumer<Throwable>() {
            @Override
            public void accept(Throwable throwable) throws Exception {
                if (consumer != null) {
                    consumer.accept(null);
                }
            }
        });
    }

    public void checkRemoteDirent2(String repoId, String fullPath1, String fullPath2, java.util.function.Consumer<DirentFileModel> consumer) {
        Single<DirentFileModel> detailSingle1 = HttpIO.getCurrentInstance()
                .execute(FileService.class)
                .getFileDetail(repoId, fullPath1);

        Single<DirentFileModel> detailSingle2 = HttpIO.getCurrentInstance()
                .execute(FileService.class)
                .getFileDetail(repoId, fullPath2);

        addSingleDisposable(detailSingle, new Consumer<DirentFileModel>() {
            @Override
            public void accept(DirentFileModel direntFileModel) throws Exception {
                if (consumer != null) {
                    consumer.accept(direntFileModel);
                }
            }
        }, new Consumer<Throwable>() {
            @Override
            public void accept(Throwable throwable) throws Exception {
                if (consumer != null) {
                    consumer.accept(null);
                }
            }
        });
    }

    public void addUploadTask(Context context, Account account, RepoModel repoModel, Uri sourceUri, String parentDir, String fileName, boolean isReplace) {
        //sourceUri content://com.android.providers.media.documents/document/image:1000182224
        TransferModel transferModel = gen(context, account, repoModel.repo_id, repoModel.repo_name, sourceUri, fileName, parentDir, isReplace);
        GlobalTransferCacheList.FILE_UPLOAD_QUEUE.put(transferModel);
        SLogs.d(TAG, "addUploadTask()", "uri = " + sourceUri);
    }

    public void addUploadTask(Context context, Account account, RepoModel repoModel, String localFileAbsPath, String parentDir, boolean isReplace) {
        TransferModel transferModel = gen(context, account, repoModel.repo_id, repoModel.repo_name, localFileAbsPath, parentDir, isReplace);
        GlobalTransferCacheList.FILE_UPLOAD_QUEUE.put(transferModel);
        SLogs.d(TAG, "addUploadTask()", "localPath = " + localFileAbsPath);
    }

    private TransferModel gen(Context context, Account account, String repo_id, String repo_name, String fileAbsPath, String parentDir, boolean isReplace) {
        //content://com.android.providers.media.documents/document/image:1000182224
        File file = new File(fileAbsPath);
        TransferModel transferModel = gen(account, repo_id, repo_name, file.getName(), parentDir, isReplace);
        transferModel.full_path = fileAbsPath;
        transferModel.file_size = file.length();
        transferModel.setId(transferModel.genStableId());
        return transferModel;
    }


    private TransferModel gen(Context context, Account account, String repo_id, String repo_name, Uri sourceUri, String fileName, String parentDir, boolean isReplace) {
        //content://com.android.providers.media.documents/document/image:1000182224
        TransferModel transferModel = gen(account, repo_id, repo_name, fileName, parentDir, isReplace);
        transferModel.full_path = sourceUri.toString();
        transferModel.file_size = FileUtils.getEstimationFileSize(context, sourceUri);
        transferModel.setId(transferModel.genStableId());
        return transferModel;
    }

    private TransferModel gen(Account account, String repo_id, String repo_name, String fileName, String parentDir, boolean isReplace) {
        //content://com.android.providers.media.documents/document/image:1000182224
        TransferModel transferModel = new TransferModel();
        transferModel.save_to = SaveTo.NO_SAVE;
        transferModel.created_at = System.currentTimeMillis();
        transferModel.repo_id = repo_id;
        transferModel.repo_name = repo_name;
        transferModel.related_account = account.getSignature();
        transferModel.target_path = Utils.pathJoin(parentDir, fileName);
        transferModel.setParentPath(parentDir);
        transferModel.file_name = fileName;
        transferModel.data_source = FeatureDataSource.MANUAL_FILE_UPLOAD;
        transferModel.transfer_status = TransferStatus.WAITING;
        transferModel.transfer_strategy = isReplace ? ExistingFileStrategy.REPLACE : ExistingFileStrategy.KEEP;
        return transferModel;
    }

}
