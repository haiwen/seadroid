package com.seafile.seadroid2.ui.main;

import android.content.Context;
import android.net.Uri;
import android.text.TextUtils;
import android.webkit.MimeTypeMap;

import androidx.fragment.app.Fragment;
import androidx.lifecycle.MutableLiveData;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.FileUtils;
import com.blankj.utilcode.util.ToastUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.framework.data.db.entities.EncKeyCacheEntity;
import com.seafile.seadroid2.framework.data.db.entities.FileTransferEntity;
import com.seafile.seadroid2.framework.data.model.enums.TransferAction;
import com.seafile.seadroid2.framework.data.model.enums.TransferDataSource;
import com.seafile.seadroid2.framework.data.model.enums.TransferResult;
import com.seafile.seadroid2.framework.data.model.enums.TransferStatus;
import com.seafile.seadroid2.framework.data.model.repo.DirentWrapperModel;
import com.seafile.seadroid2.framework.datastore.DataManager;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.framework.worker.ExistingFileStrategy;
import com.seafile.seadroid2.ui.base.viewmodel.BaseViewModel;
import com.seafile.seadroid2.context.NavContext;
import com.seafile.seadroid2.framework.data.ServerInfo;
import com.seafile.seadroid2.framework.data.db.AppDatabase;
import com.seafile.seadroid2.framework.data.db.entities.DirentModel;
import com.seafile.seadroid2.framework.data.db.entities.RepoModel;
import com.seafile.seadroid2.framework.data.model.repo.RepoWrapperModel;
import com.seafile.seadroid2.framework.data.model.server.ServerInfoModel;
import com.seafile.seadroid2.ui.repo.RepoService;
import com.seafile.seadroid2.framework.http.IO;
import com.seafile.seadroid2.ui.activities.AllActivitiesFragment;
import com.seafile.seadroid2.ui.repo.RepoQuickFragment;
import com.seafile.seadroid2.ui.star.StarredQuickFragment;
import com.seafile.seadroid2.framework.util.FileTools;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.worker.BackgroundJobManagerImpl;

import org.apache.commons.io.IOUtils;

import java.io.File;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import io.reactivex.Single;
import io.reactivex.SingleEmitter;
import io.reactivex.SingleOnSubscribe;
import io.reactivex.functions.Consumer;

public class MainViewModel extends BaseViewModel {
    //    private final MutableLiveData<Pair<String, String>> OnNewFileDownloadLiveData = new MutableLiveData<>();
    private final MutableLiveData<Integer> OnResortListLiveData = new MutableLiveData<>();

    //force refresh repo/dirents
    private final MutableLiveData<Boolean> OnForceRefreshRepoListLiveData = new MutableLiveData<>();
    private final MutableLiveData<Boolean> OnForceRefreshStarredListLiveData = new MutableLiveData<>();

    //show swipeRefresh in Repo Fragment
    private final MutableLiveData<Boolean> OnShowRefreshLoadingInRepoLiveData = new MutableLiveData<>();


    private final MutableLiveData<Boolean> OnNavChangeListenerLiveData = new MutableLiveData<>();


    private final MutableLiveData<ServerInfo> ServerInfoLiveData = new MutableLiveData<>();

//    public MutableLiveData<Pair<String, String>> getOnNewFileDownloadLiveData() {
//        return OnNewFileDownloadLiveData;
//    }

    public MutableLiveData<Boolean> getOnForceRefreshRepoListLiveData() {
        return OnForceRefreshRepoListLiveData;
    }

    public MutableLiveData<Boolean> getOnShowRefreshLoadingInRepoLiveData() {
        return OnShowRefreshLoadingInRepoLiveData;
    }

    public MutableLiveData<Boolean> getOnNavContextChangeListenerLiveData() {
        return OnNavChangeListenerLiveData;
    }

    public MutableLiveData<Boolean> getOnForceRefreshStarredListLiveData() {
        return OnForceRefreshStarredListLiveData;
    }

    public MutableLiveData<ServerInfo> getServerInfoLiveData() {
        return ServerInfoLiveData;
    }

    public MutableLiveData<Integer> getOnResortListLiveData() {
        return OnResortListLiveData;
    }

    private NavContext navContext = null;

    public NavContext getNavContext() {
        if (navContext == null) {
            navContext = new NavContext();
        }
        return navContext;
    }

    private final List<Fragment> fragments = new ArrayList<>();

    public List<Fragment> getFragments() {
        return fragments;
    }

    public MainViewModel() {
        getNavContext();

        fragments.add(RepoQuickFragment.newInstance());
        fragments.add(StarredQuickFragment.newInstance());
        fragments.add(AllActivitiesFragment.newInstance());
    }

    public void getServerInfo() {
        Single<ServerInfoModel> single = IO.getInstanceWithLoggedIn().execute(MainService.class).getServerInfo();
        addSingleDisposable(single, new Consumer<ServerInfoModel>() {
            @Override
            public void accept(ServerInfoModel serverInfo) throws Exception {
                Account account = SupportAccountManager.getInstance().getCurrentAccount();
                if (account == null) {
                    return;
                }

                ServerInfo serverInfo1 = new ServerInfo(account.server, serverInfo.version, serverInfo.getFeaturesString());
                SupportAccountManager.getInstance().setServerInfo(account, serverInfo1);

                getServerInfoLiveData().setValue(serverInfo1);
            }
        });
    }

    public void requestRepoModel(String repoId, Consumer<RepoModel> consumer) {
        getOnShowRefreshLoadingInRepoLiveData().setValue(true);

        //from db
        Single<List<RepoModel>> singleDb = AppDatabase.getInstance().repoDao().getRepoById(repoId);
        addSingleDisposable(singleDb, new Consumer<List<RepoModel>>() {
            @Override
            public void accept(List<RepoModel> repoModels) throws Exception {
                if (consumer != null) {
                    if (CollectionUtils.isEmpty(repoModels)) {
                        //no data in sqlite, request RepoApi again
                        requestRepoModelFromServer(repoId, consumer);
                    } else {
                        consumer.accept(repoModels.get(0));
                        getOnShowRefreshLoadingInRepoLiveData().setValue(false);
                    }
                } else {
                    getOnShowRefreshLoadingInRepoLiveData().setValue(false);
                }
            }
        }, new Consumer<Throwable>() {
            @Override
            public void accept(Throwable throwable) throws Exception {
                getOnShowRefreshLoadingInRepoLiveData().setValue(false);
                SLogs.e(throwable);
            }
        });
    }

    private void requestRepoModelFromServer(String repoId, Consumer<RepoModel> consumer) {
        //from net
        Single<RepoWrapperModel> singleNet = IO.getInstanceWithLoggedIn().execute(RepoService.class).getRepos();
        addSingleDisposable(singleNet, new Consumer<RepoWrapperModel>() {
            @Override
            public void accept(RepoWrapperModel repoWrapperModel) throws Exception {
                getOnShowRefreshLoadingInRepoLiveData().setValue(false);

                if (repoWrapperModel == null || CollectionUtils.isEmpty(repoWrapperModel.repos)) {
                    ToastUtils.showLong(R.string.search_library_not_found);
                    return;
                }

                Optional<RepoModel> optionalRepoModel = repoWrapperModel.repos
                        .stream()
                        .filter(f -> TextUtils.equals(f.repo_id, repoId))
                        .findFirst();
                if (optionalRepoModel.isPresent()) {
                    if (consumer != null) {
                        consumer.accept(optionalRepoModel.get());
                    }
                } else {
                    ToastUtils.showLong(R.string.search_library_not_found);
                }
            }
        }, new Consumer<Throwable>() {
            @Override
            public void accept(Throwable throwable) throws Exception {
                getOnShowRefreshLoadingInRepoLiveData().setValue(false);
                String msg = getErrorMsgByThrowable(throwable);
                ToastUtils.showLong(msg);
            }
        });
    }

    public void getEncCacheDB(String repoId, Consumer<EncKeyCacheEntity> consumer) {
        Single<List<EncKeyCacheEntity>> single = AppDatabase.getInstance().encKeyCacheDAO().getListByRepoIdAsync(repoId);
        addSingleDisposable(single, new Consumer<List<EncKeyCacheEntity>>() {
            @Override
            public void accept(List<EncKeyCacheEntity> list) throws Exception {
                if (CollectionUtils.isEmpty(list)) {
                    consumer.accept(null);
                } else {
                    consumer.accept(list.get(0));
                }
            }
        });
    }


    public void getDirentsFromServer(String repoId, String parent_dir, Consumer<List<DirentModel>> consumer) {
        Single<DirentWrapperModel> singleServer = IO.getInstanceWithLoggedIn().execute(RepoService.class).getDirents(repoId, parent_dir);
        addSingleDisposable(singleServer, new Consumer<DirentWrapperModel>() {
            @Override
            public void accept(DirentWrapperModel direntWrapperModel) throws Exception {
                if (consumer != null) {
                    consumer.accept(direntWrapperModel.dirent_list);
                }
            }
        });
    }

    private Single<File> copyFile(Account account, Context context, Uri o, String repo_id, String repo_name, boolean isUpdate) {
        return Single.create(new SingleOnSubscribe<File>() {
            @Override
            public void subscribe(SingleEmitter<File> emitter) throws Exception {
                String fileName = Utils.getFilenameFromUri(context, o);
                String p = getNavContext().getNavPath() + fileName;

                File destinationFile = DataManager.getLocalRepoFile(account, repo_id, repo_name, p);
                if (destinationFile.exists() && isUpdate) {
                    destinationFile.delete();
                }

                try (InputStream in = context.getContentResolver().openInputStream(o);
                     OutputStream out = Files.newOutputStream(destinationFile.toPath())) {
                    IOUtils.copy(in, out);
                }

                emitter.onSuccess(destinationFile);
            }
        });
    }

    public void addUploadTask(Account account, Context context, RepoModel repoModel, String targetDir, Uri sourceUri, boolean isUpdate, Consumer<FileTransferEntity> consumer) {
        Single<File> single = copyFile(account, context, sourceUri, repoModel.repo_id, repoModel.repo_name, isUpdate);
        addSingleDisposable(single, new Consumer<File>() {
            @Override
            public void accept(File file) throws Exception {
                addUploadTask(account, repoModel, targetDir, file.getAbsolutePath(), isUpdate, consumer);
            }
        });
    }

    public void addUploadTask(Account account, RepoModel repoModel, String targetDir, String localFilePath, boolean isReplace, Consumer<FileTransferEntity> consumer) {
        Single<FileTransferEntity> single = Single.create(new SingleOnSubscribe<FileTransferEntity>() {
            @Override
            public void subscribe(SingleEmitter<FileTransferEntity> emitter) throws Exception {
                FileTransferEntity entity = new FileTransferEntity();

                File file = new File(localFilePath);
                if (!file.exists()) {
                    emitter.onSuccess(entity);
                    return;
                }

                entity.full_path = file.getAbsolutePath();
                entity.target_path = Utils.pathJoin(targetDir, file.getName());
                entity.setParent_path(targetDir);

                entity.file_name = file.getName();
                entity.file_size = file.length();
                entity.file_format = FileUtils.getFileExtension(entity.full_path);
                entity.file_md5 = FileUtils.getFileMD5ToString(entity.full_path).toLowerCase();
                entity.mime_type = MimeTypeMap.getSingleton().getMimeTypeFromExtension(entity.file_format);

//                entity.is_block = repoModel.canLocalDecrypt();

                entity.repo_id = repoModel.repo_id;
                entity.repo_name = repoModel.repo_name;
                entity.related_account = account.getSignature();
                entity.data_source = TransferDataSource.FILE_BACKUP;
                entity.created_at = System.currentTimeMillis();
                entity.modified_at = entity.created_at;
                entity.file_original_modified_at = file.lastModified();
                entity.action_end_at = 0;
                entity.file_strategy = isReplace ? ExistingFileStrategy.REPLACE : ExistingFileStrategy.KEEP;
                entity.is_copy_to_local = false;
                entity.transfer_action = TransferAction.UPLOAD;
                entity.transfer_result = TransferResult.NO_RESULT;
                entity.transfer_status = TransferStatus.WAITING;

                entity.uid = entity.getUID();

                AppDatabase.getInstance().fileTransferDAO().insert(entity);

                emitter.onSuccess(entity);

            }
        });

        addSingleDisposable(single, new Consumer<FileTransferEntity>() {
            @Override
            public void accept(FileTransferEntity transferEntity) throws Exception {

                if (TextUtils.isEmpty(transferEntity.uid)) {
                    return;
                }

                //start worker
                BackgroundJobManagerImpl.getInstance().startFileUploadWorker();

                if (consumer != null) {
                    consumer.accept(transferEntity);
                }
            }
        }, new Consumer<Throwable>() {
            @Override
            public void accept(Throwable throwable) throws Exception {
                throwable.printStackTrace();
            }
        });
    }
}
