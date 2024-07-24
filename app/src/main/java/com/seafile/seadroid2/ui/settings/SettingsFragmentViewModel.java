package com.seafile.seadroid2.ui.settings;

import android.content.Context;

import androidx.lifecycle.MutableLiveData;
import androidx.work.WorkInfo;

import com.blankj.utilcode.util.CollectionUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.AccountInfo;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.framework.data.ServerInfo;
import com.seafile.seadroid2.framework.data.db.AppDatabase;
import com.seafile.seadroid2.framework.data.model.enums.TransferAction;
import com.seafile.seadroid2.framework.data.model.enums.TransferDataSource;
import com.seafile.seadroid2.framework.data.model.enums.TransferStatus;
import com.seafile.seadroid2.framework.data.model.server.ServerInfoModel;
import com.seafile.seadroid2.framework.datastore.StorageManager;
import com.seafile.seadroid2.framework.http.HttpIO;
import com.seafile.seadroid2.framework.worker.BackgroundJobManagerImpl;
import com.seafile.seadroid2.framework.worker.upload.UploadFolderFileAutomaticallyWorker;
import com.seafile.seadroid2.framework.worker.upload.UploadMediaFileAutomaticallyWorker;
import com.seafile.seadroid2.ui.account.AccountService;
import com.seafile.seadroid2.ui.base.viewmodel.BaseViewModel;
import com.seafile.seadroid2.ui.main.MainService;

import org.apache.commons.io.FileUtils;

import io.reactivex.Single;
import io.reactivex.SingleEmitter;
import io.reactivex.SingleOnSubscribe;
import io.reactivex.functions.BiFunction;
import io.reactivex.functions.Consumer;

public class SettingsFragmentViewModel extends BaseViewModel {
    private final MutableLiveData<AccountInfo> accountInfoLiveData = new MutableLiveData<>();
    private final MutableLiveData<String> cacheSizeLiveData = new MutableLiveData<>();
    private final MutableLiveData<String> folderBackupStateLiveData = new MutableLiveData<>();
    private final MutableLiveData<String> albumBackupStateLiveData = new MutableLiveData<>();

    public MutableLiveData<String> getFolderBackupStateLiveData() {
        return folderBackupStateLiveData;
    }

    public MutableLiveData<String> getAlbumBackupStateLiveData() {
        return albumBackupStateLiveData;
    }

    public MutableLiveData<String> getCacheSizeLiveData() {
        return cacheSizeLiveData;
    }

    public MutableLiveData<AccountInfo> getAccountInfoLiveData() {
        return accountInfoLiveData;
    }

    public void getAccountInfo() {
        getRefreshLiveData().setValue(true);

        Single<ServerInfoModel> single1 = HttpIO.getCurrentInstance().execute(MainService.class).getServerInfo();
        Single<AccountInfo> single2 = HttpIO.getCurrentInstance().execute(AccountService.class).getAccountInfo();

        Single<AccountInfo> single = Single.zip(single1, single2, new BiFunction<ServerInfoModel, AccountInfo, AccountInfo>() {
            @Override
            public AccountInfo apply(ServerInfoModel serverInfoModel, AccountInfo accountInfo) throws Exception {

                Account account = SupportAccountManager.getInstance().getCurrentAccount();
                if (account == null) {
                    return accountInfo;
                }

                accountInfo.setServer(HttpIO.getCurrentInstance().getServerUrl());

                ServerInfo serverInfo1 = new ServerInfo(account.server, serverInfoModel.version, serverInfoModel.getFeaturesString(), serverInfoModel.encrypted_library_version);
                SupportAccountManager.getInstance().setServerInfo(account, serverInfo1);
                return accountInfo;
            }
        });

        addSingleDisposable(single, new Consumer<AccountInfo>() {
            @Override
            public void accept(AccountInfo accountInfo) throws Exception {
                accountInfo.setServer(HttpIO.getCurrentInstance().getServerUrl());


                getRefreshLiveData().setValue(false);

                getAccountInfoLiveData().setValue(accountInfo);
            }
        });
    }

    public void countFolderBackupState(Context context) {
        Account account = SupportAccountManager.getInstance().getCurrentAccount();


        Single<Integer> folderBackupInProgressCountSingle = AppDatabase
                .getInstance()
                .fileTransferDAO()
                .getCount(account.getSignature(),
                        TransferAction.UPLOAD,
                        TransferDataSource.FOLDER_BACKUP,
                        CollectionUtils.newArrayList(TransferStatus.IN_PROGRESS, TransferStatus.WAITING));
        addSingleDisposable(folderBackupInProgressCountSingle, new Consumer<Integer>() {
            @Override
            public void accept(Integer s) throws Exception {
                if (s == 0) {
                    folderBackupStateLiveData.setValue(context.getString(R.string.folder_backup_waiting_state));
                } else {

                    WorkInfo workInfo = BackgroundJobManagerImpl.getInstance().getWorkInfoById(UploadFolderFileAutomaticallyWorker.UID);
                    if (workInfo != null && WorkInfo.State.ENQUEUED == workInfo.getState()) {
                        folderBackupStateLiveData.setValue(context.getString(R.string.waiting));
                    } else {
                        folderBackupStateLiveData.setValue(String.valueOf(s));
                    }
                }
            }
        });
    }

    public void countAlbumBackupState(Context context) {
        Account account = SupportAccountManager.getInstance().getCurrentAccount();

        Single<Integer> folderBackupInProgressCountSingle = AppDatabase
                .getInstance()
                .fileTransferDAO()
                .getCount(account.getSignature(),
                        TransferAction.UPLOAD,
                        TransferDataSource.ALBUM_BACKUP,
                        CollectionUtils.newArrayList(TransferStatus.IN_PROGRESS, TransferStatus.WAITING));

        addSingleDisposable(folderBackupInProgressCountSingle, new Consumer<Integer>() {
            @Override
            public void accept(Integer s) {
                if (s == 0) {
                    albumBackupStateLiveData.setValue(context.getString(R.string.settings_cuc_finish_title));
                } else {
                    WorkInfo workInfo = BackgroundJobManagerImpl.getInstance().getWorkInfoById(UploadMediaFileAutomaticallyWorker.UID);
                    if (workInfo != null && WorkInfo.State.ENQUEUED == workInfo.getState()) {
                        albumBackupStateLiveData.setValue(context.getString(R.string.waiting));
                    } else {
                        albumBackupStateLiveData.setValue(String.valueOf(s));
                    }

                }
            }
        });
    }

    public void calculateCacheSize() {
        Single<String> single = Single.create(new SingleOnSubscribe<String>() {
            @Override
            public void subscribe(SingleEmitter<String> emitter) throws Exception {
                long l = StorageManager.getInstance().getUsedSpace();

                String total = FileUtils.byteCountToDisplaySize(l);
                emitter.onSuccess(total);
            }
        });
        addSingleDisposable(single, new Consumer<String>() {
            @Override
            public void accept(String s) throws Exception {
                getCacheSizeLiveData().setValue(s);
            }
        });

    }
}
