package com.seafile.seadroid2.ui.settings;

import android.content.Context;
import android.text.TextUtils;

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
import com.seafile.seadroid2.framework.worker.BackgroundJobManagerImpl;
import com.seafile.seadroid2.framework.worker.FolderBackupScannerWorker;
import com.seafile.seadroid2.framework.worker.SupportWorkManager;
import com.seafile.seadroid2.framework.worker.UploadFolderFileAutomaticallyWorker;
import com.seafile.seadroid2.framework.worker.UploadMediaFileAutomaticallyWorker;
import com.seafile.seadroid2.ui.main.MainService;
import com.seafile.seadroid2.ui.base.viewmodel.BaseViewModel;
import com.seafile.seadroid2.framework.datastore.StorageManager;
import com.seafile.seadroid2.ui.account.AccountService;
import com.seafile.seadroid2.framework.http.IO;

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

        Single<ServerInfoModel> single1 = IO.getInstanceWithLoggedIn().execute(MainService.class).getServerInfo();
        Single<AccountInfo> single2 = IO.getInstanceWithLoggedIn().execute(AccountService.class).getAccountInfo();

        Single<AccountInfo> single = Single.zip(single1, single2, new BiFunction<ServerInfoModel, AccountInfo, AccountInfo>() {
            @Override
            public AccountInfo apply(ServerInfoModel serverInfoModel, AccountInfo accountInfo) throws Exception {

                Account account = SupportAccountManager.getInstance().getCurrentAccount();
                if (account == null) {
                    return accountInfo;
                }

                accountInfo.setServer(IO.getInstanceWithLoggedIn().getServerUrl());

                ServerInfo serverInfo1 = new ServerInfo(account.server, serverInfoModel.version, serverInfoModel.getFeaturesString());
                SupportAccountManager.getInstance().setServerInfo(account, serverInfo1);
                return accountInfo;
            }
        });

        addSingleDisposable(single, new Consumer<AccountInfo>() {
            @Override
            public void accept(AccountInfo accountInfo) throws Exception {
                accountInfo.setServer(IO.getInstanceWithLoggedIn().getServerUrl());


                getRefreshLiveData().setValue(false);

                getAccountInfoLiveData().setValue(accountInfo);
            }
        });
    }

    public void countFolderBackupState(Context context) {
        countFolderBackupState(context, null);
    }

    public void countFolderBackupState(Context context, String fileName) {
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
                        if (TextUtils.isEmpty(fileName)) {
                            folderBackupStateLiveData.setValue(String.valueOf(s));
                        } else {
                            folderBackupStateLiveData.setValue("(" + s + ") " + fileName);
                        }
                    }
                }
            }
        });
    }

    public void countAlbumBackupState(Context context) {
        countAlbumBackupState(context, null);
    }

    public void countAlbumBackupState(Context context, String fileName) {
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
            public void accept(Integer s) throws Exception {
                if (s == 0) {
                    albumBackupStateLiveData.setValue(context.getString(R.string.settings_cuc_finish_title));
                } else {
                    WorkInfo workInfo = BackgroundJobManagerImpl.getInstance().getWorkInfoById(UploadMediaFileAutomaticallyWorker.UID);
                    if (workInfo != null && WorkInfo.State.ENQUEUED == workInfo.getState()) {
                        albumBackupStateLiveData.setValue(context.getString(R.string.waiting));
                    } else {
                        if (TextUtils.isEmpty(fileName)) {
                            albumBackupStateLiveData.setValue(String.valueOf(s));
                        } else {
                            albumBackupStateLiveData.setValue("(" + s + ") " + fileName);
                        }
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
