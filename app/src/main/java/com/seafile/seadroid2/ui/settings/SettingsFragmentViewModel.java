package com.seafile.seadroid2.ui.settings;

import android.content.Context;

import androidx.work.WorkInfo;

import com.blankj.utilcode.util.CollectionUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.AccountInfo;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.framework.data.ServerInfo;
import com.seafile.seadroid2.framework.data.db.AppDatabase;
import com.seafile.seadroid2.enums.TransferAction;
import com.seafile.seadroid2.enums.TransferDataSource;
import com.seafile.seadroid2.enums.TransferStatus;
import com.seafile.seadroid2.framework.data.model.server.ServerInfoModel;
import com.seafile.seadroid2.framework.datastore.StorageManager;
import com.seafile.seadroid2.framework.http.HttpIO;
import com.seafile.seadroid2.framework.worker.BackgroundJobManagerImpl;
import com.seafile.seadroid2.framework.worker.upload.UploadFolderFileAutomaticallyWorker;
import com.seafile.seadroid2.framework.worker.upload.UploadMediaFileAutomaticallyWorker;
import com.seafile.seadroid2.preferences.Settings;
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
                getRefreshLiveData().setValue(false);

                // fixme ?
                Settings.USER_INFO.putValue("");
                Settings.SPACE_INFO.putValue("");
                Settings.USER_SERVER_INFO.putValue("");
                Settings.USER_INFO.putValue(accountInfo.getName());
                Settings.USER_SERVER_INFO.putValue(accountInfo.getServer());
                Settings.SPACE_INFO.putValue(accountInfo.getSpaceUsed());
            }
        });
    }

    public void countFolderBackupPendingList(Context context) {
        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        if (account == null) {
            return;
        }

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
                    Settings.FOLDER_BACKUP_STATE.putValue(context.getString(R.string.folder_backup_waiting_state));
                } else {
                    WorkInfo workInfo = BackgroundJobManagerImpl.getInstance().getWorkInfoById(UploadFolderFileAutomaticallyWorker.UID);
                    if (workInfo != null && WorkInfo.State.ENQUEUED == workInfo.getState()) {
                        Settings.FOLDER_BACKUP_STATE.putValue(context.getString(R.string.waiting));
                    } else {
                        Settings.FOLDER_BACKUP_STATE.putValue(String.valueOf(s));
                    }
                }
            }
        });
    }

    public void countAlbumBackupPendingList(Context context) {
        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        if (account == null) {
            return;
        }

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
                    Settings.ALBUM_BACKUP_STATE.putValue(context.getString(R.string.settings_cuc_finish_title));
                } else {
                    WorkInfo workInfo = BackgroundJobManagerImpl.getInstance().getWorkInfoById(UploadMediaFileAutomaticallyWorker.UID);
                    if (workInfo != null && WorkInfo.State.ENQUEUED == workInfo.getState()) {
                        Settings.ALBUM_BACKUP_STATE.putValue(context.getString(R.string.waiting));
                    } else {
                        Settings.ALBUM_BACKUP_STATE.putValue(String.valueOf(s));
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
                Settings.CACHE_SIZE.putValue(s);
            }
        });

    }
}
