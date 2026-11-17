package com.seafile.seadroid2.ui.settings;

import android.content.Context;
import android.text.TextUtils;

import androidx.lifecycle.MutableLiveData;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.AccountInfo;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.framework.http.HttpManager;
import com.seafile.seadroid2.framework.model.ServerInfo;
import com.seafile.seadroid2.framework.model.server.ServerInfoModel;
import com.seafile.seadroid2.framework.datastore.MediaDirMigrator;
import com.seafile.seadroid2.framework.datastore.StorageManager;
import com.seafile.seadroid2.framework.http.HttpIO;
import com.seafile.seadroid2.framework.util.SafeLogs;
import com.seafile.seadroid2.framework.util.Toasts;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.preferences.Settings;
import com.seafile.seadroid2.ui.account.AccountService;
import com.seafile.seadroid2.baseviewmodel.BaseViewModel;
import com.seafile.seadroid2.ui.main.MainService;

import org.apache.commons.io.FileUtils;

import java.io.File;

import io.reactivex.Single;
import io.reactivex.SingleEmitter;
import io.reactivex.SingleOnSubscribe;
import io.reactivex.functions.BiFunction;
import io.reactivex.functions.Consumer;

public class SettingsFragmentViewModel extends BaseViewModel {
    public void getAccountInfo() {
        getRefreshLiveData().setValue(true);

        Single<ServerInfoModel> serverInfoSingle = HttpManager.getCurrentHttp().execute(MainService.class).getServerInfo();
        Single<AccountInfo> accountInfoSingle = HttpManager.getCurrentHttp().execute(AccountService.class).getAccountInfo();

        Single<AccountInfo> single = Single.zip(serverInfoSingle, accountInfoSingle, new BiFunction<ServerInfoModel, AccountInfo, AccountInfo>() {
            @Override
            public AccountInfo apply(ServerInfoModel serverInfoModel, AccountInfo accountInfo) throws Exception {

                Account account = SupportAccountManager.getInstance().getCurrentAccount();
                if (account == null) {
                    return accountInfo;
                }

                accountInfo.setServer(HttpManager.getCurrentHttp().getCurrentServer());

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

    public void calculateCacheSize() {
        Single<String> single = Single.create(new SingleOnSubscribe<String>() {
            @Override
            public void subscribe(SingleEmitter<String> emitter) throws Exception {
                if (emitter == null || emitter.isDisposed()) {
                    return;
                }

                long l = StorageManager.getInstance().getUsedSpace();

                String total = FileUtils.byteCountToDisplaySize(l);
                emitter.onSuccess(total);
            }
        });
        addSingleDisposable(single, new Consumer<String>() {
            @Override
            public void accept(String s) throws Exception {
                Settings.CACHE_SIZE.putValue("");
                Settings.CACHE_SIZE.putValue(s);
            }
        });
    }

    private final MutableLiveData<Boolean> modifyStorageLocationLiveData = new MutableLiveData<>();

    public MutableLiveData<Boolean> getModifyStorageLocationLiveData() {
        return modifyStorageLocationLiveData;
    }

    /**
     * Move the downloaded files to the new location and use it.
     *
     * @param newStoragePath empty means the default location
     * @param moveFiles      false to switch without moving the files, only when the current custom dir is unavailable
     */
    public void modifyStorageLocation(Context context, String newStoragePath, boolean moveFiles) {
        StorageManager storageManager = StorageManager.getInstance();

        // the configured dir, not the fallback dir used while the custom dir is unavailable
        File curMediaDir = storageManager.getConfiguredMediaDir();
        String curPath = Utils.pathJoin(curMediaDir.getAbsolutePath(), "/");

        File newMediaDir;
        if (TextUtils.isEmpty(newStoragePath)) {
            newMediaDir = storageManager.getSelectedVolumeLocation().mediaPath;
        } else {
            newMediaDir = StorageManager.getCustomMediaDir(newStoragePath);
        }
        String newPath = Utils.pathJoin(newMediaDir.getAbsolutePath(), "/");

        if (TextUtils.equals(newPath, curPath)) {
            // nothing to move
            getModifyStorageLocationLiveData().setValue(true);
            return;
        }

        if (moveFiles && storageManager.isCustomMediaDirUnavailable()) {
            Toasts.show(context.getString(R.string.settings_download_folder_unavailable_restore, curMediaDir.getAbsolutePath()));
            return;
        }

        //can not move into itself
        if (newPath.startsWith(curPath) || curPath.startsWith(newPath)) {
            Toasts.show(R.string.settings_download_folder_invalid);
            return;
        }

        File newStorageFolder = newMediaDir.getParentFile();
        if (!com.seafile.seadroid2.framework.util.FileUtils.isAvailable(newStorageFolder)) {
            Toasts.show(context.getString(R.string.storage_manager_storage_description_not_available, newStorageFolder.getName()));
            return;
        }

        getSecondRefreshLiveData().setValue(true);

        Single<Boolean> s = Single.fromCallable(() -> {
            // waits for the running download to exit before moving the files
            MediaDirMigrator.changeMediaDir(newStoragePath, moveFiles);
            return true;
        });

        addSingleDisposable(s, new Consumer<Boolean>() {
            @Override
            public void accept(Boolean aBoolean) {
                Toasts.show(R.string.success);
                getModifyStorageLocationLiveData().setValue(true);
                getSecondRefreshLiveData().setValue(false);
            }
        }, new Consumer<Throwable>() {
            @Override
            public void accept(Throwable throwable) {
                getSecondRefreshLiveData().setValue(false);
                Toasts.show(R.string.failed);
                SafeLogs.e(throwable.getMessage());
            }
        });
    }
}
