package com.seafile.seadroid2.ui.settings;

import android.content.Context;
import android.text.TextUtils;

import androidx.lifecycle.MutableLiveData;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.AccountInfo;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.bus.BusHelper;
import com.seafile.seadroid2.framework.datastore.sp.AppDataManager;
import com.seafile.seadroid2.framework.db.AppDatabase;
import com.seafile.seadroid2.framework.http.HttpManager;
import com.seafile.seadroid2.framework.model.ServerInfo;
import com.seafile.seadroid2.framework.model.server.ServerInfoModel;
import com.seafile.seadroid2.framework.datastore.StorageManager;
import com.seafile.seadroid2.framework.http.HttpIO;
import com.seafile.seadroid2.framework.service.BackupThreadExecutor;
import com.seafile.seadroid2.framework.util.SafeLogs;
import com.seafile.seadroid2.framework.util.Toasts;
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
     * Changes the cache directory.
     * <p>All transfers and file monitoring are stopped first. All offline cache records and
     * files in the old directory are deleted, then the selected directory becomes the new
     * media root.
     */
    public void modifyStorageLocation(Context context, String newStoragePath) {
        if (TextUtils.isEmpty(newStoragePath)) {
            return;
        }

        File newStorageFolder = new File(newStoragePath);
        if (!newStorageFolder.exists() || !newStorageFolder.isDirectory()) {
            Toasts.show(R.string.offline_root_dir_unavailable);
            return;
        }

        if (!newStorageFolder.canWrite()) {
            Toasts.show(R.string.offline_root_not_writable);
            return;
        }

        String currentPath = StorageManager.getInstance().getMediaDir().getAbsolutePath();
        if (TextUtils.equals(newStoragePath, currentPath)) {
            Toasts.show(R.string.offline_root_same_dir);
            return;
        }

        getSecondRefreshLiveData().setValue(true);

        // Stop all transfers
        BackupThreadExecutor.getInstance().stopAll();
        // Stop file monitoring
        BusHelper.getCommonObserver().post(BusHelper.ACTION_OFFLINE_ROOT_SWITCH_START);

        Single<Boolean> single = Single.create(new SingleOnSubscribe<Boolean>() {
            @Override
            public void subscribe(SingleEmitter<Boolean> emitter) {
                if (emitter == null || emitter.isDisposed()) {
                    return;
                }

                try {
                    // Delete all offline cache records
                    AppDatabase.getInstance().fileCacheStatusDAO().deleteAll();
                    // Delete all files in the old cache directory
                    StorageManager.getInstance().clearMedia();

                    AppDataManager.writeCustomStorageDir(newStoragePath);
                    StorageManager.resetInstance();
                    StorageManager.getInstance().getMediaDir().mkdirs();
                    emitter.onSuccess(true);
                } catch (Exception e) {
                    SafeLogs.e("modify storage location failed: " + e.getMessage());
                    emitter.onError(e);
                }
            }
        });

        addSingleDisposable(single, new Consumer<Boolean>() {
            @Override
            public void accept(Boolean aBoolean) {
                getSecondRefreshLiveData().setValue(false);
                BusHelper.getCommonObserver().post(BusHelper.ACTION_OFFLINE_ROOT_SWITCH_COMPLETE);
                Toasts.show(R.string.offline_root_switch_success);
                getModifyStorageLocationLiveData().setValue(true);
            }
        }, new Consumer<Throwable>() {
            @Override
            public void accept(Throwable throwable) {
                getSecondRefreshLiveData().setValue(false);
                BusHelper.getCommonObserver().post(BusHelper.ACTION_OFFLINE_ROOT_SWITCH_COMPLETE);
                Toasts.show(R.string.offline_root_switch_failed);
                SafeLogs.e(throwable.getMessage());
            }
        });
    }
}
