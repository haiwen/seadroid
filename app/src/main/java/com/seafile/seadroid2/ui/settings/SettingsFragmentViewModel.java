package com.seafile.seadroid2.ui.settings;

import android.content.Context;
import android.text.TextUtils;

import androidx.lifecycle.MutableLiveData;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.AccountInfo;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.framework.datastore.sp.AppDataManager;
import com.seafile.seadroid2.framework.db.AppDatabase;
import com.seafile.seadroid2.framework.db.entities.FileCacheStatusEntity;
import com.seafile.seadroid2.framework.http.HttpManager;
import com.seafile.seadroid2.framework.model.ServerInfo;
import com.seafile.seadroid2.framework.model.server.ServerInfoModel;
import com.seafile.seadroid2.framework.datastore.StorageManager;
import com.seafile.seadroid2.framework.http.HttpIO;
import com.seafile.seadroid2.framework.service.BackupThreadExecutor;
import com.seafile.seadroid2.framework.util.SafeLogs;
import com.seafile.seadroid2.framework.util.Toasts;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.preferences.Settings;
import com.seafile.seadroid2.ui.account.AccountService;
import com.seafile.seadroid2.baseviewmodel.BaseViewModel;
import com.seafile.seadroid2.ui.main.MainService;

import org.apache.commons.io.FileUtils;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import io.reactivex.Single;
import io.reactivex.SingleEmitter;
import io.reactivex.SingleOnSubscribe;
import io.reactivex.SingleSource;
import io.reactivex.functions.BiFunction;
import io.reactivex.functions.Consumer;
import io.reactivex.functions.Function;

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
     * @param newStoragePath empty means the default location
     */
    public void modifyStorageLocation(Context context, String newStoragePath) {

        StorageManager.Location location = StorageManager.getInstance().getSelectedStorageLocation();
        String curPath = location.mediaPath.getAbsolutePath();
        curPath = Utils.pathJoin(curPath, "/");

        File newMediaDir;
        if (TextUtils.isEmpty(newStoragePath)) {
            newMediaDir = StorageManager.getInstance().getSelectedVolumeLocation().mediaPath;
        } else {
            newMediaDir = StorageManager.getCustomMediaDir(newStoragePath);
        }
        String newPath = Utils.pathJoin(newMediaDir.getAbsolutePath(), "/");

        if (TextUtils.equals(newPath, curPath)) {
            AppDataManager.writeCustomStorageDir(newStoragePath);
            StorageManager.resetInstance();
            getModifyStorageLocationLiveData().setValue(true);
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

        //stop download
        BackupThreadExecutor.getInstance().stopDownload();

        String finalNewStoragePath = newStoragePath;
        String finalCurPath = curPath;
        File finalNewMediaDir = newMediaDir;
        Single<Boolean> s = Single.create(new SingleOnSubscribe<Boolean>() {
            @Override
            public void subscribe(SingleEmitter<Boolean> emitter) throws Exception {

                //copy
                if (location.mediaPath.exists()) {
                    FileUtils.copyDirectory(location.mediaPath, finalNewMediaDir);
                }
                emitter.onSuccess(true);
            }
        }).flatMap(new Function<Boolean, SingleSource<Boolean>>() {
            @Override
            public SingleSource<Boolean> apply(Boolean aBoolean) throws Exception {

                //delete old
                StorageManager.getInstance().clearMedia();
                return Single.just(true);
            }
        }).flatMap(new Function<Boolean, SingleSource<? extends Boolean>>() {
            @Override
            public SingleSource<? extends Boolean> apply(Boolean aBoolean) throws Exception {
                return migrateCachePathInPages(finalCurPath, newPath);
            }
        }).flatMap(new Function<Boolean, SingleSource<Boolean>>() {
            @Override
            public SingleSource<Boolean> apply(Boolean aBoolean) throws Exception {
                AppDataManager.writeCustomStorageDir(finalNewStoragePath);
                StorageManager.resetInstance();
                return Single.just(true);
            }
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

    public Single<Boolean> migrateCachePathInPages(String oldPrefix, String newPrefix) {
        return Single.create(new SingleOnSubscribe<Boolean>() {
            @Override
            public void subscribe(SingleEmitter<Boolean> emitter) throws Exception {
                int pageSize = 500;
                int offset = 0;
                List<FileCacheStatusEntity> page;

                do {
                    page = AppDatabase.getInstance().fileCacheStatusDAO().getPaged(pageSize, offset);
                    List<FileCacheStatusEntity> toUpdate = new ArrayList<>();

                    for (FileCacheStatusEntity entity : page) {
                        if (entity.target_path.startsWith(oldPrefix)) {
                            String tmp = entity.target_path.substring(oldPrefix.length());
                            entity.target_path = Utils.pathJoin(newPrefix, tmp);
                            toUpdate.add(entity);
                        }
                    }

                    if (!toUpdate.isEmpty()) {
                        AppDatabase.getInstance().fileCacheStatusDAO().updateAll(toUpdate); // 批量更新
                    }

                    offset += pageSize;
                } while (page.size() == pageSize); // the last page will < pageSize

                emitter.onSuccess(true);
            }
        });
    }
}
