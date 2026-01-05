package com.seafile.seadroid2.ui.settings;

import android.content.Context;
import android.text.TextUtils;

import androidx.lifecycle.MutableLiveData;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.AccountInfo;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.annotation.NotSupport;
import com.seafile.seadroid2.annotation.Unstable;
import com.seafile.seadroid2.framework.datastore.sp.AppDataManager;
import com.seafile.seadroid2.framework.db.AppDatabase;
import com.seafile.seadroid2.framework.db.entities.FileCacheStatusEntity;
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

        Single<ServerInfoModel> serverInfoSingle = HttpIO.getCurrentInstance().execute(MainService.class).getServerInfo();
        Single<AccountInfo> accountInfoSingle = HttpIO.getCurrentInstance().execute(AccountService.class).getAccountInfo();

        Single<AccountInfo> single = Single.zip(serverInfoSingle, accountInfoSingle, new BiFunction<ServerInfoModel, AccountInfo, AccountInfo>() {
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

    @NotSupport
    @Unstable
    public void modifyStorageLocation(Context context, String newStoragePath) {

        if (TextUtils.isEmpty(newStoragePath)) {
            return;
        }

        StorageManager.Location location = StorageManager.getInstance().getSelectedStorageLocation();
        String curPath = location.mediaPath.getAbsolutePath();
        curPath = Utils.pathJoin(curPath, "/");
        if (TextUtils.equals(newStoragePath, curPath)) {
            return;
        }
        
        File oldStorageFolder = new File(curPath);
        if (!oldStorageFolder.exists()) {
            Toasts.show(context.getString(R.string.storage_manager_storage_description_not_available, oldStorageFolder.getName()));
            return;
        }

        File newStorageFolder = new File(newStoragePath);
        if (!com.seafile.seadroid2.framework.util.FileUtils.isAvailable(newStorageFolder)) {
            Toasts.show(context.getString(R.string.storage_manager_storage_description_not_available, newStorageFolder.getName()));
            return;
        }

        if (!com.blankj.utilcode.util.FileUtils.isDir(newStoragePath)) {
            return;
        }

        getSecondRefreshLiveData().setValue(true);

        //stop download
        BackupThreadExecutor.getInstance().stopDownload();

        String finalNewStoragePath = newStoragePath;
        String finalCurPath = curPath;
        Single<Boolean> s = Single.create(new SingleOnSubscribe<Boolean>() {
            @Override
            public void subscribe(SingleEmitter<Boolean> emitter) throws Exception {

                //copy
                FileUtils.copyDirectoryToDirectory(location.mediaPath, new File(finalNewStoragePath));
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
                return migrateCachePathInPages(finalCurPath, finalNewStoragePath);
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
