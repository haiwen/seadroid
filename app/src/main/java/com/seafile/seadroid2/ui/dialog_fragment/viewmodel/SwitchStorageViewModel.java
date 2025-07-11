package com.seafile.seadroid2.ui.dialog_fragment.viewmodel;

import android.content.Context;

import androidx.lifecycle.MutableLiveData;

import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.enums.FeatureDataSource;
import com.seafile.seadroid2.framework.datastore.DataManager;
import com.seafile.seadroid2.framework.datastore.StorageManager;
import com.seafile.seadroid2.framework.datastore.sp.AppDataManager;
import com.seafile.seadroid2.framework.db.AppDatabase;
import com.seafile.seadroid2.framework.db.entities.FileCacheStatusEntity;
import com.seafile.seadroid2.framework.service.TransferService;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.ui.base.viewmodel.BaseViewModel;

import org.apache.commons.io.FileUtils;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CompletableFuture;

import io.reactivex.Single;
import io.reactivex.SingleEmitter;
import io.reactivex.SingleOnSubscribe;
import io.reactivex.functions.Consumer;

public class SwitchStorageViewModel extends BaseViewModel {
    private final MutableLiveData<String> actionLiveData = new MutableLiveData<>();

    public MutableLiveData<String> getActionLiveData() {
        return actionLiveData;
    }

    public void switchStorage(Context context, StorageManager.Location newLocation) {
        if (newLocation == null || !newLocation.available) {
            SLogs.d("location is null");
            getActionLiveData().setValue("success");
            return;
        }

        StorageManager.Location currentLocation = StorageManager.getInstance().getSelectedStorageLocation();
        if (currentLocation.id == newLocation.id) {
            SLogs.d("location is same", newLocation.label);
            getActionLiveData().setValue("success");
            return;
        }

        getRefreshLiveData().setValue(true);

        //stop download
        CompletableFuture<Void> future = TransferService.getActiveTasks().getOrDefault(FeatureDataSource.DOWNLOAD, null);
        if (future != null && !future.isDone()) {
            TransferService.stopDownloadService(context);
        }

        Single<Boolean> s = Single.create(new SingleOnSubscribe<Boolean>() {
            @Override
            public void subscribe(SingleEmitter<Boolean> emitter) {
                if (emitter.isDisposed()) {
                    return;
                }

                // /storage/67DA-5855/Android/media/com.seafile.seadroid2.debug
                File newMediaDir = newLocation.mediaPath;

                try {
                    // move cached files from old location to new location (might take a while)
                    List<Account> list = SupportAccountManager.getInstance().getAccountList();
                    for (Account account : list) {

                        // /storage/emulated/0/Android/media/com.seafile.seadroid2.debug/Seafile/f4f550ea33e14f82aab7da71be0d13fa@auth.local (cloud.seafile.com)
                        String specialAccountCurrentMediaDir = DataManager.getAccountMediaDir(account);
                        File oldAccountDir = new File(specialAccountCurrentMediaDir);
                        if (!oldAccountDir.exists()) {
                            SLogs.d("oldAccountDir not exist");
                            continue;
                        }

                        String newDir = Utils.pathJoin(newMediaDir.getAbsolutePath(), "Seafile");
                        File newAccountDir = new File(newDir);

                        if (!oldAccountDir.isFile()) {
                            FileUtils.copyDirectoryToDirectory(oldAccountDir, newAccountDir);
                        }
                    }

                    //
                    StorageManager.getInstance().clearCache();

                    String oldPrefix = currentLocation.volume;
                    String newPrefix = newLocation.volume;
                    migrateCachePathInPages(oldPrefix, newPrefix);

                    SLogs.d("Setting storage directory to " + newMediaDir);
                    AppDataManager.writeStorageDirId(newLocation.id);

                } catch (Exception e) {
                    SLogs.e("Could not move cache to new location", e.getLocalizedMessage());
                    emitter.onError(e);
                    return;
                }

                emitter.onSuccess(true);
            }
        });
        addSingleDisposable(s, new Consumer<Boolean>() {
            @Override
            public void accept(Boolean o) throws Exception {
                getRefreshLiveData().setValue(false);
                getActionLiveData().setValue("success");
            }
        }, new Consumer<Throwable>() {
            @Override
            public void accept(Throwable throwable) throws Exception {
                getRefreshLiveData().setValue(false);
            }
        });
    }

    public void migrateCachePathInPages(String oldPrefix, String newPrefix) {
        int pageSize = 500;
        int offset = 0;
        List<FileCacheStatusEntity> page;

        do {
            page = AppDatabase.getInstance().fileCacheStatusDAO().getPaged(pageSize, offset);
            List<FileCacheStatusEntity> toUpdate = new ArrayList<>();

            for (FileCacheStatusEntity entity : page) {
                if (entity.target_path.startsWith(oldPrefix)) {
                    entity.target_path = entity.target_path.replaceFirst(oldPrefix, newPrefix);
                    toUpdate.add(entity);
                }
            }

            if (!toUpdate.isEmpty()) {
                AppDatabase.getInstance().fileCacheStatusDAO().updateAll(toUpdate); // 批量更新
            }

            offset += pageSize;
        } while (page.size() == pageSize); // the last page will < pageSize
    }


}
