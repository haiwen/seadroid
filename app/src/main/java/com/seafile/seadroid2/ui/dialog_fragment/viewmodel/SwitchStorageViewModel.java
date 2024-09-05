package com.seafile.seadroid2.ui.dialog_fragment.viewmodel;

import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.framework.datastore.StorageManager;
import com.seafile.seadroid2.framework.datastore.sp_livedata.AlbumBackupSharePreferenceHelper;
import com.seafile.seadroid2.framework.datastore.sp_livedata.FolderBackupSharePreferenceHelper;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.worker.BackgroundJobManagerImpl;
import com.seafile.seadroid2.ui.base.viewmodel.BaseViewModel;
import com.seafile.seadroid2.ui.camera_upload.CameraUploadManager;

import io.reactivex.Single;
import io.reactivex.SingleEmitter;
import io.reactivex.SingleOnSubscribe;
import io.reactivex.functions.Consumer;

public class SwitchStorageViewModel extends BaseViewModel {

    public void switchStorage(StorageManager.Location location, Consumer<Boolean> consumer) {
        Single<Boolean> s = Single.create(new SingleOnSubscribe<Boolean>() {
            @Override
            public void subscribe(SingleEmitter<Boolean> emitter) throws Exception {
                if (location == null) {
                    SLogs.d("location is null: " + System.currentTimeMillis());
                    emitter.onSuccess(true);
                    return;
                }

                SLogs.d("Cancel all TransferService tasks");
                BackgroundJobManagerImpl.getInstance().cancelAllJobs();

                Account camAccount = CameraUploadManager.getInstance().getCameraAccount();
                if (CameraUploadManager.getInstance().isCameraUploadEnabled()) {
                    SLogs.d("Temporarily disable camera upload");
                    CameraUploadManager.getInstance().disableCameraUpload();
                }

                AlbumBackupSharePreferenceHelper.writeBackupSwitch(false);
                FolderBackupSharePreferenceHelper.writeBackupSwitch(false);

                SLogs.d("Switching storage to " + location.description);
                StorageManager.getInstance().setStorageDir(location.id);

                if (camAccount != null) {
                    SLogs.d("reEnable camera upload");
                    CameraUploadManager.getInstance().setCameraAccount(camAccount);
                }

                emitter.onSuccess(true);
            }
        });
        addSingleDisposable(s, new Consumer<Boolean>() {
            @Override
            public void accept(Boolean o) throws Exception {
                if (consumer != null) {
                    consumer.accept(o);
                }
            }
        }, new Consumer<Throwable>() {
            @Override
            public void accept(Throwable throwable) throws Exception {
                if (consumer != null) {
                    consumer.accept(false);
                }
            }
        });
    }
}
