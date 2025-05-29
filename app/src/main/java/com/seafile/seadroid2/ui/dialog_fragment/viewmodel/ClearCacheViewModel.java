package com.seafile.seadroid2.ui.dialog_fragment.viewmodel;

import com.seafile.seadroid2.framework.glide.GlideApp;
import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.ui.base.viewmodel.BaseViewModel;
import com.seafile.seadroid2.framework.datastore.StorageManager;

import io.reactivex.Single;
import io.reactivex.SingleEmitter;
import io.reactivex.SingleOnSubscribe;
import io.reactivex.functions.Consumer;

public class ClearCacheViewModel extends BaseViewModel {

    public void clear(Consumer<Boolean> consumer) {
        Single<Boolean> s = Single.create(new SingleOnSubscribe<Boolean>() {
            @Override
            public void subscribe(SingleEmitter<Boolean> emitter) throws Exception {
                StorageManager storageManager = StorageManager.getInstance();
                storageManager.clearCache();

//                // clear cached data from database
//                DatabaseHelper dbHelper = DatabaseHelper.getDatabaseHelper();
//                dbHelper.delCaches();

                //clear Glide cache
                GlideApp.get(SeadroidApplication.getAppContext()).clearMemory();
                GlideApp.get(SeadroidApplication.getAppContext()).clearDiskCache();

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
