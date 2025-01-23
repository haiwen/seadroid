package com.seafile.seadroid2.ui.media.player;

import android.text.TextUtils;

import androidx.lifecycle.MutableLiveData;

import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.framework.http.HttpIO;
import com.seafile.seadroid2.ui.base.viewmodel.BaseViewModel;
import com.seafile.seadroid2.ui.file.FileService;

import io.reactivex.Single;
import io.reactivex.functions.Consumer;

public class PlayerViewModel extends BaseViewModel {

    private final MutableLiveData<String> _urlLiveData = new MutableLiveData<>();

    public MutableLiveData<String> getUrlLiveData() {
        return _urlLiveData;
    }

    public void getFileLink(String repoId, String p, boolean isReUsed) {
        Single<String> urlSingle = HttpIO.getCurrentInstance().execute(FileService.class).getFileDownloadLinkAsync(repoId, p, isReUsed ? 1 : 0);
        addSingleDisposable(urlSingle, new Consumer<String>() {
            @Override
            public void accept(String dlink) {
                if (TextUtils.isEmpty(dlink) || "\"\"".equals(dlink)) {
                    getSeafExceptionLiveData().setValue(SeafException.UNKNOWN_EXCEPTION);
                    return;
                }

                getUrlLiveData().setValue(dlink);
            }
        }, new Consumer<Throwable>() {
            @Override
            public void accept(Throwable throwable) throws Exception {
                SeafException seafException = getExceptionByThrowable(throwable);
                getSeafExceptionLiveData().setValue(seafException);
            }
        });
    }
}
