package com.seafile.seadroid2.ui.dialog_fragment.viewmodel;

import android.text.TextUtils;

import androidx.lifecycle.MutableLiveData;

import com.blankj.utilcode.util.ToastUtils;
import com.seafile.seadroid2.ui.base.viewmodel.BaseViewModel;
import com.seafile.seadroid2.data.model.dirents.DeleteDirentModel;
import com.seafile.seadroid2.data.db.entities.DirentModel;
import com.seafile.seadroid2.io.http.IO;
import com.seafile.seadroid2.ui.dialog_fragment.DialogService;
import com.seafile.seadroid2.util.SLogs;

import java.util.ArrayList;
import java.util.List;

import io.reactivex.Observable;
import io.reactivex.Observer;
import io.reactivex.android.schedulers.AndroidSchedulers;
import io.reactivex.disposables.Disposable;
import io.reactivex.schedulers.Schedulers;

public class DeleteDirentsViewModel extends BaseViewModel {
    private final MutableLiveData<Boolean> ActionLiveData = new MutableLiveData<>();

    public MutableLiveData<Boolean> getActionLiveData() {
        return ActionLiveData;
    }

    private Disposable disposable;

    public void cancelAllTask() {
        if (disposable != null) {
            disposable.dispose();
        }
    }

    public void deleteDirents(List<DirentModel> dirents) {
        //cancel last task
        if (disposable != null && !disposable.isDisposed()) {
            disposable.dispose();
        }

        getRefreshLiveData().setValue(true);

        List<Observable<DeleteDirentModel>> singleList = new ArrayList<>();
        for (DirentModel dirent : dirents) {
            if (dirent.isDir()) {
                Observable<DeleteDirentModel> dirSingle = IO.getSingleton().execute(DialogService.class).deleteDir(dirent.repo_id, dirent.full_path);
                singleList.add(dirSingle);
            } else {
                Observable<DeleteDirentModel> fileSingle = IO.getSingleton().execute(DialogService.class).deleteFile(dirent.repo_id, dirent.full_path);
                singleList.add(fileSingle);
            }
        }

        Observable.concat(singleList)
                .onErrorResumeNext(throwable -> {
                    String string = getErrorMsgByThrowable(throwable);
                    DeleteDirentModel d = new DeleteDirentModel();
                    d.error_msg = string;
                    return Observable.just(d);
                })
                .subscribeOn(Schedulers.io())
                .observeOn(AndroidSchedulers.mainThread())
                .subscribe(new Observer<DeleteDirentModel>() {
                    @Override
                    public void onSubscribe(Disposable d) {
                        disposable = d;
                    }

                    @Override
                    public void onNext(DeleteDirentModel resultModel) {
                        SLogs.d(resultModel.toString());
                        //TODO
                        if (!TextUtils.isEmpty(resultModel.error_msg)) {
                            ToastUtils.showLong(resultModel.error_msg);
                        }
                    }

                    @Override
                    public void onError(Throwable t) {
                        SLogs.e(t);
                    }

                    @Override
                    public void onComplete() {
                        getRefreshLiveData().setValue(false);
                        getActionLiveData().setValue(true);
                    }
                });

        addDisposable(disposable);
    }
}
//https://dev.seafile.com/seahub/api/v2.1/repos/4809a6f3-250c-4435-bdd8-b68f34c128d1/dir/?p=%2F%E5%85%94%E5%85%94
