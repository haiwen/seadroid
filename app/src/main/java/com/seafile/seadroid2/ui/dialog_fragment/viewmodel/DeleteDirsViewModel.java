package com.seafile.seadroid2.ui.dialog_fragment.viewmodel;

import androidx.lifecycle.MutableLiveData;

import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.framework.db.AppDatabase;
import com.seafile.seadroid2.framework.db.entities.DirentModel;
import com.seafile.seadroid2.framework.model.dirents.DeleteDirentModel;
import com.seafile.seadroid2.framework.http.HttpIO;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.ui.base.viewmodel.BaseViewModel;
import com.seafile.seadroid2.ui.dialog_fragment.DialogService;

import java.util.List;

import io.reactivex.Flowable;
import io.reactivex.Single;
import io.reactivex.SingleSource;
import io.reactivex.functions.Action;
import io.reactivex.functions.Consumer;
import io.reactivex.functions.Function;

public class DeleteDirsViewModel extends BaseViewModel {
    private final MutableLiveData<Boolean> ActionLiveData = new MutableLiveData<>();

    public MutableLiveData<Boolean> getActionLiveData() {
        return ActionLiveData;
    }

    public void delete(List<String> direntIds) {
        Single<List<DirentModel>> dSingle = AppDatabase.getInstance().direntDao().getListByIdsAsync(direntIds);
        addSingleDisposable(dSingle, new Consumer<List<DirentModel>>() {
            @Override
            public void accept(List<DirentModel> dirents) throws Exception {
                deleteDirents(dirents);
            }
        }, new Consumer<Throwable>() {
            @Override
            public void accept(Throwable throwable) throws Exception {
                getRefreshLiveData().setValue(false);

                SeafException seafException = getSeafExceptionByThrowable(throwable);
                getSeafExceptionLiveData().setValue(seafException);
            }
        });
    }

    private void deleteDirents(List<DirentModel> dirents) {
        getRefreshLiveData().setValue(true);

        Flowable<DeleteDirentModel> flowable = Flowable.fromIterable(dirents)
                .flatMapSingle(new Function<DirentModel, SingleSource<DeleteDirentModel>>() {
                    @Override
                    public SingleSource<DeleteDirentModel> apply(DirentModel dirent) throws Exception {
                        return HttpIO.getCurrentInstance().execute(DialogService.class).deleteDirent(dirent.repo_id, dirent.type, dirent.full_path);
                    }
                });

        addFlowableDisposable(flowable, new Consumer<DeleteDirentModel>() {
            @Override
            public void accept(DeleteDirentModel deleteDirentModel) throws Exception {
                SLogs.d(deleteDirentModel.toString());
            }
        }, new Consumer<Throwable>() {
            @Override
            public void accept(Throwable throwable) throws Exception {
                getRefreshLiveData().setValue(false);

                SeafException seafException = getSeafExceptionByThrowable(throwable);
                getSeafExceptionLiveData().setValue(seafException);
            }
        }, new Action() {
            @Override
            public void run() throws Exception {
                getRefreshLiveData().setValue(false);
                getActionLiveData().setValue(true);
            }
        });

    }
}