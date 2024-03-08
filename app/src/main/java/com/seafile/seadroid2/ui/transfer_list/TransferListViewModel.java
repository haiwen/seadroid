package com.seafile.seadroid2.ui.transfer_list;

import androidx.lifecycle.MutableLiveData;

import com.blankj.utilcode.util.FileUtils;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.data.db.AppDatabase;
import com.seafile.seadroid2.data.db.entities.FileTransferEntity;
import com.seafile.seadroid2.data.model.enums.TransferAction;
import com.seafile.seadroid2.ui.base.viewmodel.BaseViewModel;

import java.util.List;

import io.reactivex.Completable;
import io.reactivex.Single;
import io.reactivex.functions.Action;
import io.reactivex.functions.Consumer;

public class TransferListViewModel extends BaseViewModel {

    private MutableLiveData<List<FileTransferEntity>> mFileTransferEntitiesLiveData = new MutableLiveData<>();

    public MutableLiveData<List<FileTransferEntity>> getFileTransferEntitiesLiveData() {
        return mFileTransferEntitiesLiveData;
    }

    public void loadData(TransferAction transferAction) {
        getRefreshLiveData().setValue(true);
        Account account = SupportAccountManager.getInstance().getCurrentAccount();

        if (account == null) {
            getRefreshLiveData().setValue(false);
            return;
        }

        Single<List<FileTransferEntity>> single = AppDatabase.getInstance().fileTransferDAO().getListByActionAsync(account.getSignature(), transferAction);

        addSingleDisposable(single, new Consumer<List<FileTransferEntity>>() {
            @Override
            public void accept(List<FileTransferEntity> fileTransferEntities) throws Exception {
                mFileTransferEntitiesLiveData.setValue(fileTransferEntities);

                getRefreshLiveData().setValue(false);
            }
        });
    }

    public void deleteTransferData(FileTransferEntity fileTransferEntity, TransferAction transferAction, Consumer<Boolean> consumer) {
        Completable completable = AppDatabase.getInstance().fileTransferDAO().deleteAsyncById(fileTransferEntity.uid);
        addCompletableDisposable(completable, new Action() {
            @Override
            public void run() throws Exception {
                if (transferAction == TransferAction.DOWNLOAD) {
                    FileUtils.delete(fileTransferEntity.target_path);
                }

                consumer.accept(true);
            }
        });
    }
}
