package com.seafile.seadroid2.ui.dialog_fragment.viewmodel;

import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.framework.db.AppDatabase;
import com.seafile.seadroid2.baseviewmodel.BaseViewModel;

import io.reactivex.Completable;
import io.reactivex.functions.Action;
import io.reactivex.functions.Consumer;

public class ClearPasswordViewModel extends BaseViewModel {
    public void clear(Account account, Consumer<Boolean> consumer) {
        Completable completable = AppDatabase.getInstance().encKeyCacheDAO().deleteSpecialAccountData(account.getSignature());
        addCompletableDisposable(completable, new Action() {
            @Override
            public void run() throws Exception {
                if (consumer != null) {
                    consumer.accept(true);
                }
            }
        });

    }
}
