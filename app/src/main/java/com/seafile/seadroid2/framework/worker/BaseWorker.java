package com.seafile.seadroid2.framework.worker;

import android.content.Context;

import androidx.annotation.NonNull;
import androidx.work.Worker;
import androidx.work.WorkerParameters;

import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.framework.util.SLogs;

import okhttp3.Request;

public abstract class BaseWorker extends Worker {
    private final Account currentAccount;

    protected BaseWorker(@NonNull Context context, @NonNull WorkerParameters workerParams) {
        super(context, workerParams);

        currentAccount = SupportAccountManager.getInstance().getCurrentAccount();
    }

    public Account getCurrentAccount() {
        return currentAccount;
    }
}
