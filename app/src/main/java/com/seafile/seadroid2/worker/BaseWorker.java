package com.seafile.seadroid2.worker;

import android.content.Context;

import androidx.annotation.NonNull;
import androidx.work.Worker;
import androidx.work.WorkerParameters;

import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.util.SLogs;

import okhttp3.Request;
import okhttp3.RequestBody;

public abstract class BaseWorker extends Worker {
    private final Account currentAccount;

    BaseWorker(@NonNull Context context, @NonNull WorkerParameters workerParams) {
        super(context, workerParams);

        currentAccount = SupportAccountManager.getInstance().getCurrentAccount();
    }

    public Account getCurrentAccount() {
        return currentAccount;
    }

    public Request getGetRequest(String apiPath) {
        SLogs.e("get api-url = " + apiPath);
        return new Request.Builder()
                .url(apiPath)
                .get()
                .header("Authorization", "Token " + currentAccount.token)
                .build();
    }
}
