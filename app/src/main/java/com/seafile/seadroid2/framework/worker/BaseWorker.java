package com.seafile.seadroid2.framework.worker;

import android.content.Context;

import androidx.annotation.NonNull;
import androidx.work.Worker;
import androidx.work.WorkerParameters;

import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.framework.notification.TransferNotifications;
import com.seafile.seadroid2.framework.worker.queue.TransferModel;

/**
 * Legacy synchronous Worker. Its original doWork and WorkManager Result remain authoritative.
 */
@Deprecated
public abstract class BaseWorker extends Worker {
    private final Account currentAccount;

    protected BaseWorker(@NonNull Context context, @NonNull WorkerParameters params) {
        super(context, params);
        currentAccount = SupportAccountManager.getInstance().getCurrentAccount();
    }

    public Account getCurrentAccount() {
        return currentAccount;
    }

    public final boolean isTransferStopped() {
        return isStopped();
    }

    protected void cancelOwnedIO() {
    }

    protected void retainInterruptedAttempt() {
    }

    @Override
    public void onStopped() {
        // Cancel only this Worker's IO; do not cancel shared clients or the new execution chain.
        cancelOwnedIO();
    }

    // Source compatibility only: legacy Workers never create or update new notification sessions.
    protected final void noticeScanFailed() {
    }

    protected Object noticeFile(TransferModel model) {
        return null;
    }

    protected void noticeProgress(Object attempt, long sequence, int percent) {
    }

    protected void noticeFileFinished(Object attempt, boolean success) {
    }
}
