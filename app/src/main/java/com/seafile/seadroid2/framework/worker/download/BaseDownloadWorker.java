package com.seafile.seadroid2.framework.worker.download;

import android.content.Context;
import android.os.Bundle;

import androidx.annotation.NonNull;
import androidx.work.WorkerParameters;

import com.seafile.seadroid2.enums.TransferDataSource;
import com.seafile.seadroid2.framework.notification.base.BaseNotification;
import com.seafile.seadroid2.framework.worker.TransferEvent;
import com.seafile.seadroid2.framework.worker.TransferWorker;

public abstract class BaseDownloadWorker extends TransferWorker {

    public abstract BaseNotification getNotification();

    public BaseDownloadWorker(@NonNull Context context, @NonNull WorkerParameters workerParams) {
        super(context, workerParams);
    }
}
