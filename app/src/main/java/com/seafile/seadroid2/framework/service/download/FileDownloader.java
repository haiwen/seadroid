package com.seafile.seadroid2.framework.service.download;

import android.content.Context;
import android.os.Bundle;
import android.text.TextUtils;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.enums.FeatureDataSource;
import com.seafile.seadroid2.enums.TransferDataSource;
import com.seafile.seadroid2.framework.notification.DownloadNotificationHelper;
import com.seafile.seadroid2.framework.notification.TransferNotificationDispatcher;
import com.seafile.seadroid2.framework.notification.base.BaseTransferNotificationHelper;
import com.seafile.seadroid2.framework.service.ITransferNotification;
import com.seafile.seadroid2.framework.service.ParentEventDownloader;
import com.seafile.seadroid2.framework.util.SafeLogs;
import com.seafile.seadroid2.framework.util.Toasts;
import com.seafile.seadroid2.framework.worker.GlobalTransferCacheList;
import com.seafile.seadroid2.framework.worker.TransferEvent;
import com.seafile.seadroid2.framework.worker.TransferWorker;
import com.seafile.seadroid2.framework.worker.queue.TransferModel;

import java.util.concurrent.locks.ReentrantLock;

public class FileDownloader extends ParentEventDownloader {
    private final String TAG = "FileDownloader";

    public FileDownloader(Context context, ITransferNotification n) {
        super(context, n);
    }

    @Override
    public FeatureDataSource getFeatureDataSource() {
        return FeatureDataSource.DOWNLOAD;
    }

    public void stop() {
        SafeLogs.d(TAG, "stop()");

        //clear
        GlobalTransferCacheList.DOWNLOAD_QUEUE.clear();

        //stop
        stopThis();

        send(FeatureDataSource.DOWNLOAD, TransferEvent.EVENT_TRANSFER_TASK_CANCELLED);
    }


    public void stopById(String modelId) {
        SafeLogs.d(TAG, "stopById()", "stop download by id: " + modelId);

        TransferModel transferModel = getCurrentTransferringModel();
        if (transferModel == null) {
            return;
        }

        if (!TextUtils.equals(modelId, transferModel.getId())) {
            GlobalTransferCacheList.DOWNLOAD_QUEUE.remove(modelId);
            return;
        }

        //stop
        stopThis();
    }

    public SeafException download() {
        SafeLogs.d(TAG, "download()", "start download");
        //send a start event
        send(FeatureDataSource.DOWNLOAD, TransferEvent.EVENT_TRANSFER_TASK_START);

        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        if (account == null) {
            return returnSuccess();
        }

        //count
        int totalPendingCount = GlobalTransferCacheList.DOWNLOAD_QUEUE.getPendingCount();
        if (totalPendingCount <= 0) {
            SafeLogs.d(TAG, "doWork()", "download list is empty.");
            return returnSuccess();
        }

        SafeLogs.d(TAG, "download()", "pending count: " + totalPendingCount);

        //tip
        String tip = getContext().getResources().getQuantityString(R.plurals.transfer_download_started, totalPendingCount, totalPendingCount);
        Toasts.show(tip);

        SeafException resultSeafException = SeafException.SUCCESS;
        while (true) {
            TransferModel transferModel = GlobalTransferCacheList.DOWNLOAD_QUEUE.pick();
            if (transferModel == null) {
                break;
            }

            try {
                transfer(account, transferModel);

            } catch (SeafException seafException) {

                SafeLogs.e("An exception occurred and the transmission has been interrupted");
                notifyError(seafException);

                resultSeafException = seafException;
            }
        }

        //
        String errorMsg = null;
        if (resultSeafException != SeafException.SUCCESS) {
            errorMsg = resultSeafException.getMessage();

            SafeLogs.d(TAG, "download()", "all completed", "errorMsg: " + errorMsg);
            Toasts.show(R.string.download_finished);
        } else {
            SafeLogs.d(TAG, "download()", "all completed");
            Toasts.show(R.string.download_completed);
        }

        // clear all notifications
        getTransferNotificationDispatcher().clearAll();


        sendCompleteEvent(FeatureDataSource.DOWNLOAD, errorMsg, totalPendingCount);
        return SeafException.SUCCESS;
    }

    protected SeafException returnSuccess() {
        send(FeatureDataSource.DOWNLOAD, TransferEvent.EVENT_TRANSFER_TASK_COMPLETE);
        return SeafException.SUCCESS;
    }
}
