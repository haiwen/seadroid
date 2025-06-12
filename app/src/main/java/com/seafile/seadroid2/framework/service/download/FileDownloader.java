package com.seafile.seadroid2.framework.service.download;

import android.content.Context;
import android.os.Bundle;
import android.text.TextUtils;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.enums.TransferDataSource;
import com.seafile.seadroid2.framework.notification.DownloadNotificationHelper;
import com.seafile.seadroid2.framework.notification.base.BaseTransferNotificationHelper;
import com.seafile.seadroid2.framework.service.ParentEventDownloader;
import com.seafile.seadroid2.framework.util.SafeLogs;
import com.seafile.seadroid2.framework.util.Toasts;
import com.seafile.seadroid2.framework.worker.GlobalTransferCacheList;
import com.seafile.seadroid2.framework.worker.TransferEvent;
import com.seafile.seadroid2.framework.worker.TransferWorker;
import com.seafile.seadroid2.framework.worker.queue.TransferModel;

public class FileDownloader extends ParentEventDownloader {
    private final String TAG = "FileDownloader";
    private final DownloadNotificationHelper notificationManager;

    public FileDownloader(Context context, DownloadNotificationHelper downloadNotificationHelper) {
        super(context);

        this.notificationManager = downloadNotificationHelper;
    }


    @Override
    public BaseTransferNotificationHelper getNotificationHelper() {
        return notificationManager;
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
        sendWorkerEvent(TransferDataSource.DOWNLOAD, TransferEvent.EVENT_TRANSFER_TASK_START);

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

        SafeLogs.d(TAG, "download()", "all completed");

        //
        if (resultSeafException != SeafException.SUCCESS) {
            Toasts.show(R.string.download_finished);
        }

        //
        Bundle b = new Bundle();
        if (resultSeafException != SeafException.SUCCESS) {
            b.putString(TransferWorker.KEY_DATA_RESULT, resultSeafException.getMessage());
        }
        b.putInt(TransferWorker.KEY_TRANSFER_COUNT, totalPendingCount);
        sendWorkerEvent(TransferDataSource.DOWNLOAD, TransferEvent.EVENT_TRANSFER_TASK_COMPLETE, b);

        return SeafException.SUCCESS;
    }

    protected SeafException returnSuccess() {
        sendWorkerEvent(TransferDataSource.DOWNLOAD, TransferEvent.EVENT_TRANSFER_TASK_COMPLETE);
        return SeafException.SUCCESS;
    }
}
