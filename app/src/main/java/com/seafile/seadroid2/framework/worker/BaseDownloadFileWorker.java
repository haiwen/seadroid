package com.seafile.seadroid2.framework.worker;

import android.content.Context;

import androidx.annotation.NonNull;
import androidx.work.WorkerParameters;

import com.seafile.seadroid2.framework.data.db.AppDatabase;
import com.seafile.seadroid2.framework.data.db.entities.FileTransferEntity;
import com.seafile.seadroid2.framework.data.model.enums.TransferStatus;
import com.seafile.seadroid2.framework.util.TransferUtils;

public abstract class BaseDownloadFileWorker extends TransferWorker {
    BaseDownloadFileWorker(@NonNull Context context, @NonNull WorkerParameters workerParams) {
        super(context, workerParams);
    }


    protected void catchExceptionAndUpdateDB(FileTransferEntity transferEntity, Exception e) {

        transferEntity.transfer_status = TransferStatus.FAILED;
        transferEntity.action_end_at = System.currentTimeMillis();

        transferEntity.transfer_result = TransferUtils.convertException2TransferResult(e);

        AppDatabase.getInstance().fileTransferDAO().update(transferEntity);
    }
}
