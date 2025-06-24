package com.seafile.seadroid2.framework.worker.starter;

import android.content.Context;

import androidx.annotation.NonNull;
import androidx.work.Worker;
import androidx.work.WorkerParameters;

import com.seafile.seadroid2.framework.service.TransferService;

public class AlbumBackupTransferServiceStarterWorker extends Worker {
    public AlbumBackupTransferServiceStarterWorker(@NonNull Context context, @NonNull WorkerParameters workerParams) {
        super(context, workerParams);
    }

    @NonNull
    @Override
    public Result doWork() {
        TransferService.restartPhotoBackupService(getApplicationContext());
        return Result.success();
    }
}
