package com.seafile.seadroid2.framework.service.starter;

import android.content.Context;

import androidx.annotation.NonNull;
import androidx.work.Worker;
import androidx.work.WorkerParameters;

import com.seafile.seadroid2.framework.service.TransferService;

/**
 * Because the ForegroundService could not be started from the background
 */
public class AlbumBackupTransferServiceStarter extends Worker {
    public AlbumBackupTransferServiceStarter(@NonNull Context context, @NonNull WorkerParameters workerParams) {
        super(context, workerParams);
    }

    @NonNull
    @Override
    public Result doWork() {
        TransferService.restartPhotoBackupService(getApplicationContext());
        return Result.success();
    }
}
