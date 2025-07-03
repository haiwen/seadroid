package com.seafile.seadroid2.framework.service;

import android.app.job.JobInfo;
import android.app.job.JobScheduler;
import android.content.ComponentName;
import android.content.Context;
import android.os.Build;

import androidx.work.BackoffPolicy;
import androidx.work.Constraints;
import androidx.work.ExistingPeriodicWorkPolicy;
import androidx.work.ExistingWorkPolicy;
import androidx.work.ListenableWorker;
import androidx.work.NetworkType;
import androidx.work.OneTimeWorkRequest;
import androidx.work.PeriodicWorkRequest;
import androidx.work.WorkManager;
import androidx.work.WorkRequest;

import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.framework.datastore.sp_livedata.AlbumBackupSharePreferenceHelper;
import com.seafile.seadroid2.framework.datastore.sp_livedata.FolderBackupSharePreferenceHelper;
import com.seafile.seadroid2.framework.service.scan.AlbumBackupScanJobService;
import com.seafile.seadroid2.framework.service.scan.FolderBackupScanJobService;
import com.seafile.seadroid2.framework.worker.periodic.AlbumBackupPeriodicScanStarter;
import com.seafile.seadroid2.framework.service.starter.AlbumBackupTransferServiceStarter;
import com.seafile.seadroid2.framework.worker.periodic.FolderBackupPeriodicScanStarter;
import com.seafile.seadroid2.framework.util.SLogs;

import java.util.concurrent.TimeUnit;

public class BackgroundWorkScheduler {
    private final String TAG = "BackgroundWorkManager";

    private final long DEFAULT_PERIODIC_JOB_INTERVAL_MINUTES = 15L;

    private BackgroundWorkScheduler() {

    }

    public static BackgroundWorkScheduler getInstance() {
        return SingletonHolder.INSTANCE;
    }

    private static class SingletonHolder {
        private static final BackgroundWorkScheduler INSTANCE = new BackgroundWorkScheduler();
    }

    private <T extends ListenableWorker> OneTimeWorkRequest.Builder oneTimeRequestBuilder(Class<T> tClass) {
        SLogs.d(TAG, "oneTimeRequestBuilder()", "Creating WorkRequest for: " + tClass.getSimpleName());

        return new OneTimeWorkRequest.Builder(tClass)
                .setBackoffCriteria(BackoffPolicy.LINEAR, WorkRequest.MIN_BACKOFF_MILLIS, TimeUnit.MILLISECONDS)
                .setInitialDelay(0, TimeUnit.SECONDS)
                .addTag(tClass.getSimpleName());
    }

    private <T extends ListenableWorker> PeriodicWorkRequest.Builder periodicRequestBuilder(Class<T> tClass) {
        return new PeriodicWorkRequest.Builder(tClass, DEFAULT_PERIODIC_JOB_INTERVAL_MINUTES, TimeUnit.MINUTES);
    }

    public WorkManager getWorkManager() {
        return WorkManager.getInstance(SeadroidApplication.getAppContext());
    }

    // album backup
    public void startAlbumBackupTransferService(Context context) {
        OneTimeWorkRequest.Builder builder = oneTimeRequestBuilder(AlbumBackupTransferServiceStarter.class);

        WorkManager.getInstance(context).enqueueUniqueWork(
                AlbumBackupTransferServiceStarter.class.getName(),
                ExistingWorkPolicy.REPLACE,
                builder.build()
        );
    }

}
