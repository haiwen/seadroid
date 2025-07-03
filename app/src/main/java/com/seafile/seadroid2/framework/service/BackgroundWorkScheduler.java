package com.seafile.seadroid2.framework.service;

import android.app.job.JobInfo;
import android.app.job.JobScheduler;
import android.content.ComponentName;
import android.content.Context;
import android.os.Build;

import androidx.work.BackoffPolicy;
import androidx.work.Constraints;
import androidx.work.Data;
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
import com.seafile.seadroid2.framework.service.starter.AlbumBackupScanStarter;
import com.seafile.seadroid2.framework.service.starter.AlbumBackupTransferServiceStarter;
import com.seafile.seadroid2.framework.service.starter.FolderBackupScanStarter;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.worker.TransferWorker;

import java.util.concurrent.TimeUnit;

public class BackgroundWorkScheduler {
    private final String TAG = "BackgroundWorkManager";

    private final long DEFAULT_PERIODIC_JOB_INTERVAL_MINUTES = 15L;
    private static final int JOB_ID_FOLDER_BACKUP = 10001;
    private static final int JOB_ID_ALBUM_BACKUP = 10002;

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

    /**
     * Schedules a periodic AlbumBackupScanWorker.
     */
    public void scheduleAlbumBackupPeriodicScan(Context context) {
        assert context != null;

        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.Q) {
            PeriodicWorkRequest scanPeriodicRequest = getAlbumBackupScanWorkerRequest();

            getWorkManager().enqueueUniquePeriodicWork(
                    AlbumBackupScanStarter.TAG,
                    ExistingPeriodicWorkPolicy.KEEP,
                    scanPeriodicRequest
            );
        } else {
            ComponentName component = new ComponentName(context, AlbumBackupScanJobService.class);
            boolean isAllowData = AlbumBackupSharePreferenceHelper.readAllowDataPlanSwitch();
            JobInfo.Builder builder = new JobInfo.Builder(JOB_ID_ALBUM_BACKUP, component)
                    .setRequiredNetworkType(isAllowData ? JobInfo.NETWORK_TYPE_ANY : JobInfo.NETWORK_TYPE_UNMETERED)
                    .setPersisted(true)
                    .setPeriodic(15 * 60 * 1000); // 15m

            JobScheduler scheduler = (JobScheduler) context.getSystemService(Context.JOB_SCHEDULER_SERVICE);
            scheduler.schedule(builder.build());
        }
    }

    public void stopAlbumBackupPeriodicScan(Context context) {
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.Q) {
            getWorkManager().cancelUniqueWork(AlbumBackupScanStarter.TAG);
        } else {
            JobScheduler scheduler = (JobScheduler) context.getSystemService(Context.JOB_SCHEDULER_SERVICE);
            scheduler.cancel(JOB_ID_ALBUM_BACKUP);
        }
    }

    private PeriodicWorkRequest getAlbumBackupScanWorkerRequest() {
        Constraints constraints = new Constraints.Builder()
                .setRequiredNetworkType(NetworkType.CONNECTED)
                .build();

        return periodicRequestBuilder(AlbumBackupScanStarter.class)
                .setId(AlbumBackupScanStarter.UID)
                .setConstraints(constraints)
                .build();
    }

    public void scheduleFolderBackupPeriodicScan(Context context) {
        assert context != null;

        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.Q) {
            PeriodicWorkRequest scanPeriodicRequest = getFolderBackupScanWorkerRequest();

            getWorkManager().enqueueUniquePeriodicWork(
                    FolderBackupScanStarter.TAG,
                    ExistingPeriodicWorkPolicy.KEEP,
                    scanPeriodicRequest
            );
        } else {
            ComponentName component = new ComponentName(context, FolderBackupScanJobService.class);
            boolean isAllowData = FolderBackupSharePreferenceHelper.readDataPlanAllowed();
            JobInfo.Builder builder = new JobInfo.Builder(JOB_ID_FOLDER_BACKUP, component)
                    .setRequiredNetworkType(isAllowData ? JobInfo.NETWORK_TYPE_ANY : JobInfo.NETWORK_TYPE_UNMETERED)
                    .setPersisted(true)
                    .setPeriodic(15 * 60 * 1000); // 15m

            JobScheduler scheduler = (JobScheduler) context.getSystemService(Context.JOB_SCHEDULER_SERVICE);
            scheduler.schedule(builder.build());
        }
    }

    public void stopFolderBackupPeriodicScan(Context context) {
        assert context != null;

        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.Q) {
            getWorkManager().cancelUniqueWork(FolderBackupScanStarter.TAG);
        } else {
            JobScheduler scheduler = (JobScheduler) context.getSystemService(Context.JOB_SCHEDULER_SERVICE);
            scheduler.cancel(JOB_ID_FOLDER_BACKUP);
        }
    }

    private PeriodicWorkRequest getFolderBackupScanWorkerRequest() {

        Constraints constraints = new Constraints.Builder()
                .setRequiredNetworkType(NetworkType.CONNECTED)
                .build();

        return periodicRequestBuilder(FolderBackupScanStarter.class)
                .setId(FolderBackupScanStarter.UID)
                .setConstraints(constraints)
                .build();
    }
}
