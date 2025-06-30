package com.seafile.seadroid2.framework.service;

import android.content.Context;
import android.text.TextUtils;

import androidx.work.BackoffPolicy;
import androidx.work.Constraints;
import androidx.work.Data;
import androidx.work.ExistingPeriodicWorkPolicy;
import androidx.work.ExistingWorkPolicy;
import androidx.work.ListenableWorker;
import androidx.work.NetworkType;
import androidx.work.OneTimeWorkRequest;
import androidx.work.OutOfQuotaPolicy;
import androidx.work.PeriodicWorkRequest;
import androidx.work.WorkManager;

import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.framework.datastore.sp_livedata.AlbumBackupSharePreferenceHelper;
import com.seafile.seadroid2.framework.datastore.sp_livedata.FolderBackupSharePreferenceHelper;
import com.seafile.seadroid2.framework.service.starter.AlbumBackupScanStarter;
import com.seafile.seadroid2.framework.service.starter.AlbumBackupTransferServiceStarter;
import com.seafile.seadroid2.framework.service.starter.FolderBackupScanStarter;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.worker.TransferWorker;
import com.seafile.seadroid2.framework.worker.download.DownloadFileScannerWorker;
import com.seafile.seadroid2.framework.worker.download.DownloadWorker;
import com.seafile.seadroid2.framework.worker.reupoad.DownloadedFileMonitorWorker;
import com.seafile.seadroid2.framework.worker.upload.FileUploadWorker;
import com.seafile.seadroid2.framework.worker.upload.FolderBackupUploadWorker;
import com.seafile.seadroid2.framework.worker.upload.MediaBackupScanWorker;
import com.seafile.seadroid2.framework.worker.upload.MediaBackupUploadWorker;

import java.util.List;
import java.util.UUID;
import java.util.concurrent.TimeUnit;

public class BackgroundWorkManager {
    private final String TAG = "BackgroundWorkManager";

    private final long DEFAULT_PERIODIC_JOB_INTERVAL_MINUTES = 15L;

    private BackgroundWorkManager() {

    }

    public static BackgroundWorkManager getInstance() {
        return SingletonHolder.INSTANCE;
    }

    private static class SingletonHolder {
        private static final BackgroundWorkManager INSTANCE = new BackgroundWorkManager();
    }

    private <T extends ListenableWorker> OneTimeWorkRequest.Builder oneTimeRequestBuilder(Class<T> tClass) {
        SLogs.d(TAG, "oneTimeRequestBuilder()", "Creating WorkRequest for: " + tClass.getSimpleName());

        return new OneTimeWorkRequest.Builder(tClass)
                .setBackoffCriteria(BackoffPolicy.LINEAR, 5, TimeUnit.SECONDS);
    }

    private <T extends ListenableWorker> PeriodicWorkRequest.Builder periodicRequestBuilder(Class<T> tClass) {
        return new PeriodicWorkRequest.Builder(tClass, DEFAULT_PERIODIC_JOB_INTERVAL_MINUTES, TimeUnit.MINUTES);
    }

    public WorkManager getWorkManager() {
        return WorkManager.getInstance(SeadroidApplication.getAppContext());
    }

    public static void startAlbumBackupTransferService(Context context) {
        OneTimeWorkRequest request = new OneTimeWorkRequest.Builder(AlbumBackupTransferServiceStarter.class)
                .setConstraints(new Constraints.Builder().build())
                .setInitialDelay(0, TimeUnit.SECONDS)
                .build();

        WorkManager.getInstance(context).enqueueUniqueWork(
                AlbumBackupTransferServiceStarter.class.getName(),
                ExistingWorkPolicy.REPLACE,
                request
        );
    }

    public void scheduleAlbumBackupPeriodicScan() {
        PeriodicWorkRequest scanPeriodicRequest = getAlbumBackupScanWorkerRequest();

        getWorkManager().enqueueUniquePeriodicWork(
                AlbumBackupScanStarter.TAG,
                ExistingPeriodicWorkPolicy.KEEP,
                scanPeriodicRequest
        );
    }

    public void stopAlbumBackupPeriodicScan() {
        getWorkManager().cancelUniqueWork(AlbumBackupScanStarter.TAG);
    }

    private PeriodicWorkRequest getAlbumBackupScanWorkerRequest() {
        Constraints constraints = new Constraints.Builder()
                .setRequiredNetworkType(NetworkType.CONNECTED)
                .build();

        return periodicRequestBuilder(AlbumBackupScanStarter.class)
                .setId(FolderBackupScanStarter.UID)
                .setConstraints(constraints)
                .build();
    }

    /**
     * Schedules a periodic FolderBackupScanWorker.
     * The FolderBackupScanWorker itself will enqueue a FolderBackupUploadWorker if needed.
     */
    public void scheduleFolderBackupPeriodicScan() {
        PeriodicWorkRequest scanPeriodicRequest = getFolderBackupScanWorkerRequest();

        getWorkManager().enqueueUniquePeriodicWork(
                FolderBackupScanStarter.TAG,
                ExistingPeriodicWorkPolicy.KEEP,
                scanPeriodicRequest
        );
    }

    public void stopFolderBackupPeriodicScan() {
        getWorkManager().cancelUniqueWork(FolderBackupScanStarter.TAG);
    }

    private PeriodicWorkRequest getFolderBackupScanWorkerRequest() {
        Data data = new Data.Builder()
                .putBoolean(TransferWorker.DATA_FORCE_TRANSFER_KEY, true)
                .build();

        Constraints constraints = new Constraints.Builder()
                .setRequiredNetworkType(NetworkType.CONNECTED)
                .build();

        return periodicRequestBuilder(FolderBackupScanStarter.class)
                .setInputData(data)
                .setId(FolderBackupScanStarter.UID)
                .setConstraints(constraints)
                .build();
    }
}
