package com.seafile.seadroid2.framework.worker;

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
import androidx.work.WorkInfo;
import androidx.work.WorkManager;

import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.framework.datastore.sp_livedata.AlbumBackupSharePreferenceHelper;
import com.seafile.seadroid2.framework.datastore.sp_livedata.FolderBackupSharePreferenceHelper;
import com.seafile.seadroid2.framework.service.scan.AlbumBackupScanJobService;
import com.seafile.seadroid2.framework.service.scan.FolderBackupScanJobService;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.worker.periodic.AlbumBackupPeriodicScanStarter;
import com.seafile.seadroid2.framework.worker.periodic.FolderBackupPeriodicScanStarter;
import com.seafile.seadroid2.framework.worker.upload.FolderBackupScanWorker;
import com.seafile.seadroid2.framework.worker.upload.FolderBackupUploadWorker;
import com.seafile.seadroid2.framework.worker.upload.MediaBackupScanWorker;
import com.seafile.seadroid2.framework.worker.upload.MediaBackupUploadWorker;

import java.util.List;
import java.util.concurrent.TimeUnit;

public class BackgroundJobManagerImpl {
    private static final String TAG = "BackgroundJobManagerImpl";
    public static final String TAG_ALL = "*";

    private final long DEFAULT_PERIODIC_JOB_INTERVAL_MINUTES = 15L;
    private static final int JOB_ID_FOLDER_BACKUP = 10001;
    private static final int JOB_ID_ALBUM_BACKUP = 10002;

    private BackgroundJobManagerImpl() {

    }

    public static BackgroundJobManagerImpl getInstance() {
        return SingletonHolder.INSTANCE;
    }

    private static class SingletonHolder {
        private static final BackgroundJobManagerImpl INSTANCE = new BackgroundJobManagerImpl();
    }

    private <T extends ListenableWorker> OneTimeWorkRequest.Builder oneTimeRequestBuilder(Class<T> tClass) {
        SLogs.d(TAG, "oneTimeRequestBuilder()", "Creating WorkRequest for: " + tClass.getSimpleName());

        return new OneTimeWorkRequest.Builder(tClass)
                .setBackoffCriteria(BackoffPolicy.LINEAR, 5, TimeUnit.SECONDS)
                .addTag(TAG_ALL)
                .addTag(tClass.getSimpleName());
    }

    private <T extends ListenableWorker> PeriodicWorkRequest.Builder periodicRequestBuilder(Class<T> tClass) {
        return new PeriodicWorkRequest.Builder(tClass, DEFAULT_PERIODIC_JOB_INTERVAL_MINUTES, TimeUnit.MINUTES)
                .addTag(TAG_ALL)
                .addTag(tClass.getSimpleName());
    }

    public WorkManager getWorkManager() {
        return WorkManager.getInstance(SeadroidApplication.getAppContext());
    }

    // get is running
    public boolean getAlbumModuleRunning() {
        boolean ab = AlbumBackupPeriodicScanStarter.isIsWorkerRunning();
        if (ab) {
            return true;
        }

        boolean sb = MediaBackupScanWorker.isIsWorkerRunning();
        if (sb) {
            return true;
        }

        boolean ub = MediaBackupUploadWorker.isIsWorkerRunning();
        if (ub) {
            return true;
        }

        return false;
    }

    public boolean getFolderModuleRunning() {
        boolean ab = FolderBackupPeriodicScanStarter.isIsWorkerRunning();
        if (ab) {
            return true;
        }

        boolean sb = FolderBackupScanWorker.isIsWorkerRunning();
        if (sb) {
            return true;
        }

        boolean ub = FolderBackupUploadWorker.isIsWorkerRunning();
        if (ub) {
            return true;
        }

        return false;
    }

    /**
     * Schedules a periodic AlbumBackupScanWorker.
     */
    public void scheduleAlbumBackupPeriodicScan(Context context) {
        assert context != null;

        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.Q) {
            PeriodicWorkRequest scanPeriodicRequest = getAlbumBackupScanWorkerRequest();

            getWorkManager().enqueueUniquePeriodicWork(
                    AlbumBackupPeriodicScanStarter.TAG,
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
            WorkManager wm = getWorkManager();
            wm.cancelUniqueWork(AlbumBackupPeriodicScanStarter.TAG);
            wm.pruneWork();
        } else {
            JobScheduler scheduler = (JobScheduler) context.getSystemService(Context.JOB_SCHEDULER_SERVICE);
            scheduler.cancel(JOB_ID_ALBUM_BACKUP);
        }
    }

    private PeriodicWorkRequest getAlbumBackupScanWorkerRequest() {
        Constraints constraints = new Constraints.Builder()
                .setRequiredNetworkType(NetworkType.CONNECTED)
                .build();

        return periodicRequestBuilder(AlbumBackupPeriodicScanStarter.class)
                .setId(AlbumBackupPeriodicScanStarter.UID)
                .setInitialDelay(10, TimeUnit.MINUTES)
                .setConstraints(constraints)
                .build();
    }

    public void scheduleFolderBackupPeriodicScan(Context context) {
        assert context != null;

        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.Q) {
            PeriodicWorkRequest scanPeriodicRequest = getFolderBackupScanWorkerRequest();

            getWorkManager().enqueueUniquePeriodicWork(
                    FolderBackupPeriodicScanStarter.TAG,
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
            WorkManager wm = getWorkManager();
            wm.cancelUniqueWork(FolderBackupPeriodicScanStarter.TAG);
            wm.pruneWork();
        } else {
            JobScheduler scheduler = (JobScheduler) context.getSystemService(Context.JOB_SCHEDULER_SERVICE);
            scheduler.cancel(JOB_ID_FOLDER_BACKUP);
        }
    }

    private PeriodicWorkRequest getFolderBackupScanWorkerRequest() {

        Constraints constraints = new Constraints.Builder()
                .setRequiredNetworkType(NetworkType.CONNECTED)
                .build();

        return periodicRequestBuilder(FolderBackupPeriodicScanStarter.class)
                .setId(FolderBackupPeriodicScanStarter.UID)
                .setInitialDelay(10, TimeUnit.MINUTES)
                .setConstraints(constraints)
                .build();
    }

    /// /////////////////// media //////////////////////

    public void startMediaBackupChain(boolean isForce) {
        SLogs.d(TAG, "startMediaBackupChain()", "isForce:" + isForce);

        OneTimeWorkRequest scanRequest = getMediaScannerWorkerRequest(isForce);
        OneTimeWorkRequest uploadRequest = getMediaUploadWorkerRequest();

        String workerName = MediaBackupScanWorker.class.getSimpleName();

        getWorkManager()
                .beginUniqueWork(workerName, ExistingWorkPolicy.KEEP, scanRequest)
                .then(uploadRequest)
                .enqueue();
    }

    private OneTimeWorkRequest getMediaScannerWorkerRequest(boolean isForce) {
        Data data = new Data.Builder()
                .putBoolean(TransferWorker.DATA_FORCE_TRANSFER_KEY, isForce)
                .build();

        Constraints constraints = new Constraints.Builder()
                .setRequiredNetworkType(NetworkType.CONNECTED)
                .build();

        return oneTimeRequestBuilder(MediaBackupScanWorker.class)
                .setInputData(data)
                .setConstraints(constraints)
                .setId(MediaBackupScanWorker.UID)
                .build();
    }

    private OneTimeWorkRequest getMediaUploadWorkerRequest() {
        NetworkType networkType = NetworkType.UNMETERED;
        boolean isAllowData = AlbumBackupSharePreferenceHelper.readAllowDataPlanSwitch();
        if (isAllowData) {
            networkType = NetworkType.CONNECTED;
        }

        Constraints constraints = new Constraints.Builder()
                .setRequiredNetworkType(networkType)
                .build();

        return oneTimeRequestBuilder(MediaBackupUploadWorker.class)
                .setConstraints(constraints)
                .setId(MediaBackupUploadWorker.UID)
                .build();
    }


    /// /////////////////// upload folder //////////////////////

    public void startFolderBackupChain(boolean isForce) {
        SLogs.d(TAG, "startFolderBackupChain()", "isForce:" + isForce);

        OneTimeWorkRequest scanRequest = getFolderBackupScanWorkerRequest(isForce);
        OneTimeWorkRequest uploadRequest = getFolderBackupUploadWorkerRequest();

        String workerName = FolderBackupScanWorker.class.getSimpleName();

        getWorkManager()
                .beginUniqueWork(workerName, ExistingWorkPolicy.KEEP, scanRequest)
                .then(uploadRequest)
                .enqueue();
    }

    private OneTimeWorkRequest getFolderBackupScanWorkerRequest(boolean isForce) {
        Data data = new Data.Builder()
                .putBoolean(TransferWorker.DATA_FORCE_TRANSFER_KEY, isForce)
                .build();

        Constraints constraints = new Constraints.Builder()
                .setRequiredNetworkType(NetworkType.CONNECTED)
                .build();

        return oneTimeRequestBuilder(FolderBackupScanWorker.class)
                .setInputData(data)
                .setId(FolderBackupScanWorker.UID)
                .setConstraints(constraints)
                .build();
    }

    private OneTimeWorkRequest getFolderBackupUploadWorkerRequest() {
        NetworkType networkType = NetworkType.UNMETERED;
        if (FolderBackupSharePreferenceHelper.readDataPlanAllowed()) {
            networkType = NetworkType.CONNECTED;
        }

        Constraints constraints = new Constraints.Builder()
                .setRequiredNetworkType(networkType)
                .build();

        return oneTimeRequestBuilder(FolderBackupUploadWorker.class)
                .setConstraints(constraints)
                .setId(FolderBackupUploadWorker.UID)
                .build();
    }

    public void cancelAllJobs() {
        getWorkManager().cancelAllWork();
    }
}
