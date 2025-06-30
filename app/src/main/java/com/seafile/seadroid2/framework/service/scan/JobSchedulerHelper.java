package com.seafile.seadroid2.framework.service.scan;

import android.app.job.JobInfo;
import android.app.job.JobScheduler;
import android.content.ComponentName;
import android.content.Context;

import com.seafile.seadroid2.framework.util.SLogs;

@Deprecated
public class JobSchedulerHelper {
    private static final String TAG = "JobSchedulerHelper";

    private static final int JOB_ID_FOLDER_BACKUP = 1001;
    private static final int JOB_ID_ALBUM_BACKUP = 1002;

    public static void scheduleFolderBackupJob(Context context) {
        SLogs.d(TAG, "scheduleFolderBackupJob");
        ComponentName componentName = new ComponentName(context, FolderBackupScanJobService.class);
        android.app.job.JobInfo.Builder builder = new android.app.job.JobInfo.Builder(JOB_ID_FOLDER_BACKUP, componentName);
        builder.setRequiredNetworkType(JobInfo.NETWORK_TYPE_UNMETERED);
        builder.setPersisted(true);
        builder.setPeriodic(15 * 60 * 1000); //最小 15分钟
        JobInfo jobInfo = builder.build();
        JobScheduler jobScheduler = (JobScheduler) context.getSystemService(Context.JOB_SCHEDULER_SERVICE);
        int r = jobScheduler.schedule(jobInfo);
        if (r == JobScheduler.RESULT_FAILURE) {
            SLogs.d(TAG, "scheduleFolderBackupJob failed");
        } else {
            SLogs.d(TAG, "scheduleFolderBackupJob success");
        }
    }

    public static void cancelFolderBackup(Context context) {
        JobScheduler scheduler = (JobScheduler) context.getSystemService(Context.JOB_SCHEDULER_SERVICE);
        scheduler.cancel(JOB_ID_FOLDER_BACKUP);
    }

    public static void startCameraSyncJob(Context context) {
//        JobScheduler mJobScheduler = (JobScheduler) context.getSystemService(Context.JOB_SCHEDULER_SERVICE);
//        JobInfo.Builder builder = new JobInfo.Builder(JOB_ID_ALBUM_BACKUP, new ComponentName(context.getPackageName(), MediaSchedulerService.class.getName()));
//        builder.setMinimumLatency(5 * 1000);// Set to execute after at least 15 minutes delay
//        builder.setOverrideDeadline(60 * 60 * 1000);// The setting is delayed by 20 minutes,
//        builder.setRequiresCharging(false);
//        builder.setRequiresDeviceIdle(false);
//        builder.setRequiredNetworkType(JobInfo.NETWORK_TYPE_ANY);
//        builder.setPersisted(true);
//        mJobScheduler.schedule(builder.build());
    }
}
