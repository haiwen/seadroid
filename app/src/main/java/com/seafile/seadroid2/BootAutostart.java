package com.seafile.seadroid2;

import android.app.job.JobInfo;
import android.app.job.JobScheduler;
import android.content.BroadcastReceiver;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.os.Build;

import com.seafile.seadroid2.cameraupload.MediaObserverService;
import com.seafile.seadroid2.cameraupload.MediaSchedulerService;

/**
 * This receiver is called whenever the system has booted or
 * the Seadroid app has been upgraded to a new version.
 * It can be used to start up background services.
 */
public class BootAutostart extends BroadcastReceiver {
    private static final String DEBUG_TAG = "BootAutostart";
    private static final int JOB_ID = 0;

    /**
     * This method will be excecuted after
     * - booting the device
     * - upgrade of the Seadroid package
     */
    public void onReceive(Context context, Intent intent) {

        Intent mediaObserver = new Intent(context, MediaObserverService.class);
        if (Build.VERSION.SDK_INT < Build.VERSION_CODES.LOLLIPOP) {
            context.startService(mediaObserver);
        } else {
            JobScheduler mJobScheduler = (JobScheduler) context.getSystemService(Context.JOB_SCHEDULER_SERVICE);
            JobInfo.Builder builder = new JobInfo.Builder(JOB_ID, new ComponentName(context.getPackageName(),
                    MediaSchedulerService.class.getName()));
            builder.setMinimumLatency(15 * 60 * 1000);// Set to execute after at least 15 minutes delay
            builder.setOverrideDeadline(20 * 60 * 1000);// The setting is delayed by 20 minutes,
            builder.setPersisted(true);
            mJobScheduler.schedule(builder.build());
        }
    }

}
