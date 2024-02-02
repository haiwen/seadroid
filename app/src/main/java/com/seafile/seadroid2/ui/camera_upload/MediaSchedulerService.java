package com.seafile.seadroid2.ui.camera_upload;


import android.app.job.JobParameters;
import android.app.job.JobService;

import com.seafile.seadroid2.util.SLogs;
import com.seafile.seadroid2.util.sp.SettingsManager;

/**
 * This service monitors the media provider content provider for new images/videos.
 */
public class MediaSchedulerService extends JobService {
    @Override
    public boolean onStartJob(JobParameters jobParameters) {
        SLogs.d("MediaSchedulerService exec job");

        CameraUploadManager mCameraManager = new CameraUploadManager();
        if (mCameraManager.isCameraUploadEnabled() && SettingsManager.getInstance().isVideosUploadAllowed()) {
            mCameraManager.performFullSync();
        }

        jobFinished(jobParameters, true);
        return true;
    }

    @Override
    public boolean onStopJob(JobParameters jobParameters) {
        SLogs.d("MediaSchedulerService job stop");
        return false;
    }
}
