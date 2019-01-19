package com.seafile.seadroid2.cameraupload;


import android.app.job.JobParameters;
import android.app.job.JobService;
import android.content.SharedPreferences;
import android.os.Build;
import android.support.annotation.RequiresApi;

import com.seafile.seadroid2.SettingsManager;

/**
 * This service monitors the media provider content provider for new images/videos.
 */
@RequiresApi(api = Build.VERSION_CODES.LOLLIPOP)
public class MediaSchedulerService extends JobService {
    private SettingsManager mSettingsManager;
    private CameraUploadManager mCameraManager;


    @Override
    public boolean onStartJob(JobParameters jobParameters) {
        mSettingsManager = SettingsManager.instance();
        mSettingsManager.registerSharedPreferencesListener(settingsListener);
        mCameraManager = new CameraUploadManager(getApplicationContext());
        if (mCameraManager.isCameraUploadEnabled() && mSettingsManager.isVideosUploadAllowed()) {
            mCameraManager.performFullSync();
        }
        jobFinished(jobParameters, true);
        return true;
    }

    @Override
    public boolean onStopJob(JobParameters jobParameters) {
        if (mSettingsManager != null) {
            mSettingsManager.unregisterSharedPreferencesListener(settingsListener);
        }
        return false;
    }


    /**
     * If camera upload settings have changed, we might have to trigger a full resync.
     * This listener takes care of that.
     */
    private SharedPreferences.OnSharedPreferenceChangeListener settingsListener =
            new SharedPreferences.OnSharedPreferenceChangeListener() {

                @Override
                public void onSharedPreferenceChanged(SharedPreferences sharedPreferences, String key) {

                    boolean doFullResync = false;

                    // here we have to catch *all* the cases that might make a full resync to the repository
                    // necessary.

                    switch (key) {

                        // if video upload has been switched on, do a full sync, to upload
                        // any older videos already recorded.
                        case SettingsManager.CAMERA_UPLOAD_ALLOW_VIDEOS_SWITCH_KEY:
                            if (mSettingsManager != null && mSettingsManager.isVideosUploadAllowed())
                                doFullResync = true;
                            break;

                        // same goes for if the list of selected buckets has been changed
                        case SettingsManager.SHARED_PREF_CAMERA_UPLOAD_BUCKETS:
                            doFullResync = true;
                            break;

                        // the repo changed, also do a full resync
                        case SettingsManager.SHARED_PREF_CAMERA_UPLOAD_REPO_ID:
                            doFullResync = true;
                            break;
                    }

                    if (mCameraManager != null && mCameraManager.isCameraUploadEnabled() && doFullResync) {
                        // Log.i(DEBUG_TAG, "Doing a full resync of all media content.");
                        mCameraManager.performFullSync();
                    }
                }
            };
}
