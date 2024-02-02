package com.seafile.seadroid2.ui.camera_upload;

import android.app.Service;
import android.content.Intent;
import android.content.SharedPreferences;
import android.database.ContentObserver;
import android.net.Uri;
import android.os.IBinder;
import android.provider.MediaStore;

import com.seafile.seadroid2.util.SLogs;
import com.seafile.seadroid2.util.sp.SettingsManager;

/**
 * This service monitors the media provider content provider for new images/videos.
 * <p>
 * If new content appears, this service will get notified and send a syncRequest to the MediaSyncProvider.
 * <p>
 * This service is always running, even if camera upload is not active.
 * However, it will only register it's ContentObservers if Camera Upload is enabled in Seadroid.
 */
public class MediaObserverService extends Service {
    private MediaObserver mediaObserver = null;
    private CameraUploadManager cameraManager;

    /**
     * If camera upload settings have changed, we might have to trigger a full resync.
     * This listener takes care of that.
     */
    private SharedPreferences.OnSharedPreferenceChangeListener settingsListener = new SharedPreferences.OnSharedPreferenceChangeListener() {

        @Override
        public void onSharedPreferenceChanged(SharedPreferences sharedPreferences, String key) {

            boolean doFullResync = false;

            // here we have to catch *all* the cases that might make a full resync to the repository
            // necessary.

            switch (key) {
                // if video upload has been switched on, do a full sync, to upload
                // any older videos already recorded.
                case SettingsManager.CAMERA_UPLOAD_ALLOW_VIDEOS_SWITCH_KEY: {
                    if (SettingsManager.getInstance().isVideosUploadAllowed()) {
                        doFullResync = true;
                    }
                }
                break;

                // same goes for if the list of selected buckets has been changed
                case SettingsManager.SHARED_PREF_CAMERA_UPLOAD_BUCKETS:
                case SettingsManager.SHARED_PREF_CAMERA_UPLOAD_REPO_ID: {
                    doFullResync = true;
                }
                break;
                default:
                    SLogs.d(key + ": was changed");
                    break;
            }

            if (cameraManager.isCameraUploadEnabled() && doFullResync) {
                SLogs.d("Doing a full resync of all media content.");
                cameraManager.performFullSync();
            }
        }
    };

    @Override
    public void onCreate() {
        SettingsManager.getInstance().registerSharedPreferencesListener(settingsListener);
        cameraManager = new CameraUploadManager();
        registerContentObservers();

        if (cameraManager.isCameraUploadEnabled()) {
            // do a sync in case we missed something while we weren't observing
            cameraManager.performSync();
        }
    }

    @Override
    public void onDestroy() {
        SettingsManager.getInstance().unregisterSharedPreferencesListener(settingsListener);
        unregisterContentObservers();
    }

    @Override
    public int onStartCommand(Intent intent, int flags, int startId) {
        return START_STICKY;
    }

    @Override
    public IBinder onBind(Intent intent) {
        return null;
    }

    private void registerContentObservers() {
        mediaObserver = new MediaObserver();

        getApplicationContext().getContentResolver().registerContentObserver(
                MediaStore.Images.Media.EXTERNAL_CONTENT_URI, true, mediaObserver);

        getApplicationContext().getContentResolver().registerContentObserver
                (MediaStore.Video.Media.EXTERNAL_CONTENT_URI, true, mediaObserver);

        SLogs.i("Started watchting for new media content.");
    }

    private void unregisterContentObservers() {
        this.getApplicationContext().getContentResolver().unregisterContentObserver(mediaObserver);

        SLogs.i("Stopped watchting for new media content.");
    }

    private class MediaObserver extends ContentObserver {
        public MediaObserver() {
            super(null);
        }

        @Override
        public void onChange(boolean selfChange) {
            onChange(selfChange, null);
        }

        @Override
        public void onChange(boolean selfChange, Uri changeUri) {
            if (cameraManager.isCameraUploadEnabled()) {
                SLogs.d("Noticed a change in the media provider, scheduling sync.");
                cameraManager.performSync();
            }
        }
    }
}
