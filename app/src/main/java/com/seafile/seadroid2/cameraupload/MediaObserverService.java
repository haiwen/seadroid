package com.seafile.seadroid2.cameraupload;

import android.app.Notification;
import android.app.Service;
import android.content.Intent;
import android.content.SharedPreferences;
import android.database.ContentObserver;
import android.net.Uri;
import android.os.IBinder;
import android.provider.MediaStore;

import com.seafile.seadroid2.SettingsManager;
import com.seafile.seadroid2.notification.BaseNotificationProvider;

/**
 * This service monitors the media provider content provider for new images/videos.
 *
 * If new content appears, this service will get notified and send a syncRequest to the MediaSyncProvider.
 *
 * This service is always running, even if camera upload is not active.
 * However, it will only register it's ContentObservers if Camera Upload is enabled in Seadroid.
 */
public class MediaObserverService extends Service {
    private static final String DEBUG_TAG = "MediaObserverService";

    private MediaObserver mediaObserver = null;
    private SettingsManager settingsManager;
    private CameraUploadManager cameraManager;

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
                    if (settingsManager.isVideosUploadAllowed())
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

            if (cameraManager.isCameraUploadEnabled() && doFullResync) {
                // Log.i(DEBUG_TAG, "Doing a full resync of all media content.");
                cameraManager.performFullSync();
            }
        }
    };

    @Override
    public void onCreate() {
        // Log.d(DEBUG_TAG, "onCreate");
        settingsManager = SettingsManager.instance();
        settingsManager.registerSharedPreferencesListener(settingsListener);
        cameraManager = new CameraUploadManager(getApplicationContext());
        registerContentObservers();

        if (cameraManager.isCameraUploadEnabled()) {
            // do a sync in case we missed something while we weren't observing
            cameraManager.performSync();
        }
    }

    @Override
    public void onDestroy() {
        // Log.d(DEBUG_TAG, "onDestroy");
        settingsManager.unregisterSharedPreferencesListener(settingsListener);
        unregisterContentObservers();
        stopForeground(true);
    }

    @Override
    public int onStartCommand(Intent intent, int flags, int startId) {

        Notification notification = new Notification.Builder(this.getApplicationContext()).build();
        startForeground(BaseNotificationProvider.NOTIFICATION_ID_MEDIA, notification);
        return START_STICKY;
    }

    @Override
    public IBinder onBind(Intent intent) {
        return null;
    }

    private void registerContentObservers() {
        mediaObserver = new MediaObserver();

        getApplicationContext().getContentResolver().registerContentObserver(
                MediaStore.Images.Media.EXTERNAL_CONTENT_URI, false, mediaObserver);

        getApplicationContext().getContentResolver().registerContentObserver
                (MediaStore.Video.Media.EXTERNAL_CONTENT_URI, false, mediaObserver);

        // Log.i(DEBUG_TAG, "Started watchting for new media content.");
    }

    private void unregisterContentObservers() {
        this.getApplicationContext().getContentResolver()
                .unregisterContentObserver(mediaObserver);

        // Log.i(DEBUG_TAG, "Stopped watchting for new media content.");
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
                // Log.d(DEBUG_TAG, "Noticed a change in the media provider, scheduling sync.");
                cameraManager.performSync();
            }
        }
    }
}
