package com.seafile.seadroid2;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.os.Build;
import android.util.Log;

import com.seafile.seadroid2.cameraupload.MediaObserverService;

/**
 * This receiver is called whenever the system has booted or
 * the Seadroid app has been upgraded to a new version.
 * It can be used to start up background services.
 */
public class BootAutostart extends BroadcastReceiver {
    private static final String DEBUG_TAG = "BootAutostart";

    /**
     * This method will be excecuted after
     * - booting the device
     * - upgrade of the Seadroid package
     */
    public void onReceive(Context context, Intent intent) {
        Log.i(DEBUG_TAG, "Registering for MediaProvider changes.");

        Intent mediaObserver = new Intent(context, MediaObserverService.class);
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
            context.startForegroundService(mediaObserver);
        } else {
            context.startService(mediaObserver);
        }
    }

}
