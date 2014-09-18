package com.seafile.seadroid2.sync;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.preference.PreferenceManager;
import android.util.Log;

import com.seafile.seadroid2.BrowserActivity;
/**
 * Listening operating system boot broadcast.
 * 
 * If user turned on camera upload service, then run the service. Otherwise, ignore the broadcast.   
 *
 */
public class OSBootReceiver extends BroadcastReceiver {
	private static final String DEBUG_TAG = "OSBootReceiver";

	@Override
	public void onReceive(Context context, Intent intent) {

		if (Intent.ACTION_BOOT_COMPLETED.equals(intent.getAction())) {

			Intent cameraUploadIntent = new Intent(context,
					CameraUploadService.class);
			SharedPreferences settings = PreferenceManager
					.getDefaultSharedPreferences(context);
			boolean isUploadStart = settings.getBoolean(
			        BrowserActivity.CAMERA_UPLOAD_SWITCH_KEY, false);
			if (!isUploadStart) {
			    return;
			}
			Log.d(DEBUG_TAG, "start service after OS boot");
			context.startService(cameraUploadIntent);
		}
	}

}
