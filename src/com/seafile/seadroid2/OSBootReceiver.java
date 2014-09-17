package com.seafile.seadroid2;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.preference.PreferenceManager;
import android.util.Log;
import android.widget.Toast;

import com.seafile.seadroid2.sync.CameraUploadService;
/**
 * listening operating system boot broadcast.
 * 
 * If user turned on camera upload service, then run the service. Otherwise, ignore the broadcast.   
 *
 */
public class OSBootReceiver extends BroadcastReceiver {
	private static final String DEBUG_TAG = "OSBootReceiver";

	@Override
	public void onReceive(Context context, Intent intent) {

		if (Intent.ACTION_BOOT_COMPLETED.equals(intent.getAction())) {
			Toast.makeText(context, "boot completed", Toast.LENGTH_SHORT).show();
			Log.d(DEBUG_TAG, "boot to notic receiver");

			Intent cameraUploadIntent = new Intent(context,
					CameraUploadService.class);
			SharedPreferences settings = PreferenceManager
					.getDefaultSharedPreferences(context);
			boolean isUploadStart = settings.getBoolean(
					BrowserActivity.CAMERA_UPLOAD_SWITCH_KEY, false);
			if (!isUploadStart) {
				return;
			}
			Log.d(DEBUG_TAG, "boot to start service");
			context.startService(cameraUploadIntent);
		}
	}

}
