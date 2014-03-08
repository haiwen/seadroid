package com.seafile.seadroid2;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.util.Log;

public class AutoUploadStartReceiver extends BroadcastReceiver {
	
	private static final String DEBUG_TAG = "AutoUploadStartReceiver";

	@Override
	public void onReceive(Context context, Intent intent) {
		Intent serverIntent = new Intent(context, TransferService.class);
		serverIntent.setAction(TransferService.INTENT_ACTION_UPLOADONLY);
		context.startService(serverIntent);
		Log.i(DEBUG_TAG, "started");
	}

}
