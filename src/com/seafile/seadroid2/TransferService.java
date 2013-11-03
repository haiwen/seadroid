package com.seafile.seadroid2;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import android.app.Service;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.SharedPreferences;
import android.content.SharedPreferences.Editor;
import android.database.ContentObserver;
import android.database.Cursor;
import android.net.ConnectivityManager;
import android.net.NetworkInfo;
import android.net.Uri;
import android.os.Binder;
import android.os.IBinder;
import android.provider.MediaStore;
import android.provider.MediaStore.MediaColumns;
import android.support.v4.content.LocalBroadcastManager;
import android.util.Log;
import android.telephony.PhoneStateListener;
import android.telephony.TelephonyManager;
import android.util.Log;
import android.util.SparseArray;

import com.seafile.seadroid2.TransferManager.DownloadTaskInfo;
import com.seafile.seadroid2.TransferManager.TransferListener;
import com.seafile.seadroid2.TransferManager.UploadTaskInfo;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.AccountManager;

public class TransferService extends Service implements TransferListener {

	/**
	 * This observer detects changes in the media store
	 * @author kread, inspired by http://www.jessechen.net/blog/how-does-google-plus-instant-upload-work/
	 *
	 */
	private class MediaObserver extends ContentObserver {

		private static final String DEBUG_TAG = "MediaObserver";

		public MediaObserver() {
			super(null);
		}

		@Override
		public void onChange(boolean selfChange) {
			super.onChange(selfChange);

			// This should never happen, but just in case...
			if (!isAutoUploadEnabled()) {
				return;
			}
			Media media = readFromMediaStore(getApplicationContext(),
					MediaStore.Images.Media.EXTERNAL_CONTENT_URI);
			Log.d(DEBUG_TAG, "detected picture");
			if (media == null) {
				return;
			}

			// Add to queue
			synchronized (autoUploadQueue) {
				autoUploadWaiting.add(media);
			}

			if (Utils.isFastConnectionOn()) {
				// And, if we are online, add the task immediately
				media.busy = true;
				media.taskId = addUploadTask(autoUploadAccount, autoUploadRepoId, autoUploadRepoName,
						autoUploadPath, media.getPath(), false);
				synchronized (autoUploadQueue) {
					autoUploadQueue.append(media.taskId, media);
				}
			}
		}
	}
	
	/**
	 * This BroadcastReceiver is called whenever connectivity changes. This is used in conjunction with the
	 * PhoneStateListener to tell when we have a fast connection again.
	 * @author kread
	 *
	 */
	public class WifiReceiver extends BroadcastReceiver {

		@Override
		public void onReceive(Context context, Intent intent) {
			final String action = intent.getAction();
			if (DEBUG) {
				Log.d(DEBUG_TAG, "Received BC: " + action);
			}
			if (action.equals(ConnectivityManager.CONNECTIVITY_ACTION)) {
				handleNetworkStateChanged((NetworkInfo) intent
						.getParcelableExtra(ConnectivityManager.EXTRA_NETWORK_INFO));
			}
		}
	}
	
	/**
	 * To determine if we are connected to 2G or a faster mobile network WifiReceiver is not enough, because the handover from GPRS to 3G doesn't
	 * change connectivity. Here we can detect this handover.
	 * @author kread
	 *
	 */
	public class FastPhoneStateListener extends PhoneStateListener {
		public void onDataConnectionStateChanged (int state, int networkType) {
			if (state == TelephonyManager.DATA_CONNECTED) {
				if (DEBUG) {
					Log.d (DEBUG_TAG, "New networkType " + networkType);
				}
				final boolean isFast = Utils.isConnectionFast(ConnectivityManager.TYPE_MOBILE, networkType);
				
				if (isFast && !lastConnectionState) {
					lastConnectionState = isFast;
					// If we went from offline to online, restart any uploads that are queued
					startUploads();
				}
			}
		}
	}

	private class Media {
		private String path;
		private String type;
		private int taskId = -1;
		public boolean busy;
		private int timestamp;

		public Media(String path, String type, int timestamp) {
			this.path = path;
			this.type = type;
			this.timestamp = timestamp;
		}

		public String getType() {
			return type;
		}

		public String getPath() {
			return path;
		}

		public int getTaskId() {
			return taskId;
		}

		public int getTimestamp() {
			return timestamp;
		}

		public void setTaskId(int taskId) {
			this.taskId = taskId;
		}
	}

	@SuppressWarnings("unused")
	private static final String DEBUG_TAG = "TransferService";
	private static final boolean DEBUG = true;

	public static final String BROADCAST_ACTION = "com.seafile.seadroid.TX_BROADCAST";
	public static final String INTENT_ACTION_UPLOADONLY = "com.seafile.android.AUTOUPLOAD_ONLY";

	private final IBinder mBinder = new TransferBinder();
	private TransferManager txManager;

	private String autoUploadRepoId;
	private String autoUploadRepoName;
	private String autoUploadPath;
	private Account autoUploadAccount;

	private MediaObserver mediaObserver;
	private AccountManager accountManager;
	/**
	 * The queue of all upload tasks
	 */
	private SparseArray<Media> autoUploadQueue = new SparseArray<Media>();
	
	/**
	 * The queue of media files found. These might not have tasks yet.
	 */
	private ArrayList<Media> autoUploadWaiting = new ArrayList<Media>();
	
	/**
	 * The timestamp of the last media file to be scanned (not uploaded)
	 */
	private int autoUploadScannedTS;

	/**
	 * The timestamp of the last media file to be succesfully uploaded
	 */
	private int autoUploadLastTS;
	private SharedPreferences settings;
	private WifiReceiver wifiReceiver;

	/**
	 * True if we are connected via a good connection at the moment
	 */
	private boolean lastConnectionState;

	public static final String BROADCAST_FILE_DOWNLOAD_SUCCESS = "downloaded";
	public static final String BROADCAST_FILE_DOWNLOAD_FAILED = "downloadFailed";
	public static final String BROADCAST_FILE_DOWNLOAD_PROGRESS = "downloadProgress";

	public static final String BROADCAST_FILE_UPLOAD_SUCCESS = "uploaded";
	public static final String BROADCAST_FILE_UPLOAD_FAILED = "uploadFailed";
	public static final String BROADCAST_FILE_UPLOAD_PROGRESS = "uploadProgress";
	public static final String BROADCAST_FILE_UPLOAD_CANCELLED = "uploadCancelled";

	public static final String PREF_CAMERA_UPLOAD_PATH = "path";
	public static final String PREF_CAMERA_UPLOAD_REPOID = "repoId";
	public static final String PREF_CAMERA_UPLOAD_REPONAME = "repoName";
	public static final String PREF_CAMERA_UPLOAD_SERVER = "server";
	public static final String PREF_CAMERA_UPLOAD_EMAIL = "email";
	public static final String PREF_CAMERA_UPLOAD_TS = "lastTimestamp";

	@Override
	public void onCreate() {
		txManager = new TransferManager();
		txManager.setListener(this);

		// Fetch camera upload preferences
		settings = getSharedPreferences("Upload", 0);
		autoUploadRepoId = settings.getString(PREF_CAMERA_UPLOAD_REPOID, null);
		autoUploadRepoName = settings.getString(PREF_CAMERA_UPLOAD_REPONAME, null);
		autoUploadPath = settings.getString(PREF_CAMERA_UPLOAD_PATH, null);
		final String autoUploadServerName = settings.getString(PREF_CAMERA_UPLOAD_SERVER, null);
		final String autoUploadServerEmail = settings.getString(PREF_CAMERA_UPLOAD_EMAIL, null);
		autoUploadLastTS = settings.getInt(PREF_CAMERA_UPLOAD_TS, (int)(System.currentTimeMillis() / 1000));

		accountManager = new AccountManager(this);

		// If we have stored a server name and email, try to fetch the account for it
		if (autoUploadServerName != null && autoUploadServerEmail != null) {
			autoUploadAccount = accountManager.getAccount(autoUploadServerName, autoUploadServerEmail);
		}

		mediaObserver = new MediaObserver();

		if (isAutoUploadEnabled()) {
			// We need the broadcast receivers if we are to detect new media files and connectivity changes
			registerAutoUploadObserver(true);
			checkForNewImages();
		}
	}

	@Override
	public void onDestroy() {
		stopWatchingWifi();
		txManager.unsetListener();
	}

	@Override
	public int onStartCommand(Intent intent, int flags, int startId) {
		final String action = intent.getAction();
		if (DEBUG) {
			Log.d (DEBUG_TAG, "onStartCommand. intent action " + action);
		}
		if (action != null && action.equals(INTENT_ACTION_UPLOADONLY)) {
			// If this is the action, the service was started on boot by AutoUploadStartReceiver.
			// If we don't do auto uploads, there is no reason for the service to be running now.
			if (!isAutoUploadEnabled()) {
				stopSelf();
				return START_NOT_STICKY;
			}
		}
		return START_STICKY;
	}

	public class TransferBinder extends Binder {
		public TransferService getService() {
			return TransferService.this;
		}
	}

	@Override
	public IBinder onBind(Intent intent) {
		// Log.d(DEBUG_TAG, "onBind");
		return mBinder;
	}

	/**
	 * Go through the media store and retrieve all new items that were added since we last
	 * succesfully completed an upload
	 */
	private void checkForNewImages() {
		ArrayList<Media> newItems = fetchFromMediaStore(getApplicationContext(),
				MediaStore.Images.Media.EXTERNAL_CONTENT_URI, autoUploadLastTS);
		if (newItems != null) {
			final boolean isOnline = Utils.isFastConnectionOn();

			synchronized (autoUploadWaiting) {
				for (Media item : newItems) {

					if (item == null) {
						continue;
					}
					
					// Add to the waiting queue
					autoUploadWaiting.add(item);

					// If we are online, we create the tasks immediately
					if (isOnline) {
						item.busy = true;
						item.taskId = addUploadTask(autoUploadAccount, autoUploadRepoId, autoUploadRepoName,
								autoUploadPath, item.getPath(), false);
						synchronized (autoUploadQueue) {
							autoUploadQueue.append(item.taskId, item);
						}
					}
				}
			}
		}
	}
	
	/**
	 * Retrieves the newest media file from media store
	 * @param context
	 * @param uri
	 * @return a Media object or null if none can be found
	 */
	private Media readFromMediaStore(Context context, Uri uri) {
		Cursor cursor = context.getContentResolver().query(uri, null, null, null, "date_added DESC");
		Media media = null;
		if (cursor.moveToNext()) {
			int dataColumn = cursor.getColumnIndexOrThrow(MediaColumns.DATA);
			String filePath = cursor.getString(dataColumn);
			int tsColumn = cursor.getColumnIndexOrThrow(MediaColumns.DATE_ADDED);
			int timestamp = Integer.parseInt(cursor.getString(tsColumn));
			int mimeTypeColumn = cursor.getColumnIndexOrThrow(MediaColumns.MIME_TYPE);
			String mimeType = cursor.getString(mimeTypeColumn);

			final File fileObj = new File(filePath);
			if (fileObj.exists() && autoUploadScannedTS <= timestamp) {
				media = new Media(filePath, mimeType, timestamp);
				autoUploadScannedTS = timestamp;
			}
		}
		cursor.close();
		return media;
	}

	/**
	 * Fetch all media files from store that were added after a given time stamp
	 * @param context
	 * @param uri
	 * @param lastUploadedTs unix timestamp in seconds
	 * @return a list of Media objects or null if none were found
	 */
	public ArrayList<Media> fetchFromMediaStore(Context context, Uri uri, int lastUploadedTs) {
		Cursor cursor = context.getContentResolver().query(uri, null, null, null, "date_added DESC");
		ArrayList<Media> items = null;
		while (cursor.moveToNext()) {
			int dataColumn = cursor.getColumnIndexOrThrow(MediaColumns.DATA);
			String filePath = cursor.getString(dataColumn);
			int tsColumn = cursor.getColumnIndexOrThrow(MediaColumns.DATE_ADDED);
			int timestamp = Integer.parseInt(cursor.getString(tsColumn));
			if (timestamp <= lastUploadedTs) {
				break;
			}
			int mimeTypeColumn = cursor.getColumnIndexOrThrow(MediaColumns.MIME_TYPE);
			String mimeType = cursor.getString(mimeTypeColumn);

			final File fileObj = new File(filePath);
			if (!fileObj.exists()) {
				continue;
			}
			if (autoUploadScannedTS > timestamp) {
				// Don't add to queue if we have already added a newer item once
				continue;
			}

			Media media = new Media(filePath, mimeType, timestamp);
			autoUploadScannedTS = timestamp;
			if (items == null) {
				items = new ArrayList<TransferService.Media>();
			}
			items.add(media);
		}
		cursor.close();
		return items;
	}
	
	/**
	 * Iterate through all waiting auto uploads if we are online. Start any new ones and restart failed ones.
	 */
	private void startUploads () {
		// Don't run on 2g
		if (!Utils.isFastConnectionOn()) {
			return;
		}
		synchronized (autoUploadWaiting) {
			for (Media item : autoUploadWaiting) {
				if (!item.busy) {
					// This media item never had a task created for it
					item.busy = true;
					item.taskId = addUploadTask(autoUploadAccount, autoUploadRepoId, autoUploadRepoName,
							autoUploadPath, item.getPath(), false);
					synchronized (autoUploadQueue) {
						autoUploadQueue.append(item.taskId, item);
					}
				} else {
					retryUploadTask(item.taskId);
				}
			}
		}
	}



	private void handleNetworkStateChanged(NetworkInfo info) {
		boolean connectivityChanged = false;
		if (DEBUG) {
			Log.d(DEBUG_TAG, "Connection before: " + lastConnectionState + " / " + info.getExtraInfo()
					+ " / " + info.getTypeName() + " / " + info.describeContents());
		}
		final boolean isConnectionGood = Utils.isFastConnectionOn();
		if (isConnectionGood != lastConnectionState) {
			connectivityChanged = true;
			lastConnectionState = isConnectionGood;
			if (DEBUG) {
				Log.d(DEBUG_TAG, "Connection changed: " + lastConnectionState + " / " + info.getExtraInfo()
					+ " / " + info.getTypeName() + " / " + info.describeContents());
			}
		}
		if (connectivityChanged && lastConnectionState) {
			// If we went from offline to online, restart any uploads that are queued
			startUploads();
		}
	}

	public void stopWatchingWifi() {
		if (wifiReceiver != null) {
			unregisterReceiver(wifiReceiver);
		}
		wifiReceiver = null;
		Log.d(DEBUG_TAG, "Stopped watching connectivity state");
	}

	/**
	 * Registers to receive the necessary Wi-Fi broadcasts.
	 */
	private void registerForWifiBroadcasts() {
		// Only add this callback once
		if (wifiReceiver == null) {
	        TelephonyManager telMgr = (TelephonyManager)
	                SeadroidApplication.getAppContext().getSystemService(
	                        Context.TELEPHONY_SERVICE);
	        
			telMgr.listen(new FastPhoneStateListener(), PhoneStateListener.LISTEN_DATA_CONNECTION_STATE);
		}
		IntentFilter intentFilter = new IntentFilter();
		
		intentFilter.addAction(ConnectivityManager.CONNECTIVITY_ACTION);
		wifiReceiver = new WifiReceiver();
		registerReceiver(wifiReceiver, intentFilter);
		lastConnectionState = Utils.isFastConnectionOn();
	}

	/**
	 * Register or unregister the observers for media store and connectivity changes
	 * @param register
	 */
	private void registerAutoUploadObserver(boolean register) {
		if (register) {
			this.getApplicationContext()
					.getContentResolver()
					.registerContentObserver(MediaStore.Images.Media.EXTERNAL_CONTENT_URI, false,
							mediaObserver);
			registerForWifiBroadcasts();
			Log.d(DEBUG_TAG, "Media observer registered");
		} else {
			this.getApplicationContext().getContentResolver().unregisterContentObserver(mediaObserver);
			Log.d(DEBUG_TAG, "Media observer registered");
			stopWatchingWifi();
		}
	}

	public boolean isAutoUploadEnabled() {
		return autoUploadPath != null && autoUploadRepoId != null && autoUploadAccount != null;
	}

	public String getAutoUploadPath() {
		return autoUploadPath;
	}

	public String getAutoUploadRepoId() {
		return autoUploadRepoId;
	}

	public Account getAutoUploadAccount() {
		return autoUploadAccount;
	}

	public void setTimeLastUpload(int ts) {
		if (autoUploadLastTS < ts) {
			autoUploadLastTS = ts;
			settings.edit().putInt(PREF_CAMERA_UPLOAD_TS, ts).commit();
		}
	}

	public void setAutoUploadData(String repoId, String repoName, String path, Account account) {

		final boolean wasEnabled = isAutoUploadEnabled();

		autoUploadPath = path;
		autoUploadRepoId = repoId;
		autoUploadRepoName = repoName;
		autoUploadAccount = account;

		final Editor editor = settings.edit();
		editor.putString(PREF_CAMERA_UPLOAD_PATH, autoUploadPath);
		editor.putString(PREF_CAMERA_UPLOAD_REPOID, autoUploadRepoId);
		editor.putString(PREF_CAMERA_UPLOAD_REPONAME, autoUploadRepoName);
		if (account != null) {
			editor.putString(PREF_CAMERA_UPLOAD_EMAIL, account.getEmail());
			editor.putString(PREF_CAMERA_UPLOAD_SERVER, account.getServer());
		} else {
			editor.putString(PREF_CAMERA_UPLOAD_EMAIL, null);
			editor.putString(PREF_CAMERA_UPLOAD_SERVER, null);
		}
		editor.commit();

		final boolean isEnabled = isAutoUploadEnabled();

		if (wasEnabled != isEnabled) {
			// If we just enabled it, start the observer and upload any new images that were added since the last time
			registerAutoUploadObserver(isEnabled);
			if (isEnabled) {
				checkForNewImages();
			}
		}
	}

	public int addUploadTask(Account account, String repoID, String repoName, String dir, String filePath,
			boolean isUpdate) {
		return txManager.addUploadTask(account, repoID, repoName, dir, filePath, isUpdate);
	}

	public int addDownloadTask(Account account, String repoName, String repoID, String path) {
		return txManager.addDownloadTask(account, repoName, repoID, path);
	}

	public UploadTaskInfo getUploadTaskInfo(int taskID) {
		return txManager.getUploadTaskInfo(taskID);
	}

	public List<UploadTaskInfo> getAllUploadTaskInfos() {
		return txManager.getAllUploadTaskInfos();
	}

	public void removeUploadTask(int taskID) {
		txManager.removeUploadTask(taskID);
	}

	public void removeFinishedUploadTasks() {
		txManager.removeFinishedUploadTasks();
	}

	public void cancelUploadTask(int taskID) {
		txManager.cancelUploadTask(taskID);
	}

	public void retryUploadTask(int taskID) {
		txManager.retryUploadTask(taskID);
	}

	public DownloadTaskInfo getDownloadTaskInfo(int taskID) {
		return txManager.getDownloadTaskInfo(taskID);
	}

	@Override
	public void onFileUploadProgress(int taskID) {
		Intent localIntent = new Intent(BROADCAST_ACTION).putExtra("type", BROADCAST_FILE_UPLOAD_PROGRESS)
				.putExtra("taskID", taskID);
		LocalBroadcastManager.getInstance(this).sendBroadcast(localIntent);
	}

	@Override
	public void onFileUploaded(int taskID) {
		synchronized (autoUploadQueue) {
			final Media media = autoUploadQueue.get(taskID);
			media.busy = false;
			setTimeLastUpload(media.timestamp);
			autoUploadQueue.remove(taskID);
			if (media != null) {
				autoUploadWaiting.remove(media);
			}
		}
		Intent localIntent = new Intent(BROADCAST_ACTION).putExtra("type", BROADCAST_FILE_UPLOAD_SUCCESS)
				.putExtra("taskID", taskID);
		LocalBroadcastManager.getInstance(this).sendBroadcast(localIntent);
	}

	@Override
	public void onFileUploadCancelled(int taskID) {
		synchronized (autoUploadQueue) {
			final Media media = autoUploadQueue.get(taskID);
			if (media != null) {
				media.setTaskId(-1);
				media.busy = false;
				return;
			}
		}
		Intent localIntent = new Intent(BROADCAST_ACTION).putExtra("type", BROADCAST_FILE_UPLOAD_CANCELLED)
				.putExtra("taskID", taskID);
		LocalBroadcastManager.getInstance(this).sendBroadcast(localIntent);
	}

	@Override
	public void onFileUploadFailed(int taskID) {
		synchronized (autoUploadQueue) {
			final Media media = autoUploadQueue.get(taskID);
			if (media != null) {
				autoUploadQueue.remove(taskID);
				media.busy = false;
				return;
			}
		}
		Intent localIntent = new Intent(BROADCAST_ACTION).putExtra("type", BROADCAST_FILE_UPLOAD_FAILED)
				.putExtra("taskID", taskID);
		LocalBroadcastManager.getInstance(this).sendBroadcast(localIntent);
	}

	@Override
	public void onFileDownloadProgress(int taskID) {
		Intent localIntent = new Intent(BROADCAST_ACTION).putExtra("type", BROADCAST_FILE_DOWNLOAD_PROGRESS)
				.putExtra("taskID", taskID);
		LocalBroadcastManager.getInstance(this).sendBroadcast(localIntent);
	}

	@Override
	public void onFileDownloaded(int taskID) {
		Intent localIntent = new Intent(BROADCAST_ACTION).putExtra("type", BROADCAST_FILE_DOWNLOAD_SUCCESS)
				.putExtra("taskID", taskID);
		LocalBroadcastManager.getInstance(this).sendBroadcast(localIntent);
	}

	@Override
	public void onFileDownloadFailed(int taskID) {
		Intent localIntent = new Intent(BROADCAST_ACTION).putExtra("type", BROADCAST_FILE_DOWNLOAD_FAILED)
				.putExtra("taskID", taskID);
		LocalBroadcastManager.getInstance(this).sendBroadcast(localIntent);
	}

	public void cancelDownloadTask(int taskID) {
		txManager.cancelDownloadTask(taskID);
	}
}
