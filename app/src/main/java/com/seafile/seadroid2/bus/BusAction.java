package com.seafile.seadroid2.bus;

/**
 * see also {@link com.seafile.seadroid2.ui.main.MainActivity#notifyMountChanged(String, String)}.
 *
 * @see android.content.Intent#ACTION_MEDIA_MOUNTED
 * @see android.content.Intent#ACTION_MEDIA_UNMOUNTED
 * @see android.content.Intent#ACTION_MEDIA_REMOVED
 */
public class BusAction {
    public static final String RESTART_FILE_MONITOR = "RESTART_FILE_MONITOR";
    public static final String PERIODIC_ALBUM_SCAN_LAUNCH = "PERIODIC_ALBUM_SCAN_LAUNCH";
    public static final String PERIODIC_FOLDER_SCAN_LAUNCH = "PERIODIC_FOLDER_SCAN_LAUNCH";

}
