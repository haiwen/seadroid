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
    public static final String START_FOREGROUND_FILE_MONITOR = "START_FOREGROUND_FILE_MONITOR";
    public static final String STOP_FOREGROUND_FILE_MONITOR = "STOP_FOREGROUND_FILE_MONITOR";


}
