package com.seafile.seadroid2.util;

public class CameraSyncStatus {

    /**
     * judge scan  service is alive
     */
    public static int IS_SERVICE = 1;
    /**
     * The gallery has changed
     */
    public static int SCAN_START = 2;
    /**
     * Start upload
     */
    public static int ADD_TASK_QUE = 3;
    /**
     * End of the upload
     */
    public static int SCAN_END = 4;
    /**
     * network available
     */
    public static int NETWORK_AVAILABLE = 5;

    public static int STATUS_DEFAULT = 0;

}
