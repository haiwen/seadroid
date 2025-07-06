package com.seafile.seadroid2.framework.worker;

public class TransferEvent {

    // scan
    public static final String EVENT_SCANNING = "scanning";
    public static final String EVENT_SCAN_COMPLETE = "scan_end";

    /**
     * A file is being transferred to the target platform
     */
    public static final String EVENT_FILE_IN_TRANSFER = "file_in_transfer";

    /**
     * A single file was successfully transferred
     */
    public static final String EVENT_FILE_TRANSFER_SUCCESS = "file_transfer_success";

    /**
     * A single file transfer failed
     */
    public static final String EVENT_FILE_TRANSFER_FAILED = "file_transfer_failed";

    /**
     * The transfer worker is start.
     */
    public static final String EVENT_TRANSFER_TASK_START = "transfer_task_start";

    /**
     * The transfer worker is complete.
     */
    public static final String EVENT_TRANSFER_TASK_COMPLETE = "transfer_task_complete";

    /**
     * The transfer worker is cancelled.
     */
    public static final String EVENT_TRANSFER_TASK_CANCELLED = "transfer_task_cancelled";
}
