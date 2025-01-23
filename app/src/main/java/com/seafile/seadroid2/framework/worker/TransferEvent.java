package com.seafile.seadroid2.framework.worker;

public class TransferEvent {
    /**
     * scanning
     */
    public static final String EVENT_SCANNING = "scanning";

    /**
     * a scan-end event<br/>
     * <b>NOTICE: This event will be sent in Result.success(data) instead of in setProgressAsync()<b/>
     */
    public static final String EVENT_SCAN_END = "scan_end";

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
     * The transfer worker is complete.<br/>
     * <b>NOTICE: This event will be sent in Result.success(data) instead of in setProgressAsync()<b/>
     */
    public static final String EVENT_FINISH = "transfer_finish";
}
