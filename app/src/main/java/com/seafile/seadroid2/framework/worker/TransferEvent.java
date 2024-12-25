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
     * transferring
     */
    public static final String EVENT_TRANSFERRING = "transferring";

    /**
     * A single file was successfully transferred
     */
    public static final String EVENT_TRANSFER_SUCCESS = "transfer_success";

    /**
     * A single file transfer failed
     */
    public static final String EVENT_TRANSFER_FAILED = "transfer_failed";

    /**
     * The transfer worker is complete.<br/>
     * <b>NOTICE: This event will be sent in Result.success(data) instead of in setProgressAsync()<b/>
     */
    public static final String EVENT_FINISH = "transfer_finish_with_data";

    /**
     * The transfer task is complete and no data is uploaded
     */
    public static final String EVENT_FINISH_WITHOUT_DATA = "transfer_finish_without_data";

    /**
     * because of an OUT_OF_QUOTA error, the current upload worker was canceled
     */
    public static final String EVENT_CANCEL_WITH_OUT_OF_QUOTA = "transfer_cancel_with_out_of_quota";

    /**
     * because of an NETWORK error, the current upload worker was canceled
     */
    public static final String EVENT_CANCEL_WITH_NETWORK_ERR = "transfer_cancel_with_network_err";

    /**
     * Manual or passive cancellation
     */
    public static final String EVENT_CANCEL_WITH_BY_STOPPED = "transfer_cancel_with_stopped";

}
