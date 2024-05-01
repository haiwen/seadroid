package com.seafile.seadroid2.framework.worker;

public class TransferEvent {
    public static final String EVENT_SCANNING = "scanning";
    public static final String EVENT_SCAN_END = "scan_end";
    public static final String EVENT_TRANSFERRING = "transferring";
    public static final String EVENT_TRANSFERRED_WITH_DATA = "transferred_with_data";
    public static final String EVENT_TRANSFERRED_WITHOUT_DATA = "transferred_without_data";

    public static final String EVENT_NOT_TRANSFERRED = "not_transferred";
    public static final String EVENT_CANCEL_OUT_OF_QUOTA = "transfer_cancel_with_out_of_quota";
}
