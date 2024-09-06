package com.seafile.seadroid2.enums;

public enum TransferStatus {

    /**
     * waiting
     */
    WAITING,

    /**
     * Upload currently in progress or scheduled to be executed.
     */
    IN_PROGRESS,

    /**
     * Last upload failed.
     */
    FAILED,

    /**
     * Upload was successful.
     */
    SUCCEEDED,

    /**
     * Upload was cancelled by the user.
     */
    CANCELLED;

}
