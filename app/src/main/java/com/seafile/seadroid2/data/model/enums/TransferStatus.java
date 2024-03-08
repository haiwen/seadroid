package com.seafile.seadroid2.data.model.enums;

public enum TransferStatus {

    /**
     * waiting
     */
    TRANSFER_WAITING,
    /**
     * Upload currently in progress or scheduled to be executed.
     */
    TRANSFER_IN_PROGRESS,

    /**
     * Last upload failed.
     */
    TRANSFER_FAILED,

    /**
     * Upload was successful.
     */
    TRANSFER_SUCCEEDED,

    /**
     * Upload was cancelled by the user.
     */
    TRANSFER_CANCELLED;

}
