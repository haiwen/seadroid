package com.seafile.seadroid2.enums;

public enum RefreshStatusEnum {
    /** Loads the cached data only; no network request is made. */
    ONLY_LOCAL,

    /** Displays cached data first, then refreshes it from the server when connected. */
    LOCAL_THEN_REMOTE,

    /** Refreshes directly from the server and updates the local cache. */
    ONLY_REMOTE
}
