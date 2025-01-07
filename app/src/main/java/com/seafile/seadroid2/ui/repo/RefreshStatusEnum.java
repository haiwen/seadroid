package com.seafile.seadroid2.ui.repo;

public enum RefreshStatusEnum {
    /**
     * In other ways, this is also a transmit frequency controller
     */
    NO(0),
    /**
     * only refresh data from local db
     */
    ONLY_LOCAL(1),

    /**
     * local first, then remote
     */
    LOCAL_BEFORE_REMOTE(2),
    REMOTE(3);


    RefreshStatusEnum(int i) {

    }
}
