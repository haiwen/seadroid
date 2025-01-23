package com.seafile.seadroid2.enums;

public enum RefreshStatusEnum {
    /**
     * No load data, in other ways, this is also a transmit frequency controller
     */
    NO(0),

    /**
     * only load data from local db
     */
    ONLY_LOCAL(1),

    /**
     * local first, then remote, and refresh local db
     */
    LOCAL_BEFORE_REMOTE(2),

    /**
     * only load data from remote, and refresh local db
     */
    REMOTE(3);


    RefreshStatusEnum(int i) {

    }
}
