package com.seafile.seadroid2.enums;

public enum RefreshStatusEnum {
    /**
     * only load data from local db
     */
    ONLY_LOCAL(1),

    /**
     * local first, then remote, and refresh local db
     */
    LOCAL_THEN_REMOTE(2),

    /**
     * only load data from remote, and refresh local db
     */
    ONLY_REMOTE(3);


    RefreshStatusEnum(int i) {

    }
}
