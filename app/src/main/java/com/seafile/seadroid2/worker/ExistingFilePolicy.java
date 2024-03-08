package com.seafile.seadroid2.worker;

/**
 * for FILE SYNC FEAT in future.
 */
public enum ExistingFilePolicy {
    APPEND,
    REPLACE_REMOTE_FILE,
    REPLACE_LOCAL_FILE,

    /**
     * DO NOTHING
     */
    KEEP
}
