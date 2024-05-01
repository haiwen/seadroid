package com.seafile.seadroid2.framework.worker;

/**
 * check file strategy when downloading file
 */
public enum ExistingFileStrategy {
    /**
     * Automatically check the target file
     */
    AUTO,

    /**
     * Ask the user how to deal with it
     * (DOWNLOAD)
     */
    ASK,

    /**
     * doesn't exist in local
     */
    APPEND,

    /**
     * overwrite remote file
     */
    REPLACE,

    /**
     * KEEP BOTH, file will be renamed
     */
    KEEP,

    /**
     * skip file, because they are the same file
     */
    SKIP,

    /**
     * not exists in remote repo (may have been deleted).
     * if DOWNLOAD, file will not be downloaded.
     * if UPLOAD, file will be uploaded.
     */
    NOT_FOUND_IN_REMOTE
}
