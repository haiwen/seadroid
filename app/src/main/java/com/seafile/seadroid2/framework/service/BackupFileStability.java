package com.seafile.seadroid2.framework.service;

import java.io.File;
import java.util.function.LongConsumer;

/**
 * Best-effort quiet-window check, not a filesystem lock. Run only on an IO executor.
 */
public final class BackupFileStability {
    private BackupFileStability() {
    }

    /**
     * Checks whether the given file is stable across a quiet-window delay.
     * <p>
     * Verifies that the file exists and that its size and last modified timestamp remain
     * unchanged before and after the wait period. Accepts a custom wait action for testability.
     *
     * @param file the target file to verify
     * @return true if the file existed and its metadata did not change during the wait; false otherwise
     */
    public static boolean awaitStable(File file) {
        return awaitStable(file, ExecutionCancellation::await);
    }

    /**
     * Checks whether the given file is stable across a quiet-window delay.
     * <p>
     * Verifies that the file exists and that its size and last modified timestamp remain
     * unchanged before and after the wait period. Accepts a custom wait action for testability.
     *
     * @param file the target file to verify
     * @param wait consumer accepting the wait duration in milliseconds
     * @return true if the file existed and its metadata did not change during the wait; false otherwise
     */
    static boolean awaitStable(File file, LongConsumer wait) {
        ExecutionCancellation.check();
        boolean existed = file.isFile();
        long size = file.length();
        long modified = file.lastModified();
        wait.accept(1000);
        ExecutionCancellation.check();
        return existed && file.isFile() && size == file.length() && modified == file.lastModified();
    }
}
