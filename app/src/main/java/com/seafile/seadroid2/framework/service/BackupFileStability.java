package com.seafile.seadroid2.framework.service;

import java.io.File;
import java.util.function.LongConsumer;

/** Best-effort quiet-window check, not a filesystem lock. Run only on an IO executor. */
public final class BackupFileStability {
    private BackupFileStability() { }

    public static boolean awaitStable(File file) {
        return awaitStable(file, ExecutionCancellation::await);
    }

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
