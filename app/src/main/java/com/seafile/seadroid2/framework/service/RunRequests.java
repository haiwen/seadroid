package com.seafile.seadroid2.framework.service;

/** Requests for one execution. The executor serializes access with its ownership lock. */
final class RunRequests {
    private boolean cancelled, retiring, repeat, fullScan;
    private Boolean restart;

    void trigger(boolean full) {
        if (cancelled || retiring) {
            restart = Boolean.TRUE.equals(restart) || full;
        } else {
            repeat = true;
            fullScan |= full;
        }
    }

    boolean takeRepeat() {
        if (cancelled || !repeat) {
            retiring = true;
            return false;
        }
        repeat = false;
        return true;
    }

    boolean isFullScan() { return fullScan; }
    boolean isCancelled() { return cancelled; }

    void cancel() {
        cancelled = true;
        repeat = false;
        // A later stop also cancels a previously requested next generation.
        restart = null;
    }

    Boolean takeRestart() {
        Boolean next = restart;
        restart = null;
        return next;
    }
}
