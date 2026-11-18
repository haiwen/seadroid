package com.seafile.seadroid2.framework.service;

/**
 * One request awaiting startup. Only one request for the same service will be retained;
 * subsequent requests will be merged based on fields.
 */
final class StartRequest {
    private boolean fullScan;
    private boolean resetModels;

    StartRequest(boolean fullScan, boolean resetModels) {
        this.resetModels = resetModels;
        // After clearing the models to be transferred, a full rescan is necessary;
        // otherwise, the incremental window will miss earlier files.
        this.fullScan = fullScan || resetModels;
    }

    void merge(StartRequest other) {
        resetModels |= other.resetModels;
        fullScan |= other.fullScan || other.resetModels;
    }

    boolean isFullScan() {
        return fullScan;
    }

    boolean isResetModels() {
        return resetModels;
    }
}
