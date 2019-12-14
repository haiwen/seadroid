package com.seafile.seadroid2.data;

public class CheckUploadServiceEvent {
    private String logInfo;

    public CheckUploadServiceEvent(String logInfo) {
        this.logInfo = logInfo;
    }

    public void setLogInfo(String logInfo) {
        this.logInfo = logInfo;
    }

    public String getLogInfo() {
        return logInfo;
    }

}

