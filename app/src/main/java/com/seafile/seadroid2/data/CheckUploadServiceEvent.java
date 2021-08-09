package com.seafile.seadroid2.data;

public class CheckUploadServiceEvent {
    private String logInfo;
    private String state;

    public CheckUploadServiceEvent(String logInfo) {
        this.logInfo = logInfo;
    }

    public CheckUploadServiceEvent(String state, String logInfo) {
        this.state = state;
        this.logInfo = logInfo;
    }

    public void setLogInfo(String logInfo) {
        this.logInfo = logInfo;
    }

    public String getLogInfo() {
        return logInfo;
    }

    public String getState() {
        return state;
    }
}

