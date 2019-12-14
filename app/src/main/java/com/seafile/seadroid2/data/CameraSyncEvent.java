package com.seafile.seadroid2.data;

public class CameraSyncEvent {
    private int tagCode;
    private String logInfo;
    private int waitingNumber;
    private int totalNumber;

//    public CameraSyncEvent(int tagCode, String logInfo) {
//        this.tagCode = tagCode;
//        this.logInfo = logInfo;
//    }

    public CameraSyncEvent(String logInfo) {
        this.logInfo = logInfo;
    }

    public CameraSyncEvent(int waitingNumber, int totalNumber, String logInfo) {
        this.waitingNumber = waitingNumber;
        this.totalNumber = totalNumber;
        this.logInfo = logInfo;
    }

    public void setLogInfo(String logInfo) {
        this.logInfo = logInfo;
    }

    public String getLogInfo() {
        return logInfo;
    }

    public void setTagCode(int tagCode) {
        this.tagCode = tagCode;
    }

    public int getTagCode() {
        return tagCode;
    }

    public void setTotalNumber(int totalNumber) {
        this.totalNumber = totalNumber;
    }

    public int getTotalNumber() {
        return totalNumber;
    }

    public void setWaitingNumber(int waitingNumber) {
        this.waitingNumber = waitingNumber;
    }

    public int getWaitingNumber() {
        return waitingNumber;
    }
}

