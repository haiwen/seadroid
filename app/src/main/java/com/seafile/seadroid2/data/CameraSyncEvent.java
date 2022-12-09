package com.seafile.seadroid2.data;

public class CameraSyncEvent {
    private String logInfo;
    private int num;

    public CameraSyncEvent(String logInfo) {
        this.logInfo = logInfo;
    }

    public CameraSyncEvent(String logInfo, int number) {
        this.logInfo = logInfo;
        this.num = number;
    }

    public int getNum() {
        return num;
    }

    public String getLogInfo() {
        return logInfo;
    }

}

