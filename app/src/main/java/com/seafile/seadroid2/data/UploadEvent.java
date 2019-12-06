package com.seafile.seadroid2.data;

public class UploadEvent {
    private int tagcode;
    private String loginfo;
    private int waitingNum;
    private int total_number;

    public UploadEvent(int tagcode, String loginfo) {
        this.tagcode = tagcode;
        this.loginfo = loginfo;
    }

    public UploadEvent(int tagcode, int waitingNum, int total_number, String loginfo) {
        this.tagcode = tagcode;
        this.waitingNum = waitingNum;
        this.total_number = total_number;
        this.loginfo = loginfo;
    }

    public void setTotal_number(int total_number) {
        this.total_number = total_number;
    }

    public int getTotal_number() {
        return total_number;
    }

    public void setTagcode(int tagcode) {
        this.tagcode = tagcode;
    }

    public int getTagcode() {
        return tagcode;
    }

    public void setLoginfo(String loginfo) {
        this.loginfo = loginfo;
    }

    public String getLoginfo() {
        return loginfo;
    }

    public void setWaitingNum(int waitingNum) {
        this.waitingNum = waitingNum;
    }

    public int getWaitingNum() {
        return waitingNum;
    }
}
