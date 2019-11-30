package com.seafile.seadroid2.data;

public class UploadEvent {
    private int tagcode;
    private String loginfo;
    private int waitingNum;
    private String totalnum;

    public UploadEvent(int tagcode, String loginfo) {
        this.tagcode = tagcode;
        this.loginfo = loginfo;
    }

    public UploadEvent(int tagcode, int number, String loginfo) {
        this.tagcode = tagcode;
        this.waitingNum = number;
        this.loginfo = loginfo;
    }

    public void setTotalnum(String totalnum) {
        this.totalnum = totalnum;
    }

    public String getTotalnum() {
        return totalnum;
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
