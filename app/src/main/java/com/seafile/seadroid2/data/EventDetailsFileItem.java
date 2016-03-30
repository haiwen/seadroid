package com.seafile.seadroid2.data;

public class EventDetailsFileItem {

    public enum EType {
        FILE_ADDED,
        FILE_DELETED,
        FILE_MODIFIED,
        FILE_RENAMED,
        DIR_ADDED,
        DIR_DELETED
    }

    private String path;
    private EType eType;
    private SeafEvent event;

    public EventDetailsFileItem(SeafEvent event, String path, EType etype) {
        this.path = path;
        this.eType = etype;
        this.event = event;
    }

    public String getPath() {
        return path;
    }

    public void setPath(String path) {
        this.path = path;
    }

    public EType geteType() {
        return eType;
    }

    public SeafEvent getEvent() {
        return event;
    }

    public void setEvent(SeafEvent event) {
        this.event = event;
    }

    public boolean isFileOpenable() {
        return eType == EType.FILE_ADDED ||
                eType == EType.FILE_MODIFIED ||
                eType == EType.FILE_RENAMED ||
                eType == EType.DIR_ADDED;
    }

    public boolean isDir() {
        return eType == EType.DIR_ADDED;
    }

}
