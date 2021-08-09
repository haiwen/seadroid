package com.seafile.seadroid2.data;

public class SaveEditFileEvent {
    private String path;

    public SaveEditFileEvent(String path) {
        this.path = path;
    }

    public String getPath() {
        return path;
    }

}
