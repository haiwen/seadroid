package com.seafile.seadroid2.ui.selector.folder_selector;

public class VolumeBean {
    private String path;
    private String description;

    public VolumeBean(String path, String description) {
        this.path = path;
        this.description = description;
    }

    public String getPath() {
        return path;
    }

    public void setPath(String path) {
        this.path = path;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }
}
