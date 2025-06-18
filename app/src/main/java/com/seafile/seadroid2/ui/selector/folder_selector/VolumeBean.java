package com.seafile.seadroid2.ui.selector.folder_selector;

public class VolumeBean {
    private String path;
    private String description;
    private boolean isSelected;

    public VolumeBean(String path, String description) {
        this.path = path;
        this.description = description;
        this.isSelected = false;
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

    public boolean isSelected() {
        return isSelected;
    }

    public void setSelected(boolean selected) {
        isSelected = selected;
    }
}
