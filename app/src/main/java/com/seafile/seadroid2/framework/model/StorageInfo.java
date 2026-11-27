package com.seafile.seadroid2.framework.model;

import com.google.gson.annotations.SerializedName;

public class StorageInfo {
    @SerializedName("path")
    public String path;           // real path
    @SerializedName("label")
    public String label;          // user-readable label, e.g. "Internal storage", "SD card", "USB drive"
    @SerializedName("isRemovable")
    public boolean isRemovable;   // whether it is removable storage (e.g. SD card, OTG)
    @SerializedName("isPrimary")
    public boolean isPrimary;     // whether it is primary storage (e.g. /storage/emulated/0)
    @SerializedName("isAvailable")
    public boolean isAvailable;     // whether it is available
    @SerializedName("type")
    public Type type;

    public StorageInfo() {

    }


    public String getPath() {
        return path;
    }

    public void setPath(String path) {
        this.path = path;
    }

    public String getLabel() {
        return label;
    }

    public void setLabel(String label) {
        this.label = label;
    }

    public boolean isRemovable() {
        return isRemovable;
    }

    public void setRemovable(boolean removable) {
        isRemovable = removable;
    }

    public boolean isPrimary() {
        return isPrimary;
    }

    public void setPrimary(boolean primary) {
        isPrimary = primary;
    }

    public boolean isAvailable() {
        return isAvailable;
    }

    public void setAvailable(boolean available) {
        isAvailable = available;
    }

    public Type getType() {
        return type;
    }

    public void setType(Type type) {
        this.type = type;
    }

    public enum Type {
        INTERNAL, SD_CARD, OTG, UNKNOWN
    }
}
