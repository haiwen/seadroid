package com.seafile.seadroid2.framework.model;

public class StorageInfo {
    public String path;           // 真实路径
    public String label;          // 用户可读的标签，如“内部存储”“SD 卡”“U盘”
    public boolean isRemovable;   // 是否是可移除存储（如 SD 卡、OTG）
    public boolean isPrimary;     // 是否为主存储（如 /storage/emulated/0）
    public boolean isAvailable;     // 是否可用
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
