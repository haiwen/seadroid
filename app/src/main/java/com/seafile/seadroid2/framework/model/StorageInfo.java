package com.seafile.seadroid2.framework.model;

public class StorageInfo {
    public String path;           // 真实路径
    public String label;          // 用户可读的标签，如“内部存储”“SD 卡”“U盘”
    public boolean isRemovable;   // 是否是可移除存储（如 SD 卡、OTG）
    public boolean isPrimary;     // 是否为主存储（如 /storage/emulated/0）
    public Type type;

    public StorageInfo(String path, String label, boolean isRemovable, boolean isPrimary) {
        this.path = path;
        this.label = label;
        this.isRemovable = isRemovable;
        this.isPrimary = isPrimary;
    }

    public enum Type {
        INTERNAL, SD_CARD, OTG, UNKNOWN
    }
}
