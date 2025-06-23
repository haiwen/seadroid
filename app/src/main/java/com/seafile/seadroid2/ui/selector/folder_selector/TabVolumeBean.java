package com.seafile.seadroid2.ui.selector.folder_selector;


import com.seafile.seadroid2.enums.ItemPositionEnum;

public class TabVolumeBean {

    private String filePath;
    private String fileName;
    private ItemPositionEnum itemPosition;

    public TabVolumeBean(String filePath, String fileName) {
        this.filePath = filePath;
        this.fileName = fileName;
    }

    public TabVolumeBean(String filePath, String fileName, ItemPositionEnum itemPosition) {
        this.filePath = filePath;
        this.fileName = fileName;
        this.itemPosition = itemPosition;
    }

    public String getFilePath() {
        return filePath;
    }

    public String getFileName() {
        return fileName;
    }
    public ItemPositionEnum getItemPosition() {
        return itemPosition;
    }

    public void setItemPosition(ItemPositionEnum itemPosition) {
        this.itemPosition = itemPosition;
    }
}
