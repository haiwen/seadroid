package com.seafile.seadroid2.ui.repo.sort;

public class SortMenuItem {

    public String title;
    public boolean checked;
    public int menuId;

    public SortMenuItem(String title, boolean checked, int menuId) {
        this.title = title;
        this.checked = checked;
        this.menuId = menuId;
    }
}
