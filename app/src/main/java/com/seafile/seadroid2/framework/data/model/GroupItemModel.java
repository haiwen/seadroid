package com.seafile.seadroid2.framework.data.model;

import androidx.annotation.StringRes;

public class GroupItemModel extends BaseModel {
    @StringRes
    public int name;

    public String title;

    public GroupItemModel() {

    }

    public GroupItemModel(@StringRes int nameRes) {
        this.name = nameRes;
    }

    public GroupItemModel(String title) {
        this.title = title;
    }
}
