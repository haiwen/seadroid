package com.seafile.seadroid2.framework.data.model;

import androidx.annotation.StringRes;

import com.seafile.seadroid2.SeadroidApplication;

public class GroupItemModel extends BaseModel {
    @StringRes
    private int name;

    public String title;

    public GroupItemModel() {

    }

    public GroupItemModel(@StringRes int nameRes) {
        this.name = nameRes;

        title = SeadroidApplication.getInstance().getString(name);
    }

    public GroupItemModel(String title) {
        this.title = title;
    }
}
