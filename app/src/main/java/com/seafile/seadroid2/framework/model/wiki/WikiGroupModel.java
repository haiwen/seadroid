package com.seafile.seadroid2.framework.model.wiki;

import androidx.annotation.DrawableRes;
import androidx.annotation.StringRes;

import com.blankj.utilcode.util.CollectionUtils;
import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.framework.db.entities.RepoModel;
import com.seafile.seadroid2.framework.model.BaseModel;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

public class WikiGroupModel extends BaseModel {
    public String title;
    public int icon;

    public WikiGroupModel(String title, @DrawableRes int icon) {
        this.title = title;
        this.icon = icon;
    }

    public WikiGroupModel(@StringRes int nameRes, @DrawableRes int icon) {
        title = SeadroidApplication.getAppString(nameRes);
        this.icon = icon;
    }

    public String getTitle() {
        return title;
    }

    public int getIcon() {
        return icon;
    }
}
