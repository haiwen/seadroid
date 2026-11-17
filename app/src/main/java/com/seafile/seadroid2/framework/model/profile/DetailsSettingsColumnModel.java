package com.seafile.seadroid2.framework.model.profile;

import java.util.List;
import com.google.gson.annotations.SerializedName;

public class DetailsSettingsColumnModel {
    @SerializedName("columns")
    public List<DetailsSettingsKeyModel> columns;
}
