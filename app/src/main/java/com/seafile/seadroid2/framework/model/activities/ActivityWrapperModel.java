package com.seafile.seadroid2.framework.model.activities;

import java.util.List;
import com.google.gson.annotations.SerializedName;

public class ActivityWrapperModel {
    @SerializedName("events")
    public List<ActivityModel> events;
}
