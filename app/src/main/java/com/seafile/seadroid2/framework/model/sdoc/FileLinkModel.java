package com.seafile.seadroid2.framework.model.sdoc;

import android.os.Parcel;
import android.os.Parcelable;
import com.google.gson.annotations.SerializedName;

public class FileLinkModel {
    @SerializedName("display_value")
    public String display_value;
    @SerializedName("row_id")
    public String row_id;
}
