package com.seafile.seadroid2.framework.model.dirents;

import androidx.annotation.Nullable;
import androidx.room.Embedded;

import com.seafile.seadroid2.framework.db.entities.DirentModel;
import com.google.gson.annotations.SerializedName;

public class CachedDirentModel {
    @SerializedName("dirent")
    @Embedded
    public DirentModel dirent;

    @SerializedName("local_file_id")
    @Nullable
    public String local_file_id;
}
