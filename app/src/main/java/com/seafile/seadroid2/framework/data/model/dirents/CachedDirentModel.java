package com.seafile.seadroid2.framework.data.model.dirents;

import androidx.annotation.Nullable;
import androidx.room.Embedded;

import com.seafile.seadroid2.framework.data.db.entities.DirentModel;
import com.seafile.seadroid2.framework.data.model.BaseModel;

public class CachedDirentModel {
    @Embedded
    public DirentModel dirent;

    @Nullable
    public String local_file_id;
}
