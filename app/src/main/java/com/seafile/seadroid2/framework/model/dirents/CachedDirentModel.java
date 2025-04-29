package com.seafile.seadroid2.framework.model.dirents;

import androidx.annotation.Nullable;
import androidx.room.Embedded;

import com.seafile.seadroid2.framework.db.entities.DirentModel;

public class CachedDirentModel {
    @Embedded
    public DirentModel dirent;

    @Nullable
    public String local_file_id;
}
