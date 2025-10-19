package com.seafile.seadroid2.framework.model.dirents

import androidx.room.Embedded
import com.seafile.seadroid2.framework.db.entities.DirentModel

class CachedDirentModel {
    @Embedded
    @JvmField
    var dirent: DirentModel? = null

    @JvmField
    var local_file_id: String? = null
}
