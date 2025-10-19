package com.seafile.seadroid2.framework.model.repo

import com.seafile.seadroid2.framework.db.entities.DirentModel
import com.seafile.seadroid2.framework.model.ResultModel

class DirentWrapperModel : ResultModel() {
    @JvmField
    var user_perm: String? = null

    @JvmField
    var dir_id: String? = null

    @JvmField
    val dirent_list: MutableList<DirentModel> = ArrayList()
}
