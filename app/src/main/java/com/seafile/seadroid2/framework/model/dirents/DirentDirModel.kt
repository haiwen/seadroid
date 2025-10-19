package com.seafile.seadroid2.framework.model.dirents

import com.seafile.seadroid2.framework.model.ResultModel

class DirentDirModel : ResultModel() {
    @JvmField
    var repo_id: String? = null

    @JvmField
    var path: String? = null

    @JvmField
    var name: String? = null

    @JvmField
    var mtime: Long = 0

    @JvmField
    var permission: String? = null
}
