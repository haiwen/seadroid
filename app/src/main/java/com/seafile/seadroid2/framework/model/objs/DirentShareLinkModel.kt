package com.seafile.seadroid2.framework.model.objs

import com.seafile.seadroid2.framework.model.dirents.DirentPermissionModel

class DirentShareLinkModel {
    @JvmField
    var username: String? = null

    @JvmField
    var repo_id: String? = null

    @JvmField
    var repo_name: String? = null

    @JvmField
    var path: String? = null

    @JvmField
    var password: String? = null

    @JvmField
    var obj_name: String? = null

    @JvmField
    var is_dir: Boolean = false

    @JvmField
    var token: String? = null

    @JvmField
    var link: String? = null

    @JvmField
    var view_count: Int = 0

    @JvmField
    var ctime: String? = null

    @JvmField
    var expire_date: String? = null

    @JvmField
    var is_expired: Boolean = false

    @JvmField
    var permissions: DirentPermissionModel? = null
}
