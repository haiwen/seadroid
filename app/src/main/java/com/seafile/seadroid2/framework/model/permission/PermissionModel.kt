package com.seafile.seadroid2.framework.model.permission

class PermissionModel {
    @JvmField
    var create: Boolean = false

    @JvmField
    var upload: Boolean = false

    @JvmField
    var download: Boolean = false

    @JvmField
    var preview: Boolean = false

    @JvmField
    var copy: Boolean = false

    @JvmField
    var delete: Boolean = false

    @JvmField
    var modify: Boolean = false

    @JvmField
    var download_external_link: Boolean = false
}
