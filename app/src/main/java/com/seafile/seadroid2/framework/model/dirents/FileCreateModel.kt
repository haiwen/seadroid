package com.seafile.seadroid2.framework.model.dirents

class FileCreateModel {
    @JvmField
    var error_msg: String? = null

    @JvmField
    var can_edit: Boolean = false

    @JvmField
    var can_preview: Boolean = false

    @JvmField
    var is_locked: Boolean = false

    @JvmField
    var mtime: String? = null

    @JvmField
    var obj_id: String? = null

    @JvmField
    var obj_name: String? = null

    @JvmField
    var parent_dir: String? = null

    @JvmField
    var repo_id: String? = null

    @JvmField
    var size: Long = 0

    @JvmField
    var type: String? = null
}
