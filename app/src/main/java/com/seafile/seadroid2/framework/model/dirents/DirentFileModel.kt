package com.seafile.seadroid2.framework.model.dirents

import com.seafile.seadroid2.framework.model.ResultModel

class DirentFileModel() : ResultModel() {
    @JvmField
    var name: String? = null

    /**
     * Net data id (file_id/folder_id). Empty files use 40 zeros.
     */
    @JvmField
    var id: String? = null

    @JvmField
    var type: String? = null

    /**
     * Last modified timestamp (seconds).
     */
    @JvmField
    var mtime: Long = 0

    @JvmField
    var permission: String? = null

    /**
     * Size of file; 0 if directory.
     */
    @JvmField
    var size: Long = 0

    @JvmField
    var starred: Boolean = false

    @JvmField
    var can_edit: Boolean = false

    @JvmField
    var comment_total: Int = 0

    @JvmField
    var lock_owner: String? = null

    @JvmField
    var last_modified: String? = null

    @JvmField
    var last_modifier_email: String? = null

    @JvmField
    var last_modifier_name: String? = null

    @JvmField
    var last_modifier_contact_email: String? = null

    fun getMtimeInMills(): Long = mtime * 1000

    constructor(errMsg: String) : this() {
        error_msg = errMsg
    }
}
