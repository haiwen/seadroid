package com.seafile.seadroid2.framework.model.dirents

import android.text.TextUtils
import com.seafile.seadroid2.framework.util.Utils

class DirentRecursiveFileModel {
    @JvmField
    var name: String? = null

    @JvmField
    var parent_dir: String? = null

    /**
     * Normalizes [parent_dir] to ensure it is non-null, non-empty and ends with "/".
     */
    fun getParent_dir(): String {
        var normalized = parent_dir
        if (normalized.isNullOrEmpty()) {
            normalized = "/"
        }

        if (!normalized.endsWith("/")) {
            normalized += "/"
        }

        parent_dir = normalized
        return normalized
    }

    /**
     * Net data id (file_id/folder_id). Empty file uses 40 zeros.
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

    // lock status
    @JvmField
    var is_locked: Boolean = false

    @JvmField
    var locked_by_me: Boolean = false

    @JvmField
    var lock_time: Long = 0

    @JvmField
    var lock_owner: String? = null

    @JvmField
    var modifier_name: String? = null

    @JvmField
    var modifier_contact_email: String? = null

    fun isDir(): Boolean = TextUtils.equals(type, "dir")

    fun getFullFileName(): String? = Utils.pathJoin(parent_dir, name)
}
