package com.seafile.seadroid2.framework.model

class ContextModel {
    @JvmField
    var repo_id: String? = null

    @JvmField
    var repo_name: String? = null

    @JvmField
    var type: String? = null

    /**
     * parent_dir + name
     */
    @JvmField
    var full_path: String? = null

    @JvmField
    var permission: String? = null

    @JvmField
    var encrypted: Boolean = false
}
