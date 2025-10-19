package com.seafile.seadroid2.framework.model.repo

import com.seafile.seadroid2.framework.model.BaseModel

class Dirent2Model : BaseModel() {
    @JvmField
    var repo_id: String? = null

    @JvmField
    var repo_name: String? = null

    @JvmField
    var path: String? = null

    @JvmField
    var obj_name: String? = null

    @JvmField
    var mtime: String? = null

    @JvmField
    var user_email: String? = null

    @JvmField
    var user_name: String? = null

    @JvmField
    var user_contact_email: String? = null

    @JvmField
    var repo_encrypted: Boolean = false

    @JvmField
    var is_dir: Boolean = false

    override fun toString(): String {
        return "Dirent2Model(repo_id=$repo_id, repo_name=$repo_name, path=$path, obj_name=$obj_name, mtime=$mtime, user_email=$user_email, user_name=$user_name, user_contact_email=$user_contact_email, repo_encrypted=$repo_encrypted, is_dir=$is_dir)"
    }
}
