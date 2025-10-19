package com.seafile.seadroid2.framework.model.repo

import android.text.TextUtils
import com.seafile.seadroid2.R
import com.seafile.seadroid2.account.Account
import com.seafile.seadroid2.framework.db.entities.RepoModel
import org.apache.commons.lang3.StringUtils

class RepoInfoModel {
    @JvmField
    var id: String? = null

    @JvmField
    var name: String? = null

    /**
     * mine | group | shared
     */
    @JvmField
    var type: String? = null

    @JvmField
    var owner: String? = null

    @JvmField
    var mtime: Long = 0

    @JvmField
    var size: Long = 0

    @JvmField
    var encrypted: Boolean = false

    @JvmField
    var root: String? = null

    @JvmField
    var permission: String? = null

    @JvmField
    var modifier_email: String? = null

    @JvmField
    var modifier_contact_email: String? = null

    @JvmField
    var modifier_name: String? = null

    @JvmField
    var file_count: Int = 0

    @JvmField
    var head_commit_id: String? = null

    @JvmField
    var enc_version: Int = 0

    @JvmField
    var magic: String? = null

    @JvmField
    var salt: String? = null

    @JvmField
    var random_key: String? = null

    fun getIcon(): Int {
        return when {
            encrypted -> R.drawable.baseline_repo_encrypted_24
            isCustomPermission() -> R.drawable.baseline_repo_24
            !hasWritePermission() -> R.drawable.baseline_repo_readonly_24
            else -> R.drawable.baseline_repo_24
        }
    }

    /**
     * Check write permission; call [isCustomPermission] first if needed.
     */
    fun hasWritePermission(): Boolean {
        val perm = permission
        if (perm.isNullOrEmpty()) {
            return false
        }

        if (perm == "cloud-edit") {
            return false
        }

        if (perm == "preview") {
            return false
        }

        return perm.contains("w")
    }

    /**
     * Custom permission strings start with "custom-".
     */
    fun isCustomPermission(): Boolean {
        return !permission.isNullOrEmpty() && permission!!.startsWith("custom-")
    }

    /**
     * Requires [isCustomPermission] to be true.
     */
    fun getCustomPermissionNum(): Int {
        check(isCustomPermission()) { "please check isCustomPermission() first" }
        val parts = StringUtils.split(permission, "-")
        return parts[1].toInt()
    }

    /**
     * New feature introduced in 3.0.5 (2024/10/22); placeholder until implemented.
     */
    fun canLocalDecrypt(): Boolean = false

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other !is RepoInfoModel) return false

        return encrypted == other.encrypted &&
            size == other.size &&
            enc_version == other.enc_version &&
            file_count == other.file_count &&
            mtime == other.mtime &&
            id == other.id &&
            name == other.name &&
            type == other.type &&
            modifier_email == other.modifier_email &&
            modifier_name == other.modifier_name &&
            modifier_contact_email == other.modifier_contact_email &&
            permission == other.permission &&
            salt == other.salt &&
            root == other.root &&
            magic == other.magic &&
            random_key == other.random_key
    }

    override fun hashCode(): Int {
        var result = id?.hashCode() ?: 0
        result = 31 * result + (name?.hashCode() ?: 0)
        result = 31 * result + (type?.hashCode() ?: 0)
        result = 31 * result + (modifier_email?.hashCode() ?: 0)
        result = 31 * result + (modifier_name?.hashCode() ?: 0)
        result = 31 * result + (modifier_contact_email?.hashCode() ?: 0)
        result = 31 * result + encrypted.hashCode()
        result = 31 * result + size.hashCode()
        result = 31 * result + (permission?.hashCode() ?: 0)
        result = 31 * result + (salt?.hashCode() ?: 0)
        result = 31 * result + mtime.hashCode()
        result = 31 * result + (root?.hashCode() ?: 0)
        result = 31 * result + (magic?.hashCode() ?: 0)
        result = 31 * result + (random_key?.hashCode() ?: 0)
        result = 31 * result + enc_version
        result = 31 * result + file_count
        return result
    }

    companion object {
        @JvmStatic
        fun toRepoModel(account: Account, repoInfoModel: RepoInfoModel): RepoModel {
            val repoModel = RepoModel()
            repoModel.repo_id = repoInfoModel.id ?: ""
            repoModel.repo_name = repoInfoModel.name
            repoModel.type = repoInfoModel.type
            repoModel.file_count = repoInfoModel.file_count
            repoModel.size = repoInfoModel.size
            repoModel.encrypted = repoInfoModel.encrypted
            repoModel.enc_version = repoInfoModel.enc_version
            repoModel.last_modified_long = repoInfoModel.mtime
            repoModel.related_account = account.signature
            repoModel.permission = repoInfoModel.permission
            repoModel.salt = repoInfoModel.salt
            repoModel.magic = repoInfoModel.magic
            repoModel.random_key = repoInfoModel.random_key
            repoModel.root = repoInfoModel.root
            repoModel.modifier_email = repoInfoModel.modifier_email
            repoModel.modifier_name = repoInfoModel.modifier_name
            repoModel.modifier_contact_email = repoInfoModel.modifier_contact_email
            return repoModel
        }
    }
}
