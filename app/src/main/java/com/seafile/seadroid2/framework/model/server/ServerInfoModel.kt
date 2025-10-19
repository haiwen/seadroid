package com.seafile.seadroid2.framework.model.server

import com.blankj.utilcode.util.CollectionUtils

class ServerInfoModel {
    @JvmField
    var version: String? = null

    @JvmField
    var encrypted_library_version: String? = null

    /**
     * pbkdf2_sha256
     */
    @JvmField
    var encrypted_library_pwd_hash_algo: String? = null

    /**
     * hash iteration count, e.g. 1000
     */
    @JvmField
    var encrypted_library_pwd_hash_params: String? = null

    @JvmField
    var features: List<String>? = null

    fun getFeaturesString(): String? {
        if (CollectionUtils.isEmpty(features)) {
            return null
        }
        return features?.joinToString(",")
    }
}
