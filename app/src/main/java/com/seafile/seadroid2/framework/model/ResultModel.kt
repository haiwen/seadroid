package com.seafile.seadroid2.framework.model

open class ResultModel {
    @JvmField
    var success: Boolean = false

    @JvmField
    var error_msg: String? = null

    override fun toString(): String {
        return "ResultModel(success=$success, error_msg=$error_msg)"
    }
}
