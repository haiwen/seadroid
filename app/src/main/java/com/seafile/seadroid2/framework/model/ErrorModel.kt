package com.seafile.seadroid2.framework.model

import com.google.gson.annotations.SerializedName

class ErrorModel {
    @SerializedName("error_msg")
    var error_msg: String? = null
    var msg: String? = null

    @SerializedName("error")
    var errorRaw: String? = null

    var errorMsg: String? = null
    var errMessage: String? = null

    @SerializedName("err_message")
    var err_message: String? = null

    var detail: String? = null

    @SerializedName("non_field_errors")
    var non_field_errors: List<String>? = null

    fun getError(): String? {
        if (!error_msg.isNullOrEmpty()) return error_msg
        if (!errorRaw.isNullOrEmpty()) return errorRaw
        if (!errorMsg.isNullOrEmpty()) return errorMsg
        if (!detail.isNullOrEmpty()) return detail
        if (!errMessage.isNullOrEmpty()) return errMessage
        if (!err_message.isNullOrEmpty()) return err_message
        non_field_errors?.firstOrNull()?.let { return it }
        if (!msg.isNullOrEmpty()) return msg
        return null
    }
}
