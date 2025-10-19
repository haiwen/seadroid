package com.seafile.seadroid2.framework.model.dirents

import com.seafile.seadroid2.framework.model.ResultModel

class DeleteDirentModel : ResultModel() {
    @JvmField
    var commit_id: String? = null

    override fun toString(): String {
        return "DeleteDirentModel(commit_id=$commit_id, success=$success, error_msg=$error_msg)"
    }
}
