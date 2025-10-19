package com.seafile.seadroid2.framework.model.activities

import android.text.TextUtils
import com.seafile.seadroid2.enums.OpType
import com.seafile.seadroid2.framework.model.BaseModel
import com.seafile.seadroid2.framework.util.Times
import com.seafile.seadroid2.framework.util.Utils

class ActivityModel : BaseModel() {
    @JvmField
    var op_type: String? = null

    @JvmField
    var related_account: String? = null

    @JvmField
    var repo_id: String? = null

    @JvmField
    var repo_name: String? = null

    @JvmField
    var obj_type: String? = null

    @JvmField
    var commit_id: String? = null

    @JvmField
    var path: String? = null

    @JvmField
    var name: String? = null

    @JvmField
    var old_path: String? = null

    @JvmField
    var old_name: String? = null

    @JvmField
    var author_email: String? = null

    @JvmField
    var author_name: String? = null

    @JvmField
    var author_contact_email: String? = null

    @JvmField
    var avatar_url: String? = null

    @JvmField
    var time: String? = null

    @JvmField
    var opType: OpType? = null

    private var mTimeLong: Long = 0

    fun getTime(): String {
        if (mTimeLong == 0L) {
            mTimeLong = Times.convertMtime2Long(time)
        }
        return Utils.translateCommitTime(mTimeLong)
    }

    fun isFileOpenable(): Boolean {
        return opType == OpType.CREATE ||
            opType == OpType.UPDATE ||
            opType == OpType.RENAME ||
            opType == OpType.EDIT
    }

    fun isDir(): Boolean = TextUtils.equals(obj_type, "dir")
}
