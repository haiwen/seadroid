package com.seafile.seadroid2.framework.model.sdoc

class SDocOutlineWrapperModel {
    @JvmField
    var version: Int = 0

    @JvmField
    var format_version: Int = 0

    @JvmField
    var last_modify_user: String? = null

    @JvmField
    var elements: MutableList<OutlineItemModel>? = null
}
