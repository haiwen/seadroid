package com.seafile.seadroid2.framework.model.sdoc

import com.seafile.seadroid2.framework.model.BaseModel

class OutlineItemModel : BaseModel() {
    @JvmField
    var id: String? = null

    @JvmField
    var type: String? = null

    @JvmField
    var text: String? = null

    @JvmField
    var indent: Boolean = false

    @JvmField
    var children: MutableList<OutlineItemModel>? = null

    @JvmField
    var data: SDocDataModel? = null
}
