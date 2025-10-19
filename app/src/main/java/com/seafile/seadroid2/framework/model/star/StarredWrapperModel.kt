package com.seafile.seadroid2.framework.model.star

import com.seafile.seadroid2.framework.db.entities.StarredModel

class StarredWrapperModel {
    @JvmField
    var starred_item_list: MutableList<StarredModel>? = null
}
