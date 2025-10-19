package com.seafile.seadroid2.framework.model.search

class SearchWrapperModel {
    @JvmField
    var has_more: Boolean = false

    @JvmField
    var results: MutableList<SearchModel>? = null

    @JvmField
    var total: Int = 0
}
