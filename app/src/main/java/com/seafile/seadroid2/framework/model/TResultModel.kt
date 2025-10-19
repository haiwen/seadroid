package com.seafile.seadroid2.framework.model

class TResultModel<T> : ResultModel() {
    @JvmField
    var data: T? = null
}
