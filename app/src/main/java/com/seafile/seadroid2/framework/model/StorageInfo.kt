package com.seafile.seadroid2.framework.model

class StorageInfo {
    @JvmField
    var path: String? = null

    @JvmField
    var label: String? = null

    @JvmField
    var isRemovable: Boolean = false

    @JvmField
    var isPrimary: Boolean = false

    @JvmField
    var isAvailable: Boolean = false

    @JvmField
    var type: Type? = null

    fun getPath(): String? = path
    fun setPath(value: String?) {
        path = value
    }

    fun getLabel(): String? = label
    fun setLabel(value: String?) {
        label = value
    }

    fun isRemovable(): Boolean = this.isRemovable
    fun setRemovable(value: Boolean) {
        this.isRemovable = value
    }

    fun isPrimary(): Boolean = this.isPrimary
    fun setPrimary(value: Boolean) {
        this.isPrimary = value
    }

    fun isAvailable(): Boolean = this.isAvailable
    fun setAvailable(value: Boolean) {
        this.isAvailable = value
    }

    fun getType(): Type? = type
    fun setType(value: Type?) {
        type = value
    }

    enum class Type {
        INTERNAL, SD_CARD, OTG, UNKNOWN
    }
}
