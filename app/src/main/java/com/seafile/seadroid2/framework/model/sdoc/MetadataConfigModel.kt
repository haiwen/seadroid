package com.seafile.seadroid2.framework.model.sdoc

import android.os.Parcel
import android.os.Parcelable

class MetadataConfigModel() : Parcelable {
    @JvmField
    var enabled: Boolean = false

    @JvmField
    var tags_enabled: Boolean = false

    override fun describeContents(): Int = 0

    override fun writeToParcel(dest: Parcel, flags: Int) {
        dest.writeByte((if (enabled) 1 else 0).toByte())
        dest.writeByte((if (tags_enabled) 1 else 0).toByte())
    }

    private constructor(parcel: Parcel) : this() {
        enabled = parcel.readByte().toInt() != 0
        tags_enabled = parcel.readByte().toInt() != 0
    }

    companion object {
        @JvmField
        val CREATOR: Parcelable.Creator<MetadataConfigModel> =
            object : Parcelable.Creator<MetadataConfigModel> {
                override fun createFromParcel(source: Parcel): MetadataConfigModel = MetadataConfigModel(source)
                override fun newArray(size: Int): Array<MetadataConfigModel?> = arrayOfNulls(size)
            }
    }
}
