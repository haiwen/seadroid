package com.seafile.seadroid2.framework.model.sdoc

import android.os.Parcel
import android.os.Parcelable

class FileLinkModel() : Parcelable {
    @JvmField
    var display_value: String? = null

    @JvmField
    var row_id: String? = null

    override fun describeContents(): Int = 0

    override fun writeToParcel(dest: Parcel, flags: Int) {
        dest.writeString(display_value)
        dest.writeString(row_id)
    }

    private constructor(parcel: Parcel) : this() {
        display_value = parcel.readString()
        row_id = parcel.readString()
    }

    companion object {
        @JvmField
        val CREATOR: Parcelable.Creator<FileLinkModel> = object : Parcelable.Creator<FileLinkModel> {
            override fun createFromParcel(source: Parcel): FileLinkModel = FileLinkModel(source)
            override fun newArray(size: Int): Array<FileLinkModel?> = arrayOfNulls(size)
        }
    }
}
