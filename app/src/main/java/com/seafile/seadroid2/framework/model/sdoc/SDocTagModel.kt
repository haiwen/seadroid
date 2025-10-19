package com.seafile.seadroid2.framework.model.sdoc

import android.os.Parcel
import android.os.Parcelable

class SDocTagModel() : Parcelable {
    @JvmField
    var name: String? = null

    @JvmField
    var id: String? = null

    @JvmField
    var color: String? = null

    override fun describeContents(): Int = 0

    override fun writeToParcel(dest: Parcel, flags: Int) {
        dest.writeString(name)
        dest.writeString(id)
        dest.writeString(color)
    }

    private constructor(parcel: Parcel) : this() {
        name = parcel.readString()
        id = parcel.readString()
        color = parcel.readString()
    }

    companion object {
        @JvmField
        val CREATOR: Parcelable.Creator<SDocTagModel> = object : Parcelable.Creator<SDocTagModel> {
            override fun createFromParcel(source: Parcel): SDocTagModel = SDocTagModel(source)
            override fun newArray(size: Int): Array<SDocTagModel?> = arrayOfNulls(size)
        }
    }
}
