package com.seafile.seadroid2.framework.model.sdoc

import android.os.Parcel
import android.os.Parcelable
import androidx.room.Ignore

class OptionsTagModel() : Parcelable {
    @JvmField
    var borderColor: String? = null

    // default #EED5FF
    @JvmField
    var color: String = "#EED5FF"

    @JvmField
    var id: String? = null

    @JvmField
    var name: String? = null

    @JvmField
    var textColor: String = "#202428"

    @Ignore
    @JvmField
    var isSelected: Boolean = false

    override fun toString(): String {
        return "ColumnDataOptionsModel(borderColor=$borderColor, color=$color, id=$id, name=$name, textColor=$textColor, isSelected=$isSelected)"
    }

    override fun describeContents(): Int = 0

    override fun writeToParcel(dest: Parcel, flags: Int) {
        dest.writeString(borderColor)
        dest.writeString(color)
        dest.writeString(id)
        dest.writeString(name)
        dest.writeString(textColor)
        dest.writeByte((if (isSelected) 1 else 0).toByte())
    }

    fun readFromParcel(source: Parcel) {
        borderColor = source.readString()
        color = source.readString() ?: "#EED5FF"
        id = source.readString()
        name = source.readString()
        textColor = source.readString() ?: "#202428"
        isSelected = source.readByte().toInt() != 0
    }

    private constructor(parcel: Parcel) : this() {
        borderColor = parcel.readString()
        color = parcel.readString() ?: "#EED5FF"
        id = parcel.readString()
        name = parcel.readString()
        textColor = parcel.readString() ?: "#202428"
        isSelected = parcel.readByte().toInt() != 0
    }

    companion object {
        @JvmField
        val CREATOR: Parcelable.Creator<OptionsTagModel> = object : Parcelable.Creator<OptionsTagModel> {
            override fun createFromParcel(source: Parcel): OptionsTagModel = OptionsTagModel(source)
            override fun newArray(size: Int): Array<OptionsTagModel?> = arrayOfNulls(size)
        }
    }
}
