package com.seafile.seadroid2.framework.model.sdoc

import android.os.Parcel
import android.os.Parcelable

class FileTagResultModel() : Parcelable {
    @JvmField
    var _creator: String? = null

    @JvmField
    var _id: String? = null

    @JvmField
    var _last_modifier: String? = null

    @JvmField
    var _tag_color: String? = null

    @JvmField
    var _tag_name: String? = null

    @JvmField
    var _tag_file_links: MutableList<FileLinkModel>? = null

    override fun describeContents(): Int = 0

    override fun writeToParcel(dest: Parcel, flags: Int) {
        dest.writeString(_creator)
        dest.writeString(_id)
        dest.writeString(_last_modifier)
        dest.writeString(_tag_color)
        dest.writeString(_tag_name)
        dest.writeTypedList(_tag_file_links)
    }

    private constructor(parcel: Parcel) : this() {
        _creator = parcel.readString()
        _id = parcel.readString()
        _last_modifier = parcel.readString()
        _tag_color = parcel.readString()
        _tag_name = parcel.readString()
        _tag_file_links = parcel.createTypedArrayList(FileLinkModel.CREATOR)
    }

    companion object {
        @JvmField
        val CREATOR: Parcelable.Creator<FileTagResultModel> =
            object : Parcelable.Creator<FileTagResultModel> {
                override fun createFromParcel(source: Parcel): FileTagResultModel = FileTagResultModel(source)
                override fun newArray(size: Int): Array<FileTagResultModel?> = arrayOfNulls(size)
            }
    }
}
