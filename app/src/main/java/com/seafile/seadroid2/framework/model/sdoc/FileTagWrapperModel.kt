package com.seafile.seadroid2.framework.model.sdoc

import android.os.Parcel
import android.os.Parcelable

class FileTagWrapperModel() : Parcelable {
    @JvmField
    var metadata: MutableList<MetadataModel>? = null

    @JvmField
    var results: MutableList<FileTagResultModel>? = null

    override fun describeContents(): Int = 0

    override fun writeToParcel(dest: Parcel, flags: Int) {
        dest.writeTypedList(metadata)
        dest.writeTypedList(results)
    }

    private constructor(parcel: Parcel) : this() {
        metadata = parcel.createTypedArrayList(MetadataModel.CREATOR)
        results = parcel.createTypedArrayList(FileTagResultModel.CREATOR)
    }

    companion object {
        @JvmField
        val CREATOR: Parcelable.Creator<FileTagWrapperModel> =
            object : Parcelable.Creator<FileTagWrapperModel> {
                override fun createFromParcel(source: Parcel): FileTagWrapperModel = FileTagWrapperModel(source)
                override fun newArray(size: Int): Array<FileTagWrapperModel?> = arrayOfNulls(size)
            }
    }
}
