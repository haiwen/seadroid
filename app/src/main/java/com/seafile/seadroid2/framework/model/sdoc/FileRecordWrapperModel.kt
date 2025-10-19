package com.seafile.seadroid2.framework.model.sdoc

import android.os.Parcel
import android.os.Parcelable

class FileRecordWrapperModel() : Parcelable {
    @JvmField
    var metadata: MutableList<MetadataModel> = ArrayList()

    @JvmField
    var results: MutableList<Map<String, Any?>> = ArrayList()

    override fun describeContents(): Int = 0

    override fun writeToParcel(dest: Parcel, flags: Int) {
        dest.writeTypedList(metadata)
        dest.writeList(results)
    }

    private constructor(parcel: Parcel) : this() {
        metadata = parcel.createTypedArrayList(MetadataModel.CREATOR) ?: ArrayList()
        val list = ArrayList<Map<String, Any?>>()
        parcel.readList(list, Map::class.java.classLoader)
        results = list
    }

    companion object {
        @JvmField
        val CREATOR: Parcelable.Creator<FileRecordWrapperModel> =
            object : Parcelable.Creator<FileRecordWrapperModel> {
                override fun createFromParcel(source: Parcel): FileRecordWrapperModel = FileRecordWrapperModel(source)
                override fun newArray(size: Int): Array<FileRecordWrapperModel?> = arrayOfNulls(size)
            }
    }
}
