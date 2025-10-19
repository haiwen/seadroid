package com.seafile.seadroid2.framework.model.sdoc

import android.os.Parcel
import android.os.Parcelable
import com.google.gson.annotations.JsonAdapter
import com.google.gson.annotations.SerializedName
import com.seafile.seadroid2.framework.model.adapter.MetadataConfigDataJsonAdapter

class MetadataModel() : Parcelable {
    @JvmField
    var key: String? = null

    @JvmField
    var name: String? = null

    @JvmField
    var type: String? = null

    @SerializedName("data")
    @JsonAdapter(MetadataConfigDataJsonAdapter::class)
    @JvmField
    var configData: MutableList<MetadataConfigDataModel>? = null

    // holds scalar or structured values returned by the API
    @JvmField
    var value: Any? = null

    override fun describeContents(): Int = 0

    override fun writeToParcel(dest: Parcel, flags: Int) {
        dest.writeString(key)
        dest.writeString(name)
        dest.writeString(type)
        dest.writeTypedList(configData)
        dest.writeValue(value)
    }

    private constructor(parcel: Parcel) : this() {
        key = parcel.readString()
        name = parcel.readString()
        type = parcel.readString()
        configData = parcel.createTypedArrayList(MetadataConfigDataModel.CREATOR)
        value = parcel.readValue(MetadataModel::class.java.classLoader)
    }

    override fun toString(): String {
        return "MetadataModel(key=$key, name=$name, type=$type)"
    }

    companion object {
        @JvmField
        val CREATOR: Parcelable.Creator<MetadataModel> = object : Parcelable.Creator<MetadataModel> {
            override fun createFromParcel(source: Parcel): MetadataModel = MetadataModel(source)
            override fun newArray(size: Int): Array<MetadataModel?> = arrayOfNulls(size)
        }
    }
}
