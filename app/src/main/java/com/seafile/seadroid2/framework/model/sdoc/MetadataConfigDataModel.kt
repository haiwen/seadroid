package com.seafile.seadroid2.framework.model.sdoc

import android.os.Parcel
import android.os.Parcelable
import com.google.gson.annotations.SerializedName

class MetadataConfigDataModel() : Parcelable {
    @JvmField
    var format: String? = null

    @JvmField
    var geo_format: String? = null

    @JvmField
    var options: MutableList<OptionsTagModel>? = null

    @JvmField
    var enable_precision: Boolean = false

    @JvmField
    var precision: Int = 0

    @JvmField
    var currency_symbol: String? = null

    @JvmField
    var currency_symbol_position: String? = null

    @SerializedName("max")
    @JvmField
    var rate_max_number: Int = 0

    @SerializedName("color")
    @JvmField
    var rate_style_color: String? = null

    override fun describeContents(): Int = 0

    override fun writeToParcel(dest: Parcel, flags: Int) {
        dest.writeString(format)
        dest.writeString(geo_format)
        dest.writeTypedList(options)
        dest.writeInt(rate_max_number)
        dest.writeString(rate_style_color)
    }

    private constructor(parcel: Parcel) : this() {
        format = parcel.readString()
        geo_format = parcel.readString()
        options = parcel.createTypedArrayList(OptionsTagModel.CREATOR)
        rate_max_number = parcel.readInt()
        rate_style_color = parcel.readString()
    }

    companion object {
        @JvmField
        val CREATOR: Parcelable.Creator<MetadataConfigDataModel> =
            object : Parcelable.Creator<MetadataConfigDataModel> {
                override fun createFromParcel(source: Parcel): MetadataConfigDataModel =
                    MetadataConfigDataModel(source)

                override fun newArray(size: Int): Array<MetadataConfigDataModel?> = arrayOfNulls(size)
            }
    }
}
