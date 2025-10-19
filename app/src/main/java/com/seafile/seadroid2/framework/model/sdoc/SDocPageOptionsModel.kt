package com.seafile.seadroid2.framework.model.sdoc

import android.os.Parcel
import android.os.Parcelable
import android.text.TextUtils

class SDocPageOptionsModel() : Parcelable {
    @JvmField
    var docName: String? = null

    @JvmField
    var docUuid: String? = null

    @JvmField
    var seadocServerUrl: String? = null

    @JvmField
    var seadocAccessToken: String? = null

    @JvmField
    var repoID: String? = null

    @JvmField
    var repoName: String? = null

    @JvmField
    var isLocked: Boolean = false

    @JvmField
    var isStarred: Boolean = false

    @JvmField
    var enableMetadataManagement: Boolean = false

    fun canUse(): Boolean {
        return !(TextUtils.isEmpty(seadocServerUrl)
            || TextUtils.isEmpty(docUuid)
            || TextUtils.isEmpty(repoID)
            || TextUtils.isEmpty(seadocAccessToken))
    }

    override fun toString(): String {
        return "SDocPageOptionsModel(docName=$docName, docUuid=$docUuid, seadocServerUrl=$seadocServerUrl, seadocAccessToken=$seadocAccessToken, repoID=$repoID, repoName=$repoName, isLocked=$isLocked, isStarred=$isStarred)"
    }

    override fun describeContents(): Int = 0

    override fun writeToParcel(dest: Parcel, flags: Int) {
        dest.writeString(docName)
        dest.writeString(docUuid)
        dest.writeString(seadocServerUrl)
        dest.writeString(seadocAccessToken)
        dest.writeString(repoID)
        dest.writeString(repoName)
        dest.writeByte((if (isLocked) 1 else 0).toByte())
        dest.writeByte((if (isStarred) 1 else 0).toByte())
        dest.writeByte((if (enableMetadataManagement) 1 else 0).toByte())
    }

    private constructor(parcel: Parcel) : this() {
        docName = parcel.readString()
        docUuid = parcel.readString()
        seadocServerUrl = parcel.readString()
        seadocAccessToken = parcel.readString()
        repoID = parcel.readString()
        repoName = parcel.readString()
        isLocked = parcel.readByte().toInt() != 0
        isStarred = parcel.readByte().toInt() != 0
        enableMetadataManagement = parcel.readByte().toInt() != 0
    }

    companion object {
        @JvmField
        val CREATOR: Parcelable.Creator<SDocPageOptionsModel> =
            object : Parcelable.Creator<SDocPageOptionsModel> {
                override fun createFromParcel(source: Parcel): SDocPageOptionsModel =
                    SDocPageOptionsModel(source)

                override fun newArray(size: Int): Array<SDocPageOptionsModel?> = arrayOfNulls(size)
            }
    }
}
