package com.seafile.seadroid2.framework.model.user

import android.os.Parcel
import android.os.Parcelable
import com.google.gson.annotations.SerializedName

class UserModel() : Parcelable {

    var email: String? = null

    var name: String? = null

    @SerializedName("contact_email")
    private var contactEmailBacking: String? = null

    @SerializedName("avatar_url")
    private var avatarUrlBacking: String? = null

    fun getContactEmail(): String? = contactEmailBacking

    fun setContactEmail(contactEmail: String?) {
        contactEmailBacking = contactEmail
    }

    fun getAvatarUrl(): String? = avatarUrlBacking

    fun setAvatarUrl(avatarUrl: String?) {
        avatarUrlBacking = avatarUrl
    }

    override fun describeContents(): Int = 0

    override fun writeToParcel(dest: Parcel, flags: Int) {
        dest.writeString(email)
        dest.writeString(name)
        dest.writeString(contactEmailBacking)
        dest.writeString(avatarUrlBacking)
    }

    private constructor(parcel: Parcel) : this() {
        email = parcel.readString()
        name = parcel.readString()
        contactEmailBacking = parcel.readString()
        avatarUrlBacking = parcel.readString()
    }

    companion object {
        @JvmField
        val CREATOR: Parcelable.Creator<UserModel> = object : Parcelable.Creator<UserModel> {
            override fun createFromParcel(source: Parcel): UserModel = UserModel(source)
            override fun newArray(size: Int): Array<UserModel?> = arrayOfNulls(size)
        }
    }
}
