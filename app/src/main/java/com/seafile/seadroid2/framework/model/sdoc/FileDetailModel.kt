package com.seafile.seadroid2.framework.model.sdoc

import android.os.Parcel
import android.os.Parcelable
import com.google.gson.annotations.JsonAdapter
import com.google.gson.annotations.SerializedName
import com.seafile.seadroid2.framework.model.adapter.OffsetDateTimeAdapter
import com.seafile.seadroid2.framework.util.Utils
import java.time.OffsetDateTime
import java.time.format.DateTimeFormatter

class FileDetailModel() : Parcelable {

    private var type: String? = null
    private var id: String? = null
    private var name: String? = null
    private var permission: String? = null
    private var mtime: Long = 0

    @JsonAdapter(OffsetDateTimeAdapter::class)
    @SerializedName("last_modified")
    private var lastModified: OffsetDateTime? = null

    @SerializedName("last_modifier_email")
    private var lastModifierEmail: String? = null

    @SerializedName("last_modifier_name")
    private var lastModifierName: String? = null

    @SerializedName("last_modifier_contact_email")
    private var lastModifierContactEmail: String? = null

    @SerializedName("last_modifier_avatar")
    private var lastModifierAvatar: String? = null

    private var size: Long = 0
    private var starred: Boolean = false
    private var commentTotal: Long = 0
    private var canEdit: Boolean = false

    fun getPermission(): String? = permission

    fun setPermission(value: String?) {
        permission = value
    }

    fun getType(): String? = type

    fun setType(value: String?) {
        type = value
    }

    fun getMtime(): Long = mtime

    fun setMtime(value: Long) {
        mtime = value
    }

    fun getSize(): Long = size

    fun getSizeText(): String = Utils.readableFileSize(size)

    fun setSize(value: Long) {
        size = value
    }

    fun getStarred(): Boolean = starred

    fun setStarred(value: Boolean) {
        starred = value
    }

    fun getName(): String? = name

    fun setName(value: String?) {
        name = value
    }

    fun getCommentTotal(): Long = commentTotal

    fun setCommentTotal(value: Long) {
        commentTotal = value
    }

    fun getId(): String? = id

    fun setId(value: String?) {
        id = value
    }

    fun getLastModified(): OffsetDateTime? = lastModified

    fun getLastModifiedText(): String {
        val value = getLastModified() ?: return ""
        return value.format(DateTimeFormatter.ISO_LOCAL_DATE_TIME).replace("T", " ")
    }

    fun setLastModified(value: OffsetDateTime?) {
        lastModified = value
    }

    fun getLastModifierEmail(): String? = lastModifierEmail

    fun setLastModifierEmail(value: String?) {
        lastModifierEmail = value
    }

    fun getLastModifierName(): String? = lastModifierName

    fun setLastModifierName(value: String?) {
        lastModifierName = value
    }

    fun getLastModifierContactEmail(): String? = lastModifierContactEmail

    fun setLastModifierContactEmail(value: String?) {
        lastModifierContactEmail = value
    }

    fun getLastModifierAvatar(): String? = lastModifierAvatar

    fun setLastModifierAvatar(value: String?) {
        lastModifierAvatar = value
    }

    fun getCanEdit(): Boolean = canEdit

    fun setCanEdit(value: Boolean) {
        canEdit = value
    }

    override fun describeContents(): Int = 0

    override fun writeToParcel(dest: Parcel, flags: Int) {
        dest.writeString(type)
        dest.writeString(id)
        dest.writeString(name)
        dest.writeString(permission)
        dest.writeLong(mtime)
        dest.writeSerializable(lastModified)
        dest.writeString(lastModifierEmail)
        dest.writeString(lastModifierName)
        dest.writeString(lastModifierContactEmail)
        dest.writeString(lastModifierAvatar)
        dest.writeLong(size)
        dest.writeByte((if (starred) 1 else 0).toByte())
        dest.writeLong(commentTotal)
        dest.writeByte((if (canEdit) 1 else 0).toByte())
    }

    private constructor(parcel: Parcel) : this() {
        type = parcel.readString()
        id = parcel.readString()
        name = parcel.readString()
        permission = parcel.readString()
        mtime = parcel.readLong()
        lastModified = parcel.readSerializable() as? OffsetDateTime
        lastModifierEmail = parcel.readString()
        lastModifierName = parcel.readString()
        lastModifierContactEmail = parcel.readString()
        lastModifierAvatar = parcel.readString()
        size = parcel.readLong()
        starred = parcel.readByte().toInt() != 0
        commentTotal = parcel.readLong()
        canEdit = parcel.readByte().toInt() != 0
    }

    companion object {
        @JvmField
        val CREATOR: Parcelable.Creator<FileDetailModel> =
            object : Parcelable.Creator<FileDetailModel> {
                override fun createFromParcel(source: Parcel): FileDetailModel = FileDetailModel(source)
                override fun newArray(size: Int): Array<FileDetailModel?> = arrayOfNulls(size)
            }
    }
}
