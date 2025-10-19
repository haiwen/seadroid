package com.seafile.seadroid2.framework.model

import android.os.Parcel
import android.os.Parcelable
import android.text.TextUtils
import com.google.common.base.MoreObjects
import org.json.JSONException
import org.json.JSONObject

class ServerInfo(
    private var url: String?,
    private var version: String?,
    private var features: String?,
    private var encrypted_library_version: String?
) : Parcelable {

    fun getEncrypted_library_version(): String? = encrypted_library_version

    fun setEncrypted_library_version(value: String?) {
        encrypted_library_version = value
    }

    fun getVersion(): String? = version

    fun getFeatures(): String? = features

    fun isProEdition(): Boolean = features?.contains("seafile-pro") == true

    fun isSearchEnabled(): Boolean = features?.contains("file-search") == true

    fun canLocalDecrypt(): Boolean {
        if (TextUtils.isEmpty(version)) {
            return false
        }
        val realVersion = version!!.replace(".", "")
        val versionCode = realVersion.toIntOrNull() ?: return false
        return versionCode >= 510
    }

    fun getUrl(): String? = url

    override fun hashCode(): Int {
        var result = url?.hashCode() ?: 0
        result = 31 * result + (version?.hashCode() ?: 0)
        result = 31 * result + (features?.hashCode() ?: 0)
        return result
    }

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other !is ServerInfo) return false
        return other.url != null && other.version != null && other.features != null &&
            other.url == url && other.version == version && other.features == features
    }

    override fun toString(): String {
        return MoreObjects.toStringHelper(this)
            .add("url", url)
            .add("version", version)
            .add("features", features)
            .toString()
    }

    override fun describeContents(): Int = 0

    override fun writeToParcel(dest: Parcel, flags: Int) {
        dest.writeString(url)
        dest.writeString(version)
        dest.writeString(features)
        dest.writeString(encrypted_library_version)
    }

    private constructor(parcel: Parcel) : this(
        parcel.readString(),
        parcel.readString(),
        parcel.readString(),
        parcel.readString()
    )

    companion object {
        @JvmStatic
        @Throws(JSONException::class)
        fun fromJson(obj: JSONObject, server: String): ServerInfo {
            val version = obj.optString("version")
            val features = obj.optString("features")
            val encryptedLibraryVersion = obj.optString("encrypted_library_version")
            return ServerInfo(server, version, features, encryptedLibraryVersion)
        }

        @JvmField
        val CREATOR: Parcelable.Creator<ServerInfo> =
            object : Parcelable.Creator<ServerInfo> {
                override fun createFromParcel(source: Parcel): ServerInfo = ServerInfo(source)
                override fun newArray(size: Int): Array<ServerInfo?> = arrayOfNulls(size)
            }
    }
}
