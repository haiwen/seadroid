package com.seafile.seadroid2.framework.model.search

import android.os.Parcel
import android.os.Parcelable
import android.text.TextUtils
import com.seafile.seadroid2.R
import com.seafile.seadroid2.framework.db.entities.DirentModel
import com.seafile.seadroid2.framework.db.entities.RepoModel
import com.seafile.seadroid2.framework.model.BaseModel
import com.seafile.seadroid2.framework.util.Icons
import com.seafile.seadroid2.framework.util.Utils
import org.apache.commons.lang3.StringUtils

class SearchModel() : BaseModel(), Parcelable {

    @JvmField
    var is_dir: Boolean = false

    @JvmField
    var fullpath: String? = null

    @JvmField
    var related_account: String? = null

    @JvmField
    var repo_id: String? = null

    @JvmField
    var repo_name: String? = null

    @JvmField
    var repo_owner_email: String? = null

    @JvmField
    var repo_owner_name: String? = null

    @JvmField
    var repo_owner_contact_email: String? = null

    @JvmField
    var thumbnail_url: String? = null

    @JvmField
    var repo_type: String? = null

    @JvmField
    var name: String? = null

    @JvmField
    var content_highlight: String? = null

    @JvmField
    var last_modified: Long = 0

    /**
     * Size of file, 0 if directory.
     */
    @JvmField
    var size: Long = 0

    init {
        checkable = false
    }

    fun isDir(): Boolean = is_dir

    fun getTitle(): String? {
        val formatName = StringUtils.substringAfterLast(fullpath ?: "", "/")
        return if (TextUtils.isEmpty(formatName)) name else formatName
    }

    fun getSubtitle(): String? {
        val p = Utils.getPathFromFullPath(fullpath)
        return repo_name + p
    }

    fun getIcon(): Int {
        if (is_dir) {
            return if (TextUtils.equals(name, repo_name)) {
                R.drawable.baseline_repo_24
            } else {
                R.drawable.baseline_folder_24
            }
        }
        return Icons.getFileIcon(getTitle())
    }

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other !is SearchModel) return false
        return is_dir == other.is_dir &&
            last_modified == other.last_modified &&
            size == other.size &&
            fullpath == other.fullpath &&
            repo_id == other.repo_id &&
            repo_name == other.repo_name &&
            repo_owner_email == other.repo_owner_email &&
            repo_owner_name == other.repo_owner_name &&
            repo_owner_contact_email == other.repo_owner_contact_email &&
            thumbnail_url == other.thumbnail_url &&
            repo_type == other.repo_type &&
            name == other.name &&
            content_highlight == other.content_highlight
    }

    override fun hashCode(): Int {
        var result = (is_dir.hashCode())
        result = 31 * result + (fullpath?.hashCode() ?: 0)
        result = 31 * result + (repo_id?.hashCode() ?: 0)
        result = 31 * result + (repo_name?.hashCode() ?: 0)
        result = 31 * result + (repo_owner_email?.hashCode() ?: 0)
        result = 31 * result + (repo_owner_name?.hashCode() ?: 0)
        result = 31 * result + (repo_owner_contact_email?.hashCode() ?: 0)
        result = 31 * result + (thumbnail_url?.hashCode() ?: 0)
        result = 31 * result + (repo_type?.hashCode() ?: 0)
        result = 31 * result + (name?.hashCode() ?: 0)
        result = 31 * result + (content_highlight?.hashCode() ?: 0)
        result = 31 * result + last_modified.hashCode()
        result = 31 * result + size.hashCode()
        return result
    }

    override fun describeContents(): Int = 0

    override fun writeToParcel(dest: Parcel, flags: Int) {
        dest.writeByte((if (is_dir) 1 else 0).toByte())
        dest.writeString(fullpath)
        dest.writeString(related_account)
        dest.writeString(repo_id)
        dest.writeString(repo_name)
        dest.writeString(repo_owner_email)
        dest.writeString(repo_owner_name)
        dest.writeString(repo_owner_contact_email)
        dest.writeString(thumbnail_url)
        dest.writeString(repo_type)
        dest.writeString(name)
        dest.writeString(content_highlight)
        dest.writeLong(last_modified)
        dest.writeLong(size)
    }

    fun readFromParcel(source: Parcel) {
        is_dir = source.readByte().toInt() != 0
        fullpath = source.readString()
        related_account = source.readString()
        repo_id = source.readString()
        repo_name = source.readString()
        repo_owner_email = source.readString()
        repo_owner_name = source.readString()
        repo_owner_contact_email = source.readString()
        thumbnail_url = source.readString()
        repo_type = source.readString()
        name = source.readString()
        content_highlight = source.readString()
        last_modified = source.readLong()
        size = source.readLong()
    }

    private constructor(parcel: Parcel) : this() {
        is_dir = parcel.readByte().toInt() != 0
        fullpath = parcel.readString()
        related_account = parcel.readString()
        repo_id = parcel.readString()
        repo_name = parcel.readString()
        repo_owner_email = parcel.readString()
        repo_owner_name = parcel.readString()
        repo_owner_contact_email = parcel.readString()
        thumbnail_url = parcel.readString()
        repo_type = parcel.readString()
        name = parcel.readString()
        content_highlight = parcel.readString()
        last_modified = parcel.readLong()
        size = parcel.readLong()
    }

    companion object {
        @JvmField
        val CREATOR: Parcelable.Creator<SearchModel> = object : Parcelable.Creator<SearchModel> {
            override fun createFromParcel(source: Parcel): SearchModel = SearchModel(source)
            override fun newArray(size: Int): Array<SearchModel?> = arrayOfNulls(size)
        }

        @JvmStatic
        fun convert2RepoModel(model: SearchModel): RepoModel {
            val d = RepoModel()
            d.type = model.repo_type
            d.repo_id = model.repo_id ?: ""
            d.repo_name = model.repo_name
            d.related_account = ""
            d.size = model.size
            d.owner_email = model.repo_owner_email
            d.owner_name = model.repo_owner_name
            d.owner_contact_email = model.repo_owner_contact_email
            return d
        }

        @JvmStatic
        fun convert2DirentModel(model: SearchModel): DirentModel {
            val d = DirentModel()
            d.full_path = model.fullpath
            d.type = if (model.is_dir) "dir" else "file"
            d.name = Utils.getFileNameFromPath(model.fullpath)
            d.repo_id = model.repo_id
            d.repo_name = model.repo_name
            d.last_modified_at = model.last_modified
            d.size = model.size
            d.parent_dir = Utils.getParentPath(model.fullpath)
            return d
        }
    }
}
