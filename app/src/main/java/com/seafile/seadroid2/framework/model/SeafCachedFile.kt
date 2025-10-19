package com.seafile.seadroid2.framework.model

import com.seafile.seadroid2.framework.util.Icons
import com.seafile.seadroid2.framework.util.Utils
import java.io.File

class SeafCachedFile : SeafItem {
    @JvmField
    var id: Int = -1

    @JvmField
    var fileID: String? = null

    @JvmField
    var repoName: String? = null

    @JvmField
    var repoID: String? = null

    @JvmField
    var path: String = ""

    @JvmField
    var accountSignature: String? = null

    @JvmField
    var fileOriginalSize: Long = 0

    @JvmField
    var file: File? = null

    override fun getTitle(): String? {
        val lastSlash = path.lastIndexOf('/')
        return if (lastSlash >= 0 && lastSlash < path.length - 1) {
            path.substring(lastSlash + 1)
        } else {
            path
        }
    }

    override fun getSubtitle(): String {
        val size = file?.length() ?: 0L
        return Utils.readableFileSize(size)
    }

    override fun getIcon(): Int {
        val name = file?.name ?: ""
        return Icons.getFileIcon(name)
    }

    fun getSize(): Long = file?.length() ?: 0L

    fun getLastModified(): Long = file?.lastModified() ?: 0L

    fun isDirectory(): Boolean = file?.isDirectory ?: false
}
