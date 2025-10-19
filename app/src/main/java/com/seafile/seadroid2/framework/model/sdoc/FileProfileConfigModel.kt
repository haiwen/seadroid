package com.seafile.seadroid2.framework.model.sdoc

import android.os.Parcel
import android.os.Parcelable
import android.text.TextUtils
import com.blankj.utilcode.util.CollectionUtils
import com.seafile.seadroid2.config.ColumnType
import com.seafile.seadroid2.framework.model.user.UserModel
import com.seafile.seadroid2.framework.model.user.UserWrapperModel
import com.seafile.seadroid2.framework.util.SLogs
import com.seafile.seadroid2.ui.media.image.PhotoFragment

class FileProfileConfigModel() : Parcelable {
    private var detail: FileDetailModel? = null
    private var metadataConfig: MetadataConfigModel? = null

    private var relatedUserList: MutableList<UserModel> = mutableListOf()
    private var recordMetaData: MutableList<MetadataModel> = mutableListOf()
    private var recordResults: MutableList<Map<String, Any?>> = mutableListOf()
    private var tagsMap: MutableMap<String, SDocTagModel> = HashMap()

    fun setMetadataConfigModel(metadataConfigModel: MetadataConfigModel?) {
        metadataConfig = metadataConfigModel
    }

    fun getMetaEnabled(): Boolean {
        val config = metadataConfig ?: throw RuntimeException("please first call setMetadataConfigModel()")
        return config.enabled
    }

    fun getTagsEnabled(): Boolean {
        val config = metadataConfig ?: throw RuntimeException("please first call setMetadataConfigModel()")
        return config.tags_enabled
    }

    fun setDetail(detail: FileDetailModel?) {
        this.detail = detail
    }

    fun getDetail(): FileDetailModel? = detail

    fun initDefaultIfMetaNotEnable() {
        val currentDetail = detail ?: return

        val recordWrapperModel = FileRecordWrapperModel()

        val sizeMetadataModel = MetadataModel().apply {
            key = "_size"
            name = "_size"
            type = ColumnType.NUMBER
        }
        recordWrapperModel.metadata.add(sizeMetadataModel)

        val modifierMetadataModel = MetadataModel().apply {
            key = "_file_modifier"
            name = "_file_modifier"
            type = ColumnType.TEXT
        }
        recordWrapperModel.metadata.add(modifierMetadataModel)

        val mTimeMetadataModel = MetadataModel().apply {
            key = "_file_mtime"
            name = "_file_mtime"
            type = ColumnType.DATE
        }
        recordWrapperModel.metadata.add(mTimeMetadataModel)

        val m: MutableMap<String, Any?> = HashMap()
        m["_size"] = currentDetail.getSize()
        m["_file_modifier"] = currentDetail.getLastModifierEmail()
        m["_file_mtime"] = currentDetail.getLastModified()
        recordWrapperModel.results.add(m)

        addRecordWrapperModel(recordWrapperModel)

        val wrapperModel = UserWrapperModel()
        wrapperModel.user_list = ArrayList()
        val lastModifier = UserModel().apply {
            name = currentDetail.getLastModifierName()
            setAvatarUrl(currentDetail.getLastModifierAvatar())
            email = currentDetail.getLastModifierEmail()
            setContactEmail(currentDetail.getLastModifierContactEmail())
        }
        wrapperModel.user_list?.add(lastModifier)
        setRelatedUserList(wrapperModel)

        SLogs.d(PhotoFragment.TAG, "initDefaultIfMetaNotEnable()", "detail = ${currentDetail.getName()}")
    }

    fun setRelatedUserWrapperModel(relatedUserWrapperModel: UserWrapperModel) {
        relatedUserList.clear()
        relatedUserWrapperModel.user_list?.let { relatedUserList.addAll(it) }
    }

    private fun setRelatedUserList(users: UserWrapperModel) {
        relatedUserList.clear()
        users.user_list?.let { relatedUserList.addAll(it) }
    }

    fun getRelatedUserList(): List<UserModel> = relatedUserList

    fun addRecordWrapperModel(recordWrapperModel: FileRecordWrapperModel) {
        recordResults.addAll(recordWrapperModel.results)
        val metadata = swapSizePosition(recordWrapperModel.metadata)
        recordMetaData.addAll(metadata)
    }

    private fun swapSizePosition(metadata: MutableList<MetadataModel>?): MutableList<MetadataModel> {
        if (metadata.isNullOrEmpty()) {
            return CollectionUtils.newArrayList()
        }

        var index = -1
        for (i in metadata.indices) {
            if (TextUtils.equals(metadata[i].key, "_size")) {
                index = i
                break
            }
        }

        if (index <= 0) {
            return metadata
        }

        val sizeModel = metadata[index]
        metadata.removeAt(index)
        metadata.add(0, sizeModel)
        return metadata
    }

    fun setTagWrapperModel(tagWrapperModel: FileTagWrapperModel) {
        if (CollectionUtils.isEmpty(tagWrapperModel.results)) {
            return
        }

        tagWrapperModel.results?.forEach { model ->
            val tagModel = SDocTagModel()
            tagModel.id = model._id
            tagModel.name = model._tag_name
            tagModel.color = model._tag_color
            val key = tagModel.id
            if (key != null) {
                tagsMap[key] = tagModel
            }
        }
    }

    fun getRecordResultList(): MutableList<Map<String, Any?>> = recordResults

    fun getRecordMetaDataList(): List<MetadataModel> {
        if (CollectionUtils.isEmpty(getRecordResultList())) {
            throw RuntimeException("please first call build()")
        }
        return recordMetaData
    }

    fun setRecordMetaDataList(list: List<MetadataModel>) {
        recordMetaData.clear()
        recordMetaData.addAll(list)
    }

    fun getTagMap(): Map<String, SDocTagModel> = tagsMap

    override fun describeContents(): Int = 0

    override fun writeToParcel(dest: Parcel, flags: Int) {
        dest.writeParcelable(detail, flags)
        dest.writeParcelable(metadataConfig, flags)
        dest.writeTypedList(relatedUserList)
        dest.writeTypedList(recordMetaData)
        dest.writeList(recordResults)
        dest.writeInt(tagsMap.size)
        for ((key, value) in tagsMap.entries) {
            dest.writeString(key)
            dest.writeParcelable(value, flags)
        }
    }

    private constructor(parcel: Parcel) : this() {
        detail = parcel.readParcelable(FileDetailModel::class.java.classLoader)
        metadataConfig = parcel.readParcelable(MetadataConfigModel::class.java.classLoader)
        relatedUserList = parcel.createTypedArrayList(UserModel.CREATOR) ?: mutableListOf()
        recordMetaData = parcel.createTypedArrayList(MetadataModel.CREATOR) ?: mutableListOf()
        val resultList = ArrayList<Map<String, Any?>>()
        parcel.readList(resultList, Map::class.java.classLoader)
        recordResults = resultList
        val mapSize = parcel.readInt()
        tagsMap = HashMap(mapSize)
        repeat(mapSize) {
            val key = parcel.readString()
            val value = parcel.readParcelable<SDocTagModel>(SDocTagModel::class.java.classLoader)
            if (key != null && value != null) {
                tagsMap[key] = value
            }
        }
    }

    companion object {
        @JvmField
        val CREATOR: Parcelable.Creator<FileProfileConfigModel> =
            object : Parcelable.Creator<FileProfileConfigModel> {
                override fun createFromParcel(source: Parcel): FileProfileConfigModel = FileProfileConfigModel(source)
                override fun newArray(size: Int): Array<FileProfileConfigModel?> = arrayOfNulls(size)
            }
    }
}
