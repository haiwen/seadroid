package com.seafile.seadroid2.framework.model.sdoc;

import android.os.Parcel;
import android.os.Parcelable;
import android.text.TextUtils;

import androidx.annotation.Nullable;

import com.blankj.utilcode.util.CollectionUtils;
import com.seafile.seadroid2.config.ColumnType;
import com.seafile.seadroid2.framework.model.profile.DetailsSettingsKeyModel;
import com.seafile.seadroid2.framework.model.user.UserModel;
import com.seafile.seadroid2.framework.model.user.UserWrapperModel;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.ui.media.image.PhotoFragment;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Consumer;

public class FileProfileConfigModel implements Parcelable {
    private String repoId;
    private FileDetailModel detail;
    private MetadataConfigModel metadataConfig;

    private List<UserModel> relatedUserList = new ArrayList<>();
    private final LinkedHashMap<String, MetadataModel> recordMetaDataMap = new LinkedHashMap<>();
    private final LinkedHashMap<String, Object> recordResultMap = new LinkedHashMap<>();
    private Map<String, SDocTagModel> tagsMap = new HashMap<>();

    public void setRepoId(String repoId) {
        this.repoId = repoId;
    }

    public String getRepoId() {
        return repoId;
    }

    public void setMetadataConfigModel(MetadataConfigModel metadataConfigModel) {
        this.metadataConfig = metadataConfigModel;
    }

    public HashMap<String, Boolean> getDetailsSettingsMap() {
        if (metadataConfig == null) {
            throw new RuntimeException("please first call setMetadataConfigModel()");
        }

        HashMap<String, Boolean> map = new HashMap<>();
        // These three fields are displayed by default
        map.put("_size", true);
        map.put("_file_modifier", true);
        map.put("_file_mtime", true);

        List<DetailsSettingsKeyModel> list = metadataConfig.getDetailsSettingsList();
        if (CollectionUtils.isEmpty(list)) {
            return map;
        }

        for (DetailsSettingsKeyModel model : list) {
            map.put(model.key, model.shown);
        }

        return map;
    }

    public boolean isMetadataEnabled() {
        if (metadataConfig == null) {
            throw new RuntimeException("please first call setMetadataConfigModel()");
        }
        return metadataConfig.enabled;
    }

    public boolean isTagsEnabled() {
        if (metadataConfig == null) {
            throw new RuntimeException("please first call setMetadataConfigModel()");
        }
        return metadataConfig.tags_enabled;
    }

    public void setFileDetail(FileDetailModel detail) {
        this.detail = detail;
    }

    public FileDetailModel getFileDetail() {
        return detail;
    }

    public void initDefaultIfMetaNotEnable() {
        if (detail == null) {
            return;
        }

        FileRecordWrapperModel recordWrapperModel = new FileRecordWrapperModel();
        //size
        MetadataModel sizeMetadataModel = new MetadataModel();
        sizeMetadataModel.key = "_size";
        sizeMetadataModel.name = "_size";
        sizeMetadataModel.type = ColumnType.NUMBER;
        recordWrapperModel.metadata.add(sizeMetadataModel);

        //modifier
        MetadataModel modifierMetadataModel = new MetadataModel();
        modifierMetadataModel.key = "_file_modifier";
        modifierMetadataModel.name = "_file_modifier";
        modifierMetadataModel.type = ColumnType.TEXT;
        recordWrapperModel.metadata.add(modifierMetadataModel);

        //mtime
        MetadataModel mTimeMetadataModel = new MetadataModel();
        mTimeMetadataModel.key = "_file_mtime";
        mTimeMetadataModel.name = "_file_mtime";
        mTimeMetadataModel.type = ColumnType.DATE;
        recordWrapperModel.metadata.add(mTimeMetadataModel);

        Map<String, Object> m = new HashMap<>();
        m.put("_size", getFileDetail().getSize());
        m.put("_file_modifier", getFileDetail().getLastModifierEmail());
        m.put("_file_mtime", getFileDetail().getLastModified());
        recordWrapperModel.results.add(m);

        setRecordWrapperModel(recordWrapperModel);

        //add a default user with last modifier user info
        UserWrapperModel wrapperModel = new UserWrapperModel();
        wrapperModel.user_list = new ArrayList<>();
        UserModel r = new UserModel();
        r.setName(getFileDetail().getLastModifierName());
        r.setAvatarUrl(getFileDetail().getLastModifierAvatar());
        r.setEmail(getFileDetail().getLastModifierEmail());
        r.setContactEmail(getFileDetail().getLastModifierContactEmail());
        wrapperModel.user_list.add(r);
        setRelatedUserList(wrapperModel);

        SLogs.d(PhotoFragment.TAG, "initDefaultIfMetaNotEnable()", "detail = " + detail.getName());
    }

    public void setRelatedUserWrapperModel(UserWrapperModel relatedUserWrapperModel) {
        this.relatedUserList.clear();
        this.relatedUserList.addAll(relatedUserWrapperModel.user_list);
    }

    private void setRelatedUserList(UserWrapperModel users) {
        this.relatedUserList.clear();
        this.relatedUserList.addAll(users.user_list);
    }

    public List<UserModel> getRelatedUserList() {
        return relatedUserList;
    }

    public void setRecordWrapperModel(FileRecordWrapperModel recordWrapperModel) {
        if (CollectionUtils.isNotEmpty(recordWrapperModel.results)) {
            this.recordResultMap.putAll(recordWrapperModel.results.get(0));
        }

        recordMetaDataMap.clear();

        List<MetadataModel> metadataList = swapSizePosition(recordWrapperModel.metadata);
        for (MetadataModel metadataModel : metadataList) {
            Object value = recordResultMap.get(metadataModel.name);
            if ("_file_modifier".equals(metadataModel.key)) {
                metadataModel.type = "collaborator";
                metadataModel.value = CollectionUtils.newArrayList(value);
            } else {
                metadataModel.value = value;
            }

            recordMetaDataMap.put(metadataModel.key, metadataModel);
        }
    }

    private List<MetadataModel> swapSizePosition(List<MetadataModel> metadata) {
        if (CollectionUtils.isEmpty(metadata)) {
            return CollectionUtils.newArrayList();
        }

        int index = -1;
        for (int i = 0; i < metadata.size(); i++) {
            if (TextUtils.equals(metadata.get(i).key, "_size")) {
                index = i;
                break;
            }
        }

        if (index == -1) {
            return metadata;
        }

        if (index == 0) {
            return metadata;
        }

        MetadataModel sizeModel = metadata.get(index);
        metadata.remove(index);
        metadata.add(0, sizeModel);
        return metadata;
    }

    public void setTagWrapperModel(FileTagWrapperModel tagWrapperModel) {
        if (CollectionUtils.isEmpty(tagWrapperModel.results)) {
            return;
        }

        tagWrapperModel.results.forEach(new Consumer<FileTagResultModel>() {
            @Override
            public void accept(FileTagResultModel model) {
                SDocTagModel tagModel = new SDocTagModel();
                tagModel.id = model._id;
                tagModel.name = model._tag_name;
                tagModel.color = model._tag_color;
                tagsMap.put(tagModel.id, tagModel);
            }
        });
    }

//    public List<Map<String, Object>> getRecordResultList() {
//        return recordResultList;
//    }
//    public List<MetadataModel> getRecordMetaDataList() {
//        if (CollectionUtils.isEmpty(getRecordResultList())) {
//            throw new RuntimeException("please first set data");
//        }
//        return recordMetaDataList;
//    }

    public LinkedHashMap<String, MetadataModel> getRecordMetaDataMap() {
        return recordMetaDataMap;
    }

    public LinkedHashMap<String, Object> getRecordResultMap() {
        return recordResultMap;
    }

//    public void setRecordMetaDataList(List<MetadataModel> list) {
//        this.recordMetaDataList.clear();
//        this.recordMetaDataList.addAll(list);
//    }

    public Map<String, SDocTagModel> getTagMap() {
        return tagsMap;
    }

    public List<OptionTagModel> getTagList() {
        List<OptionTagModel> ops = new ArrayList<>();

        for (SDocTagModel value : tagsMap.values()) {
            OptionTagModel optionTagModel = new OptionTagModel();
            optionTagModel.id = value.id;
            optionTagModel.name = value.name;
            optionTagModel.color = value.color;
            ops.add(optionTagModel);
        }
        return ops;
    }

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(Parcel dest, int flags) {
        dest.writeParcelable(this.detail, flags);
        dest.writeParcelable(this.metadataConfig, flags);
        dest.writeTypedList(this.relatedUserList);
        dest.writeInt(this.tagsMap.size());
        for (Map.Entry<String, SDocTagModel> entry : this.tagsMap.entrySet()) {
            dest.writeString(entry.getKey());
            dest.writeParcelable(entry.getValue(), flags);
        }
    }

    public FileProfileConfigModel() {
    }

    protected FileProfileConfigModel(Parcel in) {
        this.detail = in.readParcelable(FileDetailModel.class.getClassLoader());
        this.metadataConfig = in.readParcelable(MetadataConfigModel.class.getClassLoader());
        this.relatedUserList = in.createTypedArrayList(UserModel.CREATOR);
        int tagsMapSize = in.readInt();
        this.tagsMap = new HashMap<String, SDocTagModel>(tagsMapSize);
        for (int i = 0; i < tagsMapSize; i++) {
            String key = in.readString();
            SDocTagModel value = in.readParcelable(SDocTagModel.class.getClassLoader());
            this.tagsMap.put(key, value);
        }
    }

    public static final Parcelable.Creator<FileProfileConfigModel> CREATOR = new Parcelable.Creator<FileProfileConfigModel>() {
        @Override
        public FileProfileConfigModel createFromParcel(Parcel source) {
            return new FileProfileConfigModel(source);
        }

        @Override
        public FileProfileConfigModel[] newArray(int size) {
            return new FileProfileConfigModel[size];
        }
    };
}

