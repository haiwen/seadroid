package com.seafile.seadroid2.framework.model.sdoc;

import android.text.TextUtils;

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

/**
 * There is no serialization.
 * If you need to pass or receive this type of parameter, use the TransportHolder
 */
public class FileProfileConfigModel {
    public final String TAG = "FileProfileConfigModel";
    private String repoId;
    private FileDetailModel detail;
    private MetadataConfigModel metadataConfig;
    private final List<UserModel> relatedUserList = new ArrayList<>();
    private final LinkedHashMap<String, MetadataModel> recordMetaDataMap = new LinkedHashMap<>();
    private final LinkedHashMap<String, Object> recordResultMap = new LinkedHashMap<>();
    private final Map<String, OptionTagModel> tagsMap = new HashMap<>();

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

        // default fields
        map.put("_size", true);
        map.put("_file_modifier", true);
        map.put("_file_mtime", true);

        if (isMetadataEnabled()) {
            map.put("_location", true);
            map.put("_description", true);
            map.put("_tags", true);
        }

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
        m.put("_size", getFileDetail().size);
        m.put("_file_modifier", getFileDetail().lastModifierEmail);
        m.put("_file_mtime", getFileDetail().lastModified);
        recordWrapperModel.results.add(m);

        setRecordWrapperModel(recordWrapperModel);

        //add a default user with last modifier user info
        UserWrapperModel wrapperModel = new UserWrapperModel();
        wrapperModel.user_list = new ArrayList<>();
        UserModel r = new UserModel();
        r.setName(getFileDetail().lastModifierName);
        r.setAvatarUrl(getFileDetail().lastModifierAvatar);
        r.setEmail(getFileDetail().lastModifierEmail);
        r.setContactEmail(getFileDetail().lastModifierContactEmail);
        wrapperModel.user_list.add(r);
        setRelatedUserList(wrapperModel);

        SLogs.d(TAG, "initDefaultIfMetaNotEnable()", "detail = " + detail.name);
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

    public LinkedHashMap<String, MetadataModel> getRecordMetaDataMap() {
        return recordMetaDataMap;
    }

    public LinkedHashMap<String, Object> getRecordResultMap() {
        return recordResultMap;
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

        for (FileTagResultModel model : tagWrapperModel.results) {
            OptionTagModel tagModel = new OptionTagModel();
            tagModel.type = "tag";
            tagModel.id = model._id;
            tagModel.name = model._tag_name;
            tagModel.color = model._tag_color;
            tagsMap.put(tagModel.id, tagModel);
        }
    }


    public Map<String, OptionTagModel> getTagMap() {
        return tagsMap;
    }

    public List<OptionTagModel> getTagList() {
        return new ArrayList<>(tagsMap.values());
    }
}

