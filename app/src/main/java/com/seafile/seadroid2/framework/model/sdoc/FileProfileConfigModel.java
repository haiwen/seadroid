package com.seafile.seadroid2.framework.model.sdoc;

import com.seafile.seadroid2.framework.model.user.UserWrapperModel;

import java.util.List;

public class FileProfileConfigModel {
    public UserWrapperModel users;
    public FileDetailModel detail;
    public MetadataConfigModel metadataConfigModel;

    public FileRecordWrapperModel recordWrapperModel;
    public FileTagWrapperModel tagWrapperModel;

    public void setTagWrapperModel(FileTagWrapperModel tagWrapperModel) {
        this.tagWrapperModel = tagWrapperModel;
    }

    public void setRecordWrapperModel(FileRecordWrapperModel recordWrapperModel) {
        this.recordWrapperModel = recordWrapperModel;
    }

    public List<SDocTagModel> tags;


    public MetadataConfigModel getMetadataConfigModel() {
        return metadataConfigModel;
    }

    public void setMetadataConfigModel(MetadataConfigModel metadataConfigModel) {
        this.metadataConfigModel = metadataConfigModel;
    }

    public UserWrapperModel getUsers() {
        return users;
    }

    public void setUsers(UserWrapperModel users) {
        this.users = users;
    }

    public FileDetailModel getDetail() {
        return detail;
    }

    public void setDetail(FileDetailModel detail) {
        this.detail = detail;
    }
}
