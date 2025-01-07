package com.seafile.seadroid2.framework.data.model.sdoc;

import com.seafile.seadroid2.framework.data.model.user.UserWrapperModel;

public class FileProfileConfigModel {
    public UserWrapperModel users;
    public FileDetailModel detail;
    public MetadataConfigModel metadataConfigModel;

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
