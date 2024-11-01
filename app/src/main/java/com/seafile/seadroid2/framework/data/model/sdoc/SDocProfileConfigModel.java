package com.seafile.seadroid2.framework.data.model.sdoc;

import com.seafile.seadroid2.framework.data.model.user.UserWrapperModel;

public class SDocProfileConfigModel {
    public UserWrapperModel users;
    public MetadataConfigModel metadata;
    public SDocDetailModel detail;

    public UserWrapperModel getUsers() {
        return users;
    }

    public void setUsers(UserWrapperModel users) {
        this.users = users;
    }

    public MetadataConfigModel getMetadata() {
        return metadata;
    }

    public void setMetadata(MetadataConfigModel metadata) {
        this.metadata = metadata;
    }

    public SDocDetailModel getDetail() {
        return detail;
    }

    public void setDetail(SDocDetailModel detail) {
        this.detail = detail;
    }
}
