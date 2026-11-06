package com.seafile.seadroid2.framework.model.user;

import java.util.List;
import com.google.gson.annotations.SerializedName;

public class UserWrapperModel {
    @SerializedName("user_list")
    public List<UserModel> user_list;
}
