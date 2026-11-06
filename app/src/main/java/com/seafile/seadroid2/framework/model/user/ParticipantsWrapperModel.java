package com.seafile.seadroid2.framework.model.user;

import java.util.List;
import com.google.gson.annotations.SerializedName;

public class ParticipantsWrapperModel {
    @SerializedName("participant_list")
    public List<UserModel> participant_list;
}
