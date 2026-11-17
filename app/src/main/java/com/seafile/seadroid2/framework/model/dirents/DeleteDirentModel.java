package com.seafile.seadroid2.framework.model.dirents;

import com.seafile.seadroid2.framework.model.ResultModel;
import com.google.gson.annotations.SerializedName;

public class DeleteDirentModel extends ResultModel {
    @SerializedName("commit_id")
    public String commit_id;

    @Override
    public String toString() {
        return "DeleteDirentModel{" +
                "commit_id='" + commit_id + '\'' +
                ", success=" + success +
                ", error_msg='" + error_msg + '\'' +
                '}';
    }
}
