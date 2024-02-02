package com.seafile.seadroid2.data.model.dirents;

import com.seafile.seadroid2.data.model.ResultModel;

public class DeleteDirentModel extends ResultModel {
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
