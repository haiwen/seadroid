package com.seafile.seadroid2.framework.model.transfer;

import com.google.gson.annotations.SerializedName;
import com.seafile.seadroid2.framework.model.ResultModel;

public class UploadedBytesModel extends ResultModel {
    /**
     * =0: the file has not been uploaded before
     * </br>
     * >0: uploaded bytes
     * */
    @SerializedName("uploadedBytes")
    public long uploadedBytes;
}
