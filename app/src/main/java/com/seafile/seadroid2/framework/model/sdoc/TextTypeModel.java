package com.seafile.seadroid2.framework.model.sdoc;

import com.blankj.utilcode.util.CollectionUtils;
import com.google.gson.annotations.SerializedName;

import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;
import java.util.List;

public class TextTypeModel {
    /**
     * undo
     * redo
     * paragraph,title,subtitle,h1,h2,h3,h4,h5,h6
     * unordered-list
     * ordered-list
     * checkbox
     * local-image: insert image into sdoc
     */
    @SerializedName("type")
    public String type;

    // v4.0.5
    @SerializedName("images")
    public List<UploadSdocImageModel> images;

    public TextTypeModel(String type) {
        if (StringUtils.equals("local_image", type)) {
            type = "local-image";
        }
        this.type = type;
    }

    public TextTypeModel(String type, List<UploadSdocImageResultModel> images) {
        if (StringUtils.equals("local_image", type)) {
            type = "local-image";
        }
        this.type = type;

        List<UploadSdocImageModel> list = new ArrayList<>();
        if (CollectionUtils.isNotEmpty(images)) {
            for (UploadSdocImageResultModel image : images) {
                if (CollectionUtils.isNotEmpty(image.relative_path)) {
                    for (String url : image.relative_path) {
                        UploadSdocImageModel imageModel = new UploadSdocImageModel();
                        imageModel.src = url;
                        list.add(imageModel);
                    }
                }

            }
        }
        this.images = list;
    }
}
