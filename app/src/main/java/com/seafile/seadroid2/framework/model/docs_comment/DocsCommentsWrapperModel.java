package com.seafile.seadroid2.framework.model.docs_comment;

import java.util.List;
import com.google.gson.annotations.SerializedName;

public class DocsCommentsWrapperModel {
    @SerializedName("comments")
    public List<DocsCommentModel> comments;
}
