package com.seafile.seadroid2.framework.model.iconfont;

import java.util.List;
import com.google.gson.annotations.SerializedName;

public class IconFontModel {
    @SerializedName("css_prefix_text")
    public String css_prefix_text;
    @SerializedName("glyphs")
    public List<IconFontMapModel> glyphs;
}
