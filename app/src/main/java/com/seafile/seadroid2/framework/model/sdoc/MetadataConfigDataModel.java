package com.seafile.seadroid2.framework.model.sdoc;

import com.google.gson.annotations.SerializedName;

import java.util.List;

public class MetadataConfigDataModel {

    public String format;

    public String geo_format;
    public List<OptionTagModel> options;

    public boolean enable_precision;
    public int precision;
    // 小数点分隔符
    public String decimal;
    public String thousands;
    public String currency_symbol;
    public String currency_symbol_position;

    @SerializedName("max")
    public int rate_max_number;
    @SerializedName("color")
    public String rate_style_color;

    //
    public String display_column_key;
    public String link_id;
    public String other_table_id;
    public String table_id;

//    @SerializedName("type")
//    public String rate_style_type;

    public MetadataConfigDataModel() {
    }

    public String getFormat() {
        return format
                .replace("M/D/YYYY","M/d/yyyy")
                .replace("YYYY", "yyyy")
                .replace("DD", "dd");
    }
}
