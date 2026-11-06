package com.seafile.seadroid2.framework.model.sdoc;


import java.util.List;
import com.google.gson.annotations.SerializedName;

public class MetadataConfigDataModel {

    @SerializedName("format")
    public String format;

    @SerializedName("geo_format")
    public String geo_format;
    @SerializedName("options")
    public List<OptionTagModel> options;

    @SerializedName("enable_precision")
    public boolean enable_precision;
    @SerializedName("precision")
    public int precision;
    // 小数点分隔符
    @SerializedName("decimal")
    public String decimal;
    @SerializedName("thousands")
    public String thousands;
    @SerializedName("currency_symbol")
    public String currency_symbol;
    @SerializedName("currency_symbol_position")
    public String currency_symbol_position;

    @SerializedName("max")
    public int rate_max_number;
    @SerializedName("color")
    public String rate_style_color;

    //
    @SerializedName("display_column_key")
    public String display_column_key;
    @SerializedName("link_id")
    public String link_id;
    @SerializedName("other_table_id")
    public String other_table_id;
    @SerializedName("table_id")
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
