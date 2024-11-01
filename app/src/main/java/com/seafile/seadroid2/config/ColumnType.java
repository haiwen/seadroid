package com.seafile.seadroid2.config;

import com.blankj.utilcode.util.CollectionUtils;

import java.util.List;

public class ColumnType {
    public static final String INIT_FIELD = "0000";



    public static final String TEXT = "text";
    public static final String COLLABORATOR = "collaborator";
    public static final String DATE = "date";
    public static final String LONG_TEXT = "long-text";
    public static final String NUMBER = "number";
    public static final String DURATION = "duration";
    public static final String SINGLE_SELECT = "single-select";
    public static final String MULTIPLE_SELECT = "multiple-select";
    public static final String IMAGE = "image";
    public static final String FILE = "file";
    public static final String EMAIL = "email";
    public static final String URL = "url";
    public static final String CHECKBOX = "checkbox";
    public static final String RATE = "rate";

    //Advanced
    public static final String GEOLOCATION = "geolocation";
    public static final String LINK = "link";
    public static final String DIGITAL_SIGN = "digital-sign";
    public static final String FORMULA = "formula";
    public static final String LINK_FORMULA = "link-formula";
    public static final String AUTO_NUMBER = "auto-number";
    public static final String CTIME = "ctime";
    public static final String MTIME = "mtime";
    public static final String BUTTON = "button";
    public static final String CREATOR = "creator";
    public static final String LAST_MODIFIER = "last-modifier";


    public final static List<String> ENABLE_TYPE_LIST = CollectionUtils.newArrayList(
            TEXT,
            COLLABORATOR,
            DATE,
            LONG_TEXT,
            NUMBER,
            DURATION,
            SINGLE_SELECT,
            MULTIPLE_SELECT,
            IMAGE,
            FILE,
            EMAIL,
            URL,
            CHECKBOX,
            RATE,
            GEOLOCATION,
            LINK,
            DIGITAL_SIGN);
}
