package com.seafile.seadroid2.framework.util;

import com.blankj.utilcode.util.TimeUtils;
import com.seafile.seadroid2.config.DateFormatType;

import java.util.Date;

public class Times {
    public static long convertMtime2Long(String mtime) {
        Date date = TimeUtils.string2Date(mtime, DateFormatType.DATE_XXX);
        return TimeUtils.date2Millis(date);
    }
}
