package com.seafile.seadroid2.preferences;

import android.content.Context;
import android.text.TextUtils;

public class PreferenceManagerCompat {
    public static String getDefaultSharedPreferencesName(Context context) {
        return context.getPackageName() + "_preferences";
    }

    public static String getDefaultSharedPreferencesName(Context context, String nameSuffix) {
        if (TextUtils.isEmpty(nameSuffix)) {
            return getDefaultSharedPreferencesName(context);
        }

        return context.getPackageName() + "_preferences_" + nameSuffix;
    }

    public static int getDefaultSharedPreferencesMode() {
        return Context.MODE_PRIVATE;
    }
}
