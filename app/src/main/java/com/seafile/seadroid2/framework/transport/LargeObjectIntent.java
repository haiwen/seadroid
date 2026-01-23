package com.seafile.seadroid2.framework.transport;

import android.content.Intent;

public final class LargeObjectIntent {

    private static final String EXTRA_KEY = "_large_object_key";

    public static void put(Intent intent, String largetKey) {
        intent.putExtra(EXTRA_KEY, largetKey);
    }

    public static String get(Intent intent) {
        return intent.getStringExtra(EXTRA_KEY);
    }
}
