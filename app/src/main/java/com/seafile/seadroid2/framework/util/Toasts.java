package com.seafile.seadroid2.framework.util;

import android.content.Context;
import android.text.TextUtils;
import android.widget.Toast;

import androidx.annotation.StringRes;

import com.blankj.utilcode.util.ThreadUtils;
import com.seafile.seadroid2.SeadroidApplication;

public class Toasts {
    public static void show(@StringRes final int resId) {
        show(SeadroidApplication.getAppContext(), resId, false);
    }

    public static void show(String text) {
        show(SeadroidApplication.getAppContext(), text, false);
    }

    public static void showShort(@StringRes final int resId) {
        show(SeadroidApplication.getAppContext(), resId, true);
    }

    public static void showShort(String text) {
        show(SeadroidApplication.getAppContext(), text, true);
    }

    private static void show(final Context context, @StringRes final int resId) {
        if (context == null) {
            return;
        }

        if (resId == 0) {
            return;
        }

        show(context, context.getString(resId), false);
    }

    private static void show(final Context context, @StringRes final int resId, boolean isShort) {
        if (context == null) {
            return;
        }

        if (resId == 0) {
            return;
        }

        show(context, context.getString(resId), isShort);
    }

    private static void show(final Context context, final CharSequence text, boolean isShort) {
        if (context == null) {
            return;
        }
        if (TextUtils.isEmpty(text)) {
            return;
        }

        ThreadUtils.runOnUiThread(new Runnable() {
            @Override
            public void run() {
                Toast.makeText(context, text, isShort ? Toast.LENGTH_LONG : Toast.LENGTH_SHORT).show();
            }
        });
    }
}
