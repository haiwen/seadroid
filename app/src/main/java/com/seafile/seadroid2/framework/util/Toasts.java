package com.seafile.seadroid2.framework.util;

import android.content.Context;
import android.os.Handler;
import android.os.Looper;
import android.text.TextUtils;
import android.widget.Toast;

import androidx.annotation.StringRes;

import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.annotation.Todo;
import com.seafile.seadroid2.annotation.Unstable;

public class Toasts {

    public static void show(@StringRes final int resId) {
        show(SeadroidApplication.getAppContext(), resId);
    }

    public static void show(String text) {
        show(SeadroidApplication.getAppContext(), text);
    }

    private static void show(final Context context, @StringRes final int resId) {
        if (context == null) {
            return;
        }

        if (resId == 0) {
            return;
        }

        show(context, context.getString(resId));
    }

    private static void show(final Context context, final CharSequence text) {
        if (context == null) {
            return;
        }
        if (TextUtils.isEmpty(text)) {
            return;
        }

        if (Looper.getMainLooper().getThread() == Thread.currentThread()) {
            Toast.makeText(context, text, Toast.LENGTH_LONG).show();
        } else {
            new Handler(Looper.getMainLooper()).post(() -> {
                Toast.makeText(context, text, Toast.LENGTH_LONG).show();
            });
        }
    }
}
