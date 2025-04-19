package com.seafile.seadroid2.view.webview.strategy;

import android.app.Activity;
import android.content.Context;
import android.content.MutableContextWrapper;

import androidx.appcompat.app.AppCompatActivity;

import com.seafile.seadroid2.annotation.Todo;
import com.seafile.seadroid2.annotation.Unstable;
import com.seafile.seadroid2.view.webview.IWebViewActionStrategy;


@Unstable
@Todo("uncompleted feature")
public class PageStatusColorSetStrategy implements IWebViewActionStrategy {
    private Context context;

    public PageStatusColorSetStrategy(Context context) {
        this.context = context;
    }

    @Override
    public String route(String paramsStr) {
        if (context != null) {
            MutableContextWrapper c = (MutableContextWrapper) context;
            if (c.getBaseContext() instanceof Activity) {
                AppCompatActivity a = (AppCompatActivity) c.getBaseContext();
                //todo
            } else {
                throw new IllegalArgumentException("Context is not activity");
            }
        }

        return null;
    }
}
