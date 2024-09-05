package com.seafile.seadroid2.view;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.TextView;

import com.seafile.seadroid2.R;

public class TipsViews {
    public static TextView getTipTextView(Context context) {
        return (TextView) LayoutInflater.from(context).inflate(R.layout.view_tip_textview, null);
    }
}
