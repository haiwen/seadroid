package com.seafile.seadroid2.view;

import android.content.Context;
import android.text.TextUtils;
import android.util.AttributeSet;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.widget.AppCompatTextView;

import com.seafile.seadroid2.framework.util.IconFontUtils;

public class IconFontTextView extends AppCompatTextView {

    public IconFontTextView(@NonNull Context context) {
        super(context);
        this.setTypeface(IconFontUtils.getIconFontTypeFace());
    }

    public IconFontTextView(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
        this.setTypeface(IconFontUtils.getIconFontTypeFace());
    }

    public IconFontTextView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        this.setTypeface(IconFontUtils.getIconFontTypeFace());
    }

    @Override
    public void setText(CharSequence text, BufferType type) {
        if (TextUtils.isEmpty(text)) {
            super.setText(IconFontUtils.getIconFontDefaultMap(), type);
        } else {
            super.setText(IconFontUtils.getIconFontMap().get(text.toString()), type);
        }
    }
}
