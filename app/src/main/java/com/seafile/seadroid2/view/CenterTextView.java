package com.seafile.seadroid2.view;

import android.content.Context;
import android.graphics.Canvas;
import android.graphics.Paint;
import android.text.TextUtils;
import android.util.AttributeSet;

import androidx.appcompat.widget.AppCompatTextView;

public class CenterTextView extends AppCompatTextView {
    public CenterTextView(Context context) {
        super(context);
        init();
    }

    public CenterTextView(Context context, AttributeSet attrs) {
        super(context, attrs);
        init();
    }

    public CenterTextView(Context context, AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        init();
    }

    private void init() {
        // 强制单行
        setSingleLine(true);
        setMaxLines(1);
        setIncludeFontPadding(false); // 去掉默认字体内边距
        setEllipsize(TextUtils.TruncateAt.END);
    }

    @Override
    protected void onDraw(Canvas canvas) {
        // 字体公平垂直居中 + 水平居中
        final Paint paint = getPaint();
        final String text = getText().toString();

        // 水平测量
        float textWidth = paint.measureText(text);
        float x = (getWidth() - textWidth) / 2f;

        // 垂直测量
        Paint.FontMetrics fm = paint.getFontMetrics();
        float centerY = getHeight() / 2f;
        // (ascent + descent) / 2 代表文本真实中点偏移
        float baselineOffset = (fm.ascent + fm.descent) / 2f;
        float y = centerY - baselineOffset;

        // RTL 环境下自动翻转测量逻辑
        if (getLayoutDirection() == LAYOUT_DIRECTION_RTL) {
            x = (getWidth() + textWidth) / 2f - textWidth;
        }

        paint.setColor(getCurrentTextColor());
        canvas.drawText(text, x, y, paint);
    }
}
