package com.seafile.seadroid2.view;

import android.content.Context;
import android.graphics.Canvas;
import android.text.TextPaint;
import android.text.TextUtils;
import android.util.AttributeSet;

import androidx.appcompat.widget.AppCompatTextView;

/**
 * Don't use locate attributes such as gravity
 * */
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
        // Force single line
        setSingleLine(true);
        setMaxLines(1);
        setIncludeFontPadding(false); // Remove default font padding
        setEllipsize(TextUtils.TruncateAt.END);
    }

    @Override
    protected void onDraw(Canvas canvas) {
        // True vertical centering for text
        final TextPaint paint = getPaint();
        String text = getText().toString();

        // Use default x coordinate (considering padding)
        float x = getCompoundPaddingLeft();

        // Calculate available width
        float availableWidth = getWidth() - getCompoundPaddingLeft() - getCompoundPaddingRight();

        // Handle ellipsize
        if (!TextUtils.isEmpty(text)) {
            float textWidth = paint.measureText(text);
            if (textWidth > availableWidth) {
                // Text exceeds available width, needs truncation
                text = TextUtils.ellipsize(text, paint, availableWidth, TextUtils.TruncateAt.END).toString();
            }
        }

        // Vertical measurement
        TextPaint.FontMetrics fm = paint.getFontMetrics();
        float centerY = getHeight() / 2f;
        // (ascent + descent) / 2 represents the true center offset of the text
        float baselineOffset = (fm.ascent + fm.descent) / 2f;
        float y = centerY - baselineOffset;

        paint.setColor(getCurrentTextColor());
        canvas.drawText(text, x, y, paint);
    }
}
