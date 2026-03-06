package com.seafile.seadroid2.markdown;

import android.text.TextPaint;
import android.text.style.CharacterStyle;
import android.text.style.UpdateAppearance;

public class RemoveUnderlineSpan extends CharacterStyle implements UpdateAppearance {
    @Override
    public void updateDrawState(TextPaint tp) {
        tp.setUnderlineText(false);
    }
}
