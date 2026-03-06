package com.seafile.seadroid2.markdown;

import android.text.TextUtils;
import android.view.View;

import androidx.annotation.NonNull;

import com.seafile.seadroid2.framework.util.Toasts;

import java.util.Locale;

import io.noties.markwon.LinkResolver;
import io.noties.markwon.core.MarkwonTheme;
import io.noties.markwon.core.spans.LinkSpan;

public class LinkClickSelfSpan extends LinkSpan {
    public LinkClickSelfSpan(
            @NonNull MarkwonTheme theme,
            @NonNull String link,
            @NonNull LinkResolver resolver) {
        super(theme, link, resolver);
    }

    @Override
    public void onClick(View widget) {
        String link = getLink();
        if (TextUtils.isEmpty(link) || !link.toLowerCase(Locale.getDefault()).startsWith("http")) {
            Toasts.show(com.seafile.seadroid2.R.string.tip_press_again_to_exit);
            return;
        }

//        WebViewActivity.startThis(widget.getContext(), getLink());
    }
}
