package com.seafile.seadroid2.config;

import com.bumptech.glide.request.RequestOptions;
import com.bumptech.glide.signature.ObjectKey;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.ui.WidgetUtils;

public class GlideLoadConfig {

//    public static GlideUrl getGlideUrl(String url) {
//
//        Account account = SupportAccountManager.getInstance().getCurrentAccount();
//        if (account == null) {
//            return new GlideUrl(url, new LazyHeaders.Builder().build());
//        }
//
//        String token = account.token;
//
//        return new GlideUrl(url, new LazyHeaders.Builder()
//                .addHeader("Authorization", "Token " + token)
//                .build());
//    }
//
//    public static GlideUrl getGlideUrl(String url, String token) {
//        return new GlideUrl(url, new LazyHeaders.Builder()
//                .addHeader("Authorization", "Token " + token)
//                .build());
//    }

    public static RequestOptions getAvatarOptions() {
        return new RequestOptions()
                .fallback(R.drawable.default_avatar)
                .placeholder(R.drawable.default_avatar)
                .override(WidgetUtils.getThumbnailWidth(), WidgetUtils.getThumbnailWidth());
    }

    public static RequestOptions getOptions() {
        return new RequestOptions()
                .fallback(R.drawable.file_image)
                .placeholder(R.drawable.file_image);
    }

    public static RequestOptions getOptions(String key) {
        return new RequestOptions()
                .fallback(R.drawable.file_image)
                .placeholder(R.drawable.file_image)
                .signature(new ObjectKey(key))
                .override(WidgetUtils.getThumbnailWidth(), WidgetUtils.getThumbnailWidth());
    }
}
