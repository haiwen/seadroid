package com.seafile.seadroid2.config;

import com.bumptech.glide.load.model.GlideUrl;
import com.bumptech.glide.load.model.Headers;
import com.seafile.seadroid2.framework.util.SLogs;

import java.net.URL;

public class OriGlideUrl extends GlideUrl {
    private String oriCacheKey;

    public OriGlideUrl(URL url) {
        super(url);
    }

    public OriGlideUrl(String url) {
        super(url);
    }

    public OriGlideUrl(String url, String oriCacheKey) {
        super(url);
        this.oriCacheKey = oriCacheKey;
    }


    public OriGlideUrl(URL url, Headers headers) {
        super(url, headers);
    }

    public OriGlideUrl(String url, Headers headers) {
        super(url, headers);
    }

    @Override
    public String getCacheKey() {
        return oriCacheKey;
    }
}
