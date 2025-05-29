package com.seafile.seadroid2.framework.glide;

import androidx.annotation.NonNull;

import com.bumptech.glide.load.model.GlideUrl;
import com.bumptech.glide.load.model.ModelLoader;
import com.bumptech.glide.load.model.ModelLoaderFactory;
import com.bumptech.glide.load.model.MultiModelLoaderFactory;

import java.io.InputStream;

public class GlideImageModelLoaderFactory implements ModelLoaderFactory<GlideImage, InputStream> {

    @NonNull
    @Override
    public ModelLoader<GlideImage, InputStream> build(@NonNull MultiModelLoaderFactory multiFactory) {
        ModelLoader<GlideUrl, InputStream> glideUrlLoader = multiFactory.build(GlideUrl.class, InputStream.class);

        return new GlideImageModelLoader(glideUrlLoader);
    }

    @Override
    public void teardown() {
        // no-op
    }
}