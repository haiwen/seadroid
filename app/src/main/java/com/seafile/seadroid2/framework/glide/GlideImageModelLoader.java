package com.seafile.seadroid2.framework.glide;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.bumptech.glide.load.Options;
import com.bumptech.glide.load.model.GlideUrl;
import com.bumptech.glide.load.model.LazyHeaders;
import com.bumptech.glide.load.model.ModelLoader;

import java.io.InputStream;

public class GlideImageModelLoader implements ModelLoader<GlideImage, InputStream> {
    private final ModelLoader<GlideUrl, InputStream> delegate;

    public GlideImageModelLoader(ModelLoader<GlideUrl, InputStream> delegate) {
        this.delegate = delegate;
    }

    @Nullable
    @Override
    public LoadData<InputStream> buildLoadData(
            @NonNull GlideImage model,
            int width,
            int height,
            @NonNull Options options
    ) {
        GlideUrl glideUrl = new GlideUrl(
                model.url,
                new LazyHeaders.Builder()
                        .addHeader("Authorization", "Token " + model.token)
                        .build()
        );

        return delegate.buildLoadData(glideUrl, width, height, options);
    }

    @Override
    public boolean handles(@NonNull GlideImage model) {
        return true;
    }
}
